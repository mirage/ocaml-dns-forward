(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let src =
  let src = Logs.Src.create "Dns_forward" ~doc:"DNS resolution" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

open Lwt.Infix

let is_in_domain name domain =
  let name' = List.length name and domain' = List.length domain in
  name' >= domain' && begin
    let to_remove = name' - domain' in
    let rec trim n xs = match n, xs with
      | 0, _ -> xs
      | _, [] -> invalid_arg "trim"
      | n, _ :: xs -> trim (n - 1) xs in
    let trimmed_name = trim to_remove name in
    trimmed_name = domain
  end

module IntSet = Set.Make(struct type t = int let compare (a: int) (b: int) = compare a b end)

let choose_servers config request =
  let open Dns.Packet in
  let open Dns_forward_config in
  (* Match the name in the query against the configuration *)
  begin match request with
  | { questions = [ { q_name; _ } ]; _ } ->
    let labels = Dns.Name.to_string_list q_name in
    let matching_servers = List.filter (fun server ->
      Domain.Set.fold (fun zone acc -> acc || (is_in_domain labels zone)) server.Server.zones false
    ) config in
    let all = match matching_servers with
      | _ :: _ ->
        (* If any of the configured domains match, send to these servers *)
        matching_servers
      | [] ->
        (* Otherwise send to all servers *)
        config in
    (* Now we order by the order field *)
    let orders = List.fold_left (fun set server -> IntSet.add server.Server.order set) IntSet.empty all in
    List.map
      (fun order ->
        List.filter (fun server -> server.Server.order = order) all
      ) (IntSet.elements orders)
    |> List.concat
  | _ -> []
  end

let or_fail_msg m = m >>= function
  | Result.Error `Eof -> Lwt.fail End_of_file
  | Result.Error (`Msg m) -> Lwt.fail (Failure m)
  | Result.Ok x -> Lwt.return x

module type S = Dns_forward_s.RESOLVER

module Make(Client: Dns_forward_s.RPC_CLIENT)(Time: V1_LWT.TIME) = struct

  module Cache = Dns_forward_cache.Make(Time)

  type address = Dns_forward_config.Address.t
  type message_cb = ?src:address -> ?dst:address -> buf:Cstruct.t -> unit -> unit Lwt.t

  type t = {
    connections: (Dns_forward_config.Server.t * Client.t) list;
    local_names_cb: (Dns.Packet.question -> Dns.Packet.rr list option Lwt.t);
    cache: Cache.t;
  }

  let create ?(local_names_cb=fun _ -> Lwt.return_none) ?message_cb config =
    Lwt_list.map_s (fun server ->
      or_fail_msg @@ Client.connect ?message_cb server.Dns_forward_config.Server.address
      >>= fun client ->
      Lwt.return (server, client)
    ) (Dns_forward_config.Server.Set.elements config.Dns_forward_config.servers)
    >>= fun connections ->
    let cache = Cache.make () in
    Lwt.return { connections; local_names_cb; cache }

  let destroy t =
    Cache.destroy t.cache;
    Lwt_list.iter_s (fun (_, c) -> Client.disconnect c) t.connections

  let answer buffer t =
    let len = Cstruct.len buffer in
    let buf = Dns.Buf.of_cstruct buffer in
    let open Dns.Packet in
    match Dns.Protocol.Server.parse (Dns.Buf.sub buf 0 len) with
    | Some ({ questions = [ question ]; _ } as request) ->
      (* Given a set of answers (resource records), synthesize an answer to the
         current question. *)
      let marshal_reply answers =
        let id = request.id in
        let detail = { request.detail with Dns.Packet.qr = Dns.Packet.Response; ra = true } in
        let questions = request.questions in
        let authorities = [] and additionals = [] in
        let pkt = { id; detail; questions; answers; authorities; additionals } in
        let buf = Dns.Buf.create 1024 in
        let buf = marshal buf pkt in
        Cstruct.of_bigarray buf in

      (* Look for any local answers to this question *)
      begin
        t.local_names_cb question
        >>= function
        | Some answers -> Lwt_result.return (marshal_reply answers)
        | None ->
          (* Ask one server, with caching. Possible results are:
            Ok (`Success buf): succesful reply
            Ok (`Failure buf): an error like NXDomain
            Error (`Msg m): a low-level error or timeout
          *)
          let one_rpc server =
            let open Dns_forward_config in
            let address = server.Server.address in
            (* Look in the cache *)
            match Cache.answer t.cache address question with
            | Some answers -> Lwt.return (Ok (`Success (marshal_reply answers)))
            | None ->
              let _, client = List.find (fun (s, _) -> s = server) t.connections in
              begin
                (* If no timeout is configured, we will stop listening after
                   5s to avoid leaking threads if a server is offline *)
                let timeout_ms = match server.Server.timeout_ms with None -> 5000 | Some x -> x in
                Lwt.pick [
                  ( Time.sleep (float_of_int timeout_ms /. 1000.0)
                    >>= fun () ->
                    Lwt.return (Error (`Msg "timeout")) );
                  Client.rpc client buffer
                ]
                >>= function
                | Error x -> Lwt.return (Error x)
                | Ok reply ->
                  (* Determine whether it's a success or a failure; if a success
                     then insert the value into the cache. *)
                  let len = Cstruct.len reply in
                  let buf = Dns.Buf.of_cstruct reply in
                  begin match Dns.Protocol.Server.parse (Dns.Buf.sub buf 0 len) with
                  | Some { detail = { rcode = NoError; _ }; answers = ((_ :: _) as answers); _ } ->
                    Cache.insert t.cache address question answers;
                    Lwt.return (Ok (`Success reply))
                  | packet ->
                    Lwt.return (Ok (`Failure (packet, reply)))
                  end
              end in

          (* Filter the list of servers using any "zone" setting -- this will
             prevent queries for private names being leaked to public servers
             (if configured).
             Group the servers into lists of equal priorities. *)
          choose_servers (List.map fst t.connections) request
          |>
          (* Send all requests in parallel to minimise the chance of hitting a
             timeout. Positive replies will be cached, but servers which don't
             recognise the name will be queried each time. *)
          List.map one_rpc
          |>
          Lwt_list.fold_left_s
            (fun best_so_far next_t -> match best_so_far with
              | Ok (`Success result) ->
                (* No need to wait for any of the rest: one success is good enough *)
                Lwt.return (Ok (`Success result))
              | best_so_far ->
                next_t >>= fun next ->
                begin match best_so_far, next with
                | _, Ok (`Success result) ->
                  Lwt.return (Ok (`Success result))
                | Ok (`Failure (a_packet, a_reply)), Ok (`Failure (b_packet, b_reply)) ->
                  begin match a_packet, b_packet with
                  (* Prefer NXDomain to errors like Refused *)
                  | Some { detail = { rcode = NXDomain; _ }; _ }, _ -> Lwt.return (Ok (`Failure (a_packet, a_reply)))
                  | _, Some { detail = { rcode = NXDomain; _ }; _ } -> Lwt.return (Ok (`Failure (b_packet, b_reply)))
                  | _, _ ->
                    (* other than that, the earlier error is better *)
                    Lwt.return (Ok (`Failure (a_packet, a_reply)))
                  end
                | Error _, Ok (`Failure (b_packet, b_reply)) ->
                  (* prefer a high-level error over a low-level (e.g. socket) error *)
                  Lwt.return (Ok (`Failure (b_packet, b_reply)))
                | best_so_far, _ ->
                  Lwt.return best_so_far
                end
            ) (Error (`Msg "no servers configured"))
          >>= function
          | Ok (`Success reply) -> Lwt_result.return reply
          | Ok (`Failure (_, reply)) -> Lwt_result.return reply
          | Error x -> Lwt_result.fail x
      end
    | Some { questions = _; _} ->
      Lwt_result.fail (`Msg "cannot handle DNS packets where len(questions)<>1")
    | None ->
      Lwt_result.fail (`Msg "failed to parse request")

end
