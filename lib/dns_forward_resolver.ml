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
        (* Otherwise send to all servers with no match *)
        List.filter (fun server -> server.Server.zones = Domain.Set.empty) config in
    (* Now we order by the order field *)
    let orders = List.fold_left (fun set server -> IntSet.add server.Server.order set) IntSet.empty all in
    List.map
      (fun order ->
        List.filter (fun server -> server.Server.order = order) all
      ) (IntSet.elements orders)
  | _ -> [ [] ]
  end

let or_fail_msg m = m >>= function
  | Result.Error `Eof -> Lwt.fail End_of_file
  | Result.Error (`Msg m) -> Lwt.fail (Failure m)
  | Result.Ok x -> Lwt.return x

let or_option m = m >>= function
  | Result.Error _ -> Lwt.return None
  | Result.Ok x -> Lwt.return (Some x)

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
        let detail = { request.detail with Dns.Packet.qr = Dns.Packet.Response } in
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
          (* Possible outcomes are:
             - Some <result>: candidate for returning to the client
             - None: timeout
             - failure: probably a network error *)
          let one_rpc server =
            let open Dns_forward_config in
            let address = server.Server.address in
            (* Look in the cache *)
            match Cache.answer t.cache address question with
            | Some answers -> Lwt.return (Some (marshal_reply answers))
            | None ->
              let _, client = List.find (fun (s, _) -> s = server) t.connections in
              let request = or_option @@ Client.rpc client buffer in
              begin
                begin match server.Server.timeout_ms with
                | None -> request
                | Some t -> Lwt.pick [ (Time.sleep (float_of_int t /. 1000.0) >>= fun () -> Lwt.return None); request ]
                end >>= function
                | None -> Lwt.return None
                | Some reply ->
                  (* Insert the reply into the cache *)
                  let len = Cstruct.len reply in
                  let buf = Dns.Buf.of_cstruct reply in
                  begin match Dns.Protocol.Server.parse (Dns.Buf.sub buf 0 len) with
                  | Some { answers; _ } -> if answers <> [] then Cache.insert t.cache address question answers
                  | _ -> ()
                  end;
                  Lwt.return (Some reply)
              end in

          (* Send the request to all relevant servers in groups. If no response
             is heard from a group, then we proceed to the next group *)
          Lwt_list.fold_left_s
            (fun acc servers -> match acc with
              | None ->
                let result_t, result_u = Lwt.task () in
                let all = List.map (fun server ->
                  one_rpc server
                  >>= function
                  | Some result ->
                    (* first positive response becomes the result *)
                    (try Lwt.wakeup_later result_u (Some result) with Invalid_argument _ -> ());
                    Lwt.return_unit
                  | None -> Lwt.return_unit
                ) servers in
                (* Wait until either a positive result or all threads have quit *)
                let all_quit = Lwt.catch (fun () -> Lwt.join all >>= fun () -> Lwt.return_none) (fun _ -> Lwt.return_none) in
                Lwt.choose [ all_quit; result_t ]
                >>= fun _ ->
                (* If we have a result, return it *)
                begin match Lwt.state result_t with
                | Lwt.Return x -> Lwt.return x
                | _ -> Lwt.return_none
                end
              | r -> Lwt.return r (* got a reply already *)
            ) None (choose_servers (List.map fst t.connections) request)
          >>= function
          | None -> Lwt_result.fail (`Msg "no response within the timeout")
          | Some reply -> Lwt_result.return reply
      end
    | Some { questions = _; _} ->
      Lwt_result.fail (`Msg "cannot handle DNS packets where len(questions)<>1")
    | None ->
      Lwt_result.fail (`Msg "failed to parse request")

end
