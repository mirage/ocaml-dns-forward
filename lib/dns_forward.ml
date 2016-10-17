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
  let src = Logs.Src.create "Dns_forward" ~doc:"DNS forwarding" in
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

let choose_servers config request =
  let open Dns.Packet in
  let open Dns_forward_config in
  (* Match the name in the query against the configuration *)
  begin match request with
  | { questions = [ { q_name; _ } ]; _ } ->
    let labels = Dns.Name.to_string_list q_name in
    let matching_servers = List.filter (fun server ->
      List.fold_left (||) false @@ List.map (is_in_domain labels) server.zones
    ) config in
    begin match matching_servers with
    | _ :: _ ->
      (* If any of the configured domains match, send to these servers *)
      matching_servers
    | [] ->
      (* Otherwise send to all servers with no match *)
      List.filter (fun server -> server.zones = []) config
    end
  | _ -> []
  end

let or_fail_msg m = m >>= function
  | `Error `Eof -> Lwt.fail End_of_file
  | `Error (`Msg m) -> Lwt.fail (Failure m)
  | `Ok x -> Lwt.return x

module Make(Server: Dns_forward_s.RPC_SERVER)(Client: Dns_forward_s.RPC_CLIENT)(Time: V1_LWT.TIME) = struct

  type t = {
    connections: (Dns_forward_config.server * Client.t) list;
  }

  let make config =
    Lwt_list.map_s (fun server ->
      or_fail_msg @@ Client.connect server.Dns_forward_config.address
      >>= fun client ->
      Lwt.return (server, client)
    ) config
    >>= fun connections ->
    Lwt.return { connections }

  let answer buffer t =
    let len = Cstruct.len buffer in
    let buf = Dns.Buf.of_cstruct buffer in
    match Dns.Protocol.Server.parse (Dns.Buf.sub buf 0 len) with
    | Some request ->
      let servers = choose_servers (List.map fst t.connections) request in
      (* send the request to all upstream servers *)
      let rpc server =
        let open Dns_forward_config in
        let _, client = List.find (fun (s, _) -> s = server) t.connections in
        Log.debug (fun f -> f "forwarding to server %s:%d" (Ipaddr.to_string server.address.ip) server.address.port);
        or_fail_msg @@ Client.rpc client buffer
        >>= fun reply ->
        Lwt.return (Some reply) in

      (* Pick the first reply to come back, or timeout *)
      ( Lwt.pick @@ (Time.sleep 2. >>= fun () -> Lwt.return None) :: (List.map rpc servers)
        >>= function
        | None -> Lwt.return (`Error (`Msg "no response within the timeout"))
        | Some reply -> Lwt.return (`Ok reply)
      )
    | None ->
      Lwt.return (`Error (`Msg "failed to parse request"))

  let serve ~address t =
    let open Dns_forward_error.Infix in
    Server.bind address
    >>= fun server ->
    Server.listen server (fun buf -> answer buf t)
    >>= fun () ->
    Lwt.return (`Ok ())
end
