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
  let src = Logs.Src.create "Dns_forward" ~doc:"DNS over TCP" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Make(Tcp: Dns_forward_s.TCPIP) = struct
  type address = Dns_forward_config.address

  module C = Channel.Make(Tcp)

  type t = {
    address: address;
    c: C.t;
  }

  type request = Cstruct.t
  type response = Cstruct.t

  module Error = Dns_forward_error.Infix
  module FlowError = Dns_forward_error.FromFlowError(Tcp)

  let connect address =
    Log.debug (fun f -> f "forwarding to server %s:%d"
      (Ipaddr.to_string address.Dns_forward_config.ip)
      address.Dns_forward_config.port);
    let open Error in
    Tcp.connect (address.Dns_forward_config.ip, address.Dns_forward_config.port)
    >>= fun flow ->
    let c = C.create flow in
    Lwt.return (`Ok { address; c })

  let disconnect { c; _ } =
    Tcp.close @@ C.to_flow c

  let write_buffer c buffer =
    (* RFC 1035 4.2.2 TCP Usage: 2 byte length field *)
    let header = Cstruct.create 2 in
    Cstruct.BE.set_uint16 header 0 (Cstruct.len buffer);
    C.write_buffer c header;
    C.write_buffer c buffer;
    C.flush c

  let read_buffer c =
    let open Lwt.Infix in
    C.read_exactly ~len:2 c
    >>= fun bufs ->
    let buf = Cstruct.concat bufs in
    let len = Cstruct.BE.get_uint16 buf 0 in
    C.read_exactly ~len c
    >>= fun bufs ->
    Lwt.return (Cstruct.concat bufs)

  let rpc (t: t) request =
    let open Lwt.Infix in
    write_buffer t.c request
    >>= fun () ->
    read_buffer t.c
    >>= fun buf ->
    Lwt.return (`Ok buf)

  type server = {
    address: address;
    server: Tcp.server;
  }

  let bind address =
    let open Error in
    Tcp.bind (address.Dns_forward_config.ip, address.Dns_forward_config.port)
    >>= fun server ->
    Lwt.return (`Ok { address; server })

  let listen { server; _ } cb =
    let open Lwt.Infix in
    Tcp.listen server (fun flow ->
      let c = C.create flow in
      let rec loop () =
        read_buffer c
        >>= fun request ->
        (* FIXME: need to run these in the background *)
        (* FIXME: need to rewrite transaction IDs if the requests come from
           different resolvers *)
        cb request
        >>= function
        | `Error _ -> Lwt.return_unit
        | `Ok response ->
          write_buffer c response
          >>= fun () ->
          loop () in
      loop ()
    );
    Lwt.return (`Ok ())

  let shutdown server =
    Tcp.shutdown server.server

end
