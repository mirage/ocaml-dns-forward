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

  let rpc (t: t) request =
    let open Lwt.Infix in
    (* RFC 1035 4.2.2 TCP Usage: 2 byte length field *)
    let header = Cstruct.create 2 in
    Cstruct.BE.set_uint16 header 0 (Cstruct.len request);
    C.write_buffer t.c header;
    C.write_buffer t.c request;
    C.flush t.c
    >>= fun () ->
    C.read_exactly ~len:2 t.c
    >>= fun bufs ->
    let buf = Cstruct.concat bufs in
    let len = Cstruct.BE.get_uint16 buf 0 in
    C.read_exactly ~len t.c
    >>= fun bufs ->
    Lwt.return (`Ok (Cstruct.concat bufs))

end
