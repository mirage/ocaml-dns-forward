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
  let src = Logs.Src.create "Dns_forward" ~doc:"DNS over UDP" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module ReaderWriter(Flow: V1_LWT.FLOW) = struct
  module Error = Dns_forward_error.Infix
  let errorf = Dns_forward_error.errorf

  type request = Cstruct.t
  type response = Cstruct.t
  type t = Flow.flow

  let connect flow = flow

  let close t =
    Flow.close t

  let read t =
    let open Lwt.Infix in
    Flow.read t
    >>= function
    | `Ok buf ->
      Lwt.return (`Ok buf)
    | `Eof ->
      errorf "read: Eof"
    | `Error e ->
      errorf "read: %s" (Flow.error_message e)

  let write t buf =
    let open Lwt.Infix in
    Flow.write t buf
    >>= function
    | `Ok buf ->
      Lwt.return (`Ok buf)
    | `Eof ->
      errorf "read: Eof"
    | `Error e ->
      errorf "write: %s" (Flow.error_message e)
end

module Make(Udp: Dns_forward_s.SOCKETS) = struct
  type address = Dns_forward_config.address

  module RW = ReaderWriter(Udp)

  type t = {
    address: address;
    rw: RW.t;
  }

  type request = Cstruct.t
  type response = Cstruct.t

  module Error = Dns_forward_error.Infix
  module FlowError = Dns_forward_error.FromFlowError(Udp)

  let connect address =
    let open Error in
    Udp.connect (address.Dns_forward_config.ip, address.Dns_forward_config.port)
    >>= fun flow ->
    let rw = RW.connect flow in
    Lwt.return (`Ok { address; rw })

  let disconnect { rw; _ } =
    RW.close rw

  let rpc (t: t) request =
    let open Error in
    RW.write t.rw request
    >>= fun () ->
    RW.read t.rw
    >>= fun reply ->
    Lwt.return (`Ok reply)

  type server = {
    address: address;
    server: Udp.server;
  }

  let bind address =
    let open Error in
    Udp.bind (address.Dns_forward_config.ip, address.Dns_forward_config.port)
    >>= fun server ->
    Lwt.return (`Ok { address; server })

  let listen { server; _ } cb =
    let open Lwt.Infix in
    Udp.listen server (fun flow ->
      let rw = RW.connect flow in
      ( RW.read rw
        >>= function
        | `Error _ -> Lwt.return_unit
        | `Ok request ->
          ( cb request
            >>= function
            | `Error _ -> Lwt.return_unit
            | `Ok response ->
              ( RW.write rw response
                >>= function
                | `Error _ -> Lwt.return_unit
                | `Ok () -> Lwt.return_unit ) ) )
      );
    Lwt.return (`Ok ())

  let shutdown server =
    Udp.shutdown server.server
end
