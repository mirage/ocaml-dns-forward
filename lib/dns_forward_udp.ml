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

module Make(Udp: Dns_forward_s.TCPIP) = struct
  type address = Dns_forward_config.address

  type t = {
    address: address;
    flow: Udp.flow;
  }

  type request = Cstruct.t
  type response = Cstruct.t

  module Error = Dns_forward_error.Infix
  module FlowError = Dns_forward_error.FromFlowError(Udp)

  let connect address =
    Log.debug (fun f -> f "forwarding to server %s:%d"
      (Ipaddr.to_string address.Dns_forward_config.ip)
      address.Dns_forward_config.port);
    let open Error in
    Udp.connect (address.Dns_forward_config.ip, address.Dns_forward_config.port)
    >>= fun flow ->
    Lwt.return (`Ok { address; flow })

  let disconnect { flow; _ } =
    Udp.close flow

  let rpc (t: t) request =
    let open FlowError in
    Udp.write t.flow request
    >>= fun () ->
    Udp.read t.flow
    >>= fun reply ->
    Lwt.return (`Ok reply)

end
