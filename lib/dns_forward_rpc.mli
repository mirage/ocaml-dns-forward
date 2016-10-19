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

(** Implement the client and server DNS RPC protocol*)

module type Client = Dns_forward_s.RPC_CLIENT
module type Server = Dns_forward_s.RPC_SERVER

module Make
  (Sockets: Dns_forward_s.SOCKETS)
  (Packet: Dns_forward_s.READERWRITER with type flow = Sockets.flow)
  (Time: V1_LWT.TIME): sig
  type request = Cstruct.t
  type response = Cstruct.t
  type address = Dns_forward_config.address

  include Dns_forward_s.RPC_CLIENT
    with type request  := request
     and type response := response
     and type address  := address

  include Dns_forward_s.RPC_SERVER
    with type request  := request
     and type response := response
     and type address  := address
end
