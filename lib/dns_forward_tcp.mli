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

(** DNS over TCP uses a simple header to delineate message boundaries *)

module ReaderWriter(Flow: V1_LWT.FLOW): sig
  include Dns_forward_s.READERWRITER

  val connect: Flow.flow -> t
end

module Make(Tcp: Dns_forward_s.TCPIP)(Time: V1_LWT.TIME): sig
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
