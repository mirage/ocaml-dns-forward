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


module Make_resolver(Client: Dns_forward_s.RPC_CLIENT)(Time: V1_LWT.TIME): sig
  (** A simple DNS resolver *)

  include Dns_forward_s.RESOLVER

  val create: Dns_forward_config.t -> t Lwt.t
  (** Construct a resolver given some configuration *)

  val destroy: t -> unit Lwt.t
  (** Destroy and free all resources associated with the resolver *)
end

module Make_server(Server: Dns_forward_s.RPC_SERVER)(Client: Dns_forward_s.RPC_CLIENT)(Time: V1_LWT.TIME): sig

  type t
  (** A forwarding DNS proxy *)

  val create: Dns_forward_config.t -> t Lwt.t
  (** Construct a forwarding DNS proxy given some configuration *)

  val serve:
    address:Dns_forward_config.address ->
    ?local_names_cb:(Dns.Packet.question -> Dns.Packet.rr list option Lwt.t) ->
    t -> [ `Ok of unit | `Error of [ `Msg of string ] ] Lwt.t
  (** Serve requests on the given [address] forever *)

  val destroy: t -> unit Lwt.t
  (** Shutdown the server and release allocated resources *)
end
