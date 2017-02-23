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

module type Comparable = sig
  type t

  val compare: t -> t -> int
end

module type FLOW_CLIENT = sig
  include Mirage_flow.SHUTDOWNABLE
  type address
  val connect: ?read_buffer_size:int -> address
    -> (flow, error) Result.result Lwt.t
end

module type FLOW_SERVER = sig
  type server
  type address
  type error
  val bind: address -> (server, error) Result.result Lwt.t
  val getsockname: server -> address
  type flow
  val listen: server -> (flow -> unit Lwt.t) -> unit
  val shutdown: server -> unit Lwt.t
end

module type RPC_CLIENT = sig
  type request = Cstruct.t
  type response = Cstruct.t
  type address = Dns_forward_config.Address.t
  type error
  type t
  type message_cb = ?src:address -> ?dst:address -> buf:Cstruct.t -> unit -> unit Lwt.t
  val connect: ?message_cb:message_cb -> address -> (t, error) Result.result Lwt.t
  val rpc: t -> request -> (response, error) Result.result Lwt.t
  val disconnect: t -> unit Lwt.t
end

module type RPC_SERVER = sig
  type request = Cstruct.t
  type response = Cstruct.t
  type address = Dns_forward_config.Address.t

  type error
  val pp_error: error Fmt.t
  type write_error
  val pp_write_error: write_error Fmt.t

  type server
  val bind: address -> (server, error) Result.result Lwt.t
  val listen: server -> (request -> (response, error) Result.result Lwt.t) -> (unit, error) Result.result Lwt.t
  val shutdown: server -> unit Lwt.t
end

module type RESOLVER = sig
  type t
  type error
  type address = Dns_forward_config.Address.t
  type message_cb = ?src:address -> ?dst:address -> buf:Cstruct.t -> unit -> unit Lwt.t
  val create:
    ?local_names_cb:(Dns.Packet.question -> Dns.Packet.rr list option Lwt.t) ->
    ?message_cb:message_cb ->
    Dns_forward_config.t ->
    t Lwt.t
  val destroy: t -> unit Lwt.t
  val answer: Cstruct.t -> t -> (Cstruct.t, error) Result.result Lwt.t
end

module type SERVER = sig
  type t
  type resolver
  type error
  val create: resolver -> t Lwt.t
  val serve:
    address:Dns_forward_config.Address.t ->
    t -> (unit, error) Result.result Lwt.t
  val destroy: t -> unit Lwt.t
end

module type READERWRITER = sig
  (** Read and write DNS packets from a flow *)
  type request = Cstruct.t
  type response = Cstruct.t
  type t
  type error
  val pp_error: error Fmt.t
  type write_error
  val pp_write_error: write_error Fmt.t
  type flow
  val connect: flow -> t
  val read: t -> (request Mirage_flow.or_eof, error) Result.result Lwt.t
  val write: t -> response -> (unit, write_error) Result.result Lwt.t
  val close: t -> unit Lwt.t
end
