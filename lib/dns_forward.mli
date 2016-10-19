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

module Flow: sig
  (** A BSD-socket-like interface for establishing flows by connecting to a
      well-known address (see Client) or by listening for incoming connections
      on a well-known address (see Server) *)

  module type Client = sig
    include Mirage_flow_s.SHUTDOWNABLE

    type address
    (** Identifies an endpoint for [connect] *)

    val connect: ?read_buffer_size:int -> address
      -> [ `Ok of flow | `Error of [ `Msg of string ] ] Lwt.t
    (** [connect address] creates a connection to [address] and returns
        he connected flow. *)
  end
  module type Server = sig
    type server
    (* A server bound to some address *)

    type address

    val bind: address -> [ `Ok of server | `Error of [ `Msg of string ]] Lwt.t
    (** Bind a server to an address *)

    val getsockname: server -> address
    (** Query the address the server is bound to *)

    type flow

    val listen: server -> (flow -> unit Lwt.t) -> unit
    (** Accept connections forever, calling the callback with each one.
        Connections are closed automatically when the callback finishes. *)

    val shutdown: server -> unit Lwt.t
    (** Stop accepting connections on the given server *)
  end
end

module Framing: sig
  (** DNS messages are framed when sent over other protocols. These modules
      convert byte-stream flows into streams of framed messages. *)

  module type S = sig
    (** Read and write framed DNS packets *)

    type request = Cstruct.t (** A DNS request *)
    type response = Cstruct.t (** A DNS response *)
    type t
    (** A connection which can read and write complete DNS messages *)

    type flow
    (** The flow over which we read and write complete DNS messages *)

    val connect: flow -> t
    (** Prepare to read and write complete DNS messages over the given flow *)

    val read: t -> [ `Ok of request | `Error of [ `Msg of string ] ] Lwt.t
    (** Read a complete DNS message *)

    val write: t -> response -> [ `Ok of unit | `Error of [ `Msg of string ] ] Lwt.t
    (** Write a complete DNS message *)

    val close: t -> unit Lwt.t
    (** Free resources and close the underlying flow *)
  end

  module Tcp(Flow: V1_LWT.FLOW): S with type flow = Flow.flow
  (** Use TCP framing *)

  module Udp(Flow: V1_LWT.FLOW): S with type flow = Flow.flow
  (** Use UDP framing *)
end

module type RPC_CLIENT = sig
  type request = Cstruct.t
  type response = Cstruct.t
  type address = Dns_forward_config.address
  type t
  val connect: address -> [ `Ok of t | `Error of [ `Msg of string ] ] Lwt.t
  val rpc: t -> request -> [ `Ok of response | `Error of [ `Msg of string ] ] Lwt.t
  val disconnect: t -> unit Lwt.t
end

module type RPC_SERVER = sig
  type request = Cstruct.t
  type response = Cstruct.t
  type address = Dns_forward_config.address

  type server
  val bind: address -> [ `Ok of server | `Error of [ `Msg of string ] ] Lwt.t
  val listen: server -> (request -> [ `Ok of response | `Error of [ `Msg of string ] ] Lwt.t) -> [`Ok of unit | `Error of [ `Msg of string ]] Lwt.t
  val shutdown: server -> unit Lwt.t
end

module Resolver: sig
  (** A Resolver converts a DNS query into an optional DNS response. *)

  module type S = sig
    type t

    val create: Dns_forward_config.t -> t Lwt.t
    (** Construct a resolver given some configuration *)

    val destroy: t -> unit Lwt.t
    (** Destroy and free all resources associated with the resolver *)

    val answer:
      ?local_names_cb:(Dns.Packet.question -> Dns.Packet.rr list option Lwt.t) ->
      ?timeout:float ->
      Cstruct.t ->
      t -> [ `Ok of Cstruct.t | `Error of [ `Msg of string ] ] Lwt.t
    (** Process a query by first checking whether the name can be satisfied
        locally via the [local_names_cb] and failing that, sending it to
        upstream servers according to the resolver configuration. By default
        the call will timeout after a few seconds, but this can be customised
        via the [timeout] optional argument. *)
  end

  module Make(Client: RPC_CLIENT)(Time: V1_LWT.TIME): S
  (** Construct a DNS resolver which will use the given [Client] Implementation
      to contact upstream servers, and the given [Time] implementation to handle
      timeouts. *)
end

module Server: sig
  (** A server listens for incoming connections containing streams of requests
      and attempts to answer them using the given client. *)

  module type S = sig
    type t
    (** A forwarding DNS proxy *)

    val create: Dns_forward_config.t -> t Lwt.t
    (** Construct a forwarding DNS proxy given some configuration *)

    val serve:
      address:Dns_forward_config.address ->
      ?local_names_cb:(Dns.Packet.question -> Dns.Packet.rr list option Lwt.t) ->
      ?timeout:float ->
      t -> [ `Ok of unit | `Error of [ `Msg of string ] ] Lwt.t
    (** Serve requests on the given [address] forever *)

    val destroy: t -> unit Lwt.t
    (** Shutdown the server and release allocated resources *)
  end

  module Make(Server: RPC_SERVER)(Client: RPC_CLIENT)(Time: V1_LWT.TIME): S

end
