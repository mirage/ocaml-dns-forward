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

module Error: sig
  type 'a t = [ `Ok of 'a | `Error of [ `Msg of string ] ]
  (** All errors are currently fatal and should cause the request being processed
      or the program to abort *)

  module Lwt: sig
    module Infix: sig
      val (>>=): [< `Error of [< `Msg of 'a ] | `Ok of 'b ] Lwt.t ->
        ('b -> ([> `Error of [> `Msg of 'a ] ] as 'c) Lwt.t) -> 'c Lwt.t
    end
  end

end

module Flow: sig
  (** A BSD-socket-like interface for establishing flows by connecting to a
      well-known address (see Client) or by listening for incoming connections
      on a well-known address (see Server) *)

  module type Client = sig
    include Mirage_flow_s.SHUTDOWNABLE

    type address
    (** Identifies an endpoint for [connect] *)

    val connect: ?read_buffer_size:int -> address -> flow Error.t Lwt.t
    (** [connect address] creates a connection to [address] and returns
        he connected flow. *)
  end
  module type Server = sig
    type server
    (* A server bound to some address *)

    type address

    val bind: address -> server Error.t Lwt.t
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

    type request = Cstruct.t
    (** A DNS request *)

    type response = Cstruct.t
    (** A DNS response *)

    type t
    (** A connection which can read and write complete DNS messages *)

    type flow
    (** The flow over which we read and write complete DNS messages *)

    val connect: flow -> t
    (** Prepare to read and write complete DNS messages over the given flow *)

    val read: t -> request Error.t Lwt.t
    (** Read a complete DNS message *)

    val write: t -> response -> unit Error.t Lwt.t
    (** Write a complete DNS message *)

    val close: t -> unit Lwt.t
    (** Free resources and close the underlying flow *)
  end

  module Tcp(Flow: V1_LWT.FLOW): S with type flow = Flow.flow
  (** Use TCP framing *)

  module Udp(Flow: V1_LWT.FLOW): S with type flow = Flow.flow
  (** Use UDP framing *)
end

module Config: sig
  type address = {
    ip: Ipaddr.t;
    port: int;
  }
  (** The address of a DNS server *)

  type domain = string list
  (** A DNS domain e.g. [ "a"; "b" ] would be the domain a.b. *)

  type server = {
    zones: domain list; (** use this server for these specific domains *)
    address: address;
  }
  (** A single upstream DNS server. If [zones = []] then the server can handle
      all queries; otherwise [zones] is a list of domains that this server
      should be preferentially queried for. For example if an organisation
      has a VPN and a special DNS server for the domain `mirage.io` it may
      want to only send queries for `foo.mirage.io` to this server and avoid
      leaking internal names by sending queries to public server. *)

  type t = server list [@@deriving sexp]
  (** Upstream DNS servers *)

end

module Rpc: sig
  (** A Remote Procedure Call client and server implementation *)

  module Client: sig
    module type S = sig
      type t
      (** A Remote Procedure Call client which can send requests to a server
          and receive responses. *)

      type request = Cstruct.t
      (** A complete request *)

      type response = Cstruct.t
      (** A complete response *)

      type address = Config.address
      (** The address of the remote endpoint *)

      val connect: address -> t Error.t Lwt.t
      (** Connect to the remote server *)

      val rpc: t -> request -> response Error.t Lwt.t
      (** Send a request and await a response. Multiple threads may call this
          in parallel and it is the implementation's job to associate the right
          response with the original request. *)

      val disconnect: t -> unit Lwt.t
      (** Disconnect from the server and free all resources. *)
    end

    module Make
      (Flow: Flow.Client with type address = Ipaddr.t * int)
      (Framing: Framing.S with type flow = Flow.flow)
      (Time: V1_LWT.TIME): S
    (** Construct an RPC client given a Flow and a method of Framing messages
        over the flow. *)
  end

  module Server: sig
    module type S = sig
      type server
      (** A Remote Procedure Call server which will listen on an address, accept
          incoming connections and answer them *)

      type request = Cstruct.t
      (** A complete request *)

      type response = Cstruct.t
      (** A complete response *)

      type address = Config.address
      (** The address of the server *)

      val bind: address -> server Error.t Lwt.t
      (** Bind to the given address. This will fail if the address does not exist
          or if another server is already bound there. *)

      val listen: server -> (request -> response Error.t Lwt.t) -> unit Error.t Lwt.t
      (** Listen and accept incoming connections, use the provided callback to
          answer requests. *)

      val shutdown: server -> unit Lwt.t
      (** Shutdown the server and free any allocated resources. *)
    end

    module Make
      (Flow: Flow.Server with type address = Ipaddr.t * int)
      (Framing: Framing.S with type flow = Flow.flow)
      (Time: V1_LWT.TIME): S
    (** Construct an RPC server given a Flow and a method of Framing messages
        over the flow. *)
  end
end

module Resolver: sig
  (** A Resolver converts a DNS query into an optional DNS response. *)

  module type S = sig
    type t

    val create: Config.t -> t Lwt.t
    (** Construct a resolver given some configuration *)

    val destroy: t -> unit Lwt.t
    (** Destroy and free all resources associated with the resolver *)

    val answer:
      ?local_names_cb:(Dns.Packet.question -> Dns.Packet.rr list option Lwt.t) ->
      ?timeout:float ->
      Cstruct.t ->
      t -> Cstruct.t Error.t Lwt.t
    (** Process a query by first checking whether the name can be satisfied
        locally via the [local_names_cb] and failing that, sending it to
        upstream servers according to the resolver configuration. By default
        the call will timeout after a few seconds, but this can be customised
        via the [timeout] optional argument. *)
  end

  module Make(Client: Rpc.Client.S)(Time: V1_LWT.TIME): S
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

    val create: Config.t -> t Lwt.t
    (** Construct a forwarding DNS proxy given some configuration *)

    val serve:
      address:Config.address ->
      ?local_names_cb:(Dns.Packet.question -> Dns.Packet.rr list option Lwt.t) ->
      ?timeout:float ->
      t -> unit Error.t Lwt.t
    (** Serve requests on the given [address] forever *)

    val destroy: t -> unit Lwt.t
    (** Shutdown the server and release allocated resources *)
  end

  module Make(Server: Rpc.Server.S)(Client: Rpc.Client.S)(Time: V1_LWT.TIME): S

end
