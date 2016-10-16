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

module Make(Tcp: Dns_forward_s.TCPIP)(Time: V1_LWT.TIME) = struct
  type address = Dns_forward_config.address

  module C = Channel.Make(Tcp)

  type t = {
    address: address;
    mutable c: C.t option;
    mutable disconnect_on_idle: unit Lwt.t;
    wakeners: (int, [ `Ok of Cstruct.t | `Error of [ `Msg of string ] ] Lwt.u) Hashtbl.t;
    m: Lwt_mutex.t;
    write_m: Lwt_mutex.t;
  }

  module Error = Dns_forward_error.Infix
  module FlowError = Dns_forward_error.FromFlowError(Tcp)
  let errorf = Dns_forward_error.errorf

  let disconnect t =
    Lwt_mutex.with_lock t.m
      (fun () ->
        match t with
        | { c = Some c; _ } as t ->
          t.c <- None;
          let tbl = Hashtbl.copy t.wakeners in
          Hashtbl.clear t.wakeners;
          let error = `Error (`Msg "connection to server was closed") in
          Hashtbl.iter (fun id u ->
            Log.err (fun f -> f "disconnect: failing request with id %d" id);
            Lwt.wakeup_later u error
          ) tbl;
          Tcp.close @@ C.to_flow c
        | _ -> Lwt.return_unit
      )

  let read_buffer c =
    let open Error in
    Lwt.catch
      (fun () ->
        let open Lwt.Infix in
        C.read_exactly ~len:2 c
        >>= fun bufs ->
        Lwt.return (`Ok bufs)
      ) (fun e ->
        errorf "Failed to read response header: %s" (Printexc.to_string e)
      )
    >>= fun bufs ->
    let buf = Cstruct.concat bufs in
    let len = Cstruct.BE.get_uint16 buf 0 in
    Lwt.catch
      (fun () ->
        let open Lwt.Infix in
        C.read_exactly ~len c
        >>= fun bufs ->
        Lwt.return (`Ok bufs)
      ) (fun e ->
        errorf "Failed to read response payload (%d bytes): %s" len (Printexc.to_string e)
      )
    >>= fun bufs ->
    Lwt.return (`Ok (Cstruct.concat bufs))

  (* Receive all the responses and demux to the right thread. When the connection
     is closed, `read_buffer` will fail and this thread will exit. *)
  let dispatcher t c () =
    let open Lwt.Infix in
    let rec loop () =
      read_buffer c
      >>= function
      | `Error (`Msg m) ->
        Log.info (fun f -> f "%s: dispatcher shutting down" m);
        Lwt.return_unit
      | `Ok buffer ->
        let buf = Dns.Buf.of_cstruct buffer in
        begin match Dns.Protocol.Server.parse (Dns.Buf.sub buf 0 (Cstruct.len buffer)) with
        | None ->
          Log.err (fun f -> f "failed to parse response");
          Lwt.fail (Failure "failed to parse response")
        | Some response ->
          let client_id = response.Dns.Packet.id in
          if Hashtbl.mem t.wakeners client_id then begin
            let u = Hashtbl.find t.wakeners client_id in
            Hashtbl.remove t.wakeners client_id;
            Lwt.wakeup_later u (`Ok buffer)
          end else begin
            Log.err (fun f -> f "failed to find a wakener for id %d" client_id);
          end;
          loop ()
        end
    in
    Lwt.catch loop
      (fun e ->
        Log.info (fun f -> f "dispatcher caught %s" (Printexc.to_string e));
        Lwt.return_unit
      )

  let get_c t =
    let open Error in
    Lwt.cancel t.disconnect_on_idle;
    Lwt_mutex.with_lock t.m
      (fun () -> match t.c with
        | None ->
          Tcp.connect (t.address.Dns_forward_config.ip, t.address.Dns_forward_config.port)
          >>= fun flow ->
          let c = C.create flow in
          t.c <- Some c;
          Lwt.async (dispatcher t c);
          Lwt.return (`Ok c)
        | Some c ->
          Lwt.return (`Ok c))
    >>= fun c ->
    (* Add a fresh idle timer *)
    t.disconnect_on_idle <- (let open Lwt.Infix in Time.sleep 30. >>= fun () -> disconnect t);
    Lwt.return (`Ok c)

  type request = Cstruct.t
  type response = Cstruct.t

  let connect address =
    Log.debug (fun f -> f "forwarding to server %s:%d"
      (Ipaddr.to_string address.Dns_forward_config.ip)
      address.Dns_forward_config.port);
    let c = None in
    let m = Lwt_mutex.create () in
    let disconnect_on_idle = Lwt.return_unit in
    let wakeners = Hashtbl.create 7 in
    let write_m = Lwt_mutex.create () in
    Lwt.return (`Ok { address; c; disconnect_on_idle; wakeners; m; write_m })

  let write_buffer c buffer =
    (* RFC 1035 4.2.2 TCP Usage: 2 byte length field *)
    let header = Cstruct.create 2 in
    Cstruct.BE.set_uint16 header 0 (Cstruct.len buffer);
    C.write_buffer c header;
    C.write_buffer c buffer;
    Lwt.catch
      (fun () ->
        let open Lwt.Infix in
        C.flush c
        >>= fun () ->
        Lwt.return (`Ok ())
      ) (fun e ->
        errorf "Failed to write %d bytes: %s" (Cstruct.len buffer) (Printexc.to_string e)
      )

  let rpc (t: t) buffer =
    let buf = Dns.Buf.of_cstruct buffer in
    match Dns.Protocol.Server.parse (Dns.Buf.sub buf 0 (Cstruct.len buffer)) with
    | None ->
      Log.err (fun f -> f "failed to parse request");
      Lwt.return (`Error (`Msg "failed to parse request"))
    | Some request ->
      (* FIXME: need to regenerate the id *)
      let client_id = request.Dns.Packet.id in
      let fresh_id = client_id in

      let th, u = Lwt.task () in
      Hashtbl.replace t.wakeners fresh_id u;

      (* If we fail to connect, return the error *)
      let open Lwt.Infix in
      begin
        let open Error in
        get_c t
        >>= fun c ->
        let open Lwt.Infix in

        (* An existing connection to the server might have been closed by the server;
           therefore if we fail to write the request, reconnect and try once more. *)
        Lwt_mutex.with_lock t.write_m (fun () -> write_buffer c buffer)
        >>= function
        | `Ok () ->
          Lwt.return (`Ok ())
        | `Error (`Msg m) ->
          Log.info (fun f -> f "caught %s writing request, attempting to reconnect" m);
          disconnect t
          >>= fun () ->
          let open Error in
          get_c t
          >>= fun c ->
          Lwt_mutex.with_lock t.write_m (fun () -> write_buffer c buffer)
      end
      >>= function
      | `Error (`Msg m) ->
        Hashtbl.remove t.wakeners fresh_id;
        Lwt.return (`Error (`Msg m))
      | `Ok () ->
        th (* will be woken up by the dispatcher *)

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
    Tcp.listen server (fun flow ->
      let open Lwt.Infix in
      let c = C.create flow in
      let rec loop () =
        let open Error in
        read_buffer c
        >>= fun request ->
        (* FIXME: need to run these in the background *)
        (* FIXME: need to rewrite transaction IDs if the requests come from
           different resolvers *)
        let open Lwt.Infix in
        cb request
        >>= function
        | `Error _ ->
          loop ()
        | `Ok response ->
          let open Error in
          write_buffer c response
          >>= fun () ->
          loop () in
      loop ()
      >>= function
      | `Error (`Msg m) ->
        Log.err (fun f -> f "server loop failed with: %s" m);
        Lwt.return_unit
      | `Ok () ->
        Lwt.return_unit
    );
    Lwt.return (`Ok ())

  let shutdown server =
    Tcp.shutdown server.server

end
