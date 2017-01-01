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
  let src = Logs.Src.create "Dns_forward" ~doc:"DNS framing" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module type S = Dns_forward_s.READERWRITER

module Tcp(Flow: V1_LWT.FLOW) = struct

  module C = Channel.Make(Flow)

  type request = Cstruct.t
  type response = Cstruct.t
  type flow = Flow.flow
  type t = {
    c: C.t;
    write_m: Lwt_mutex.t;
    read_m: Lwt_mutex.t;
  }

  let connect flow =
    let c = C.create flow in
    let write_m = Lwt_mutex.create () in
    let read_m = Lwt_mutex.create () in
    { c; write_m; read_m }

  let close t =
    Flow.close @@ C.to_flow t.c

  let (>>==) a fn =
    let open Lwt.Infix in
    a >>= function
    | Error e -> Lwt.return (Error e)
    | Ok `Eof -> Lwt.return (Ok `Eof)
    | Ok (`Data x) -> fn x 

  let read t =
    Lwt_mutex.with_lock t.read_m
      (fun () ->
        C.read_exactly ~len:2 t.c >>== fun bufs ->
        let buf = Cstruct.concat bufs in
        let len = Cstruct.BE.get_uint16 buf 0 in
        C.read_exactly ~len t.c >>== fun bufs ->
        Lwt.return (Ok (`Data (Cstruct.concat bufs)))
      )

  let write t buffer =
    Lwt_mutex.with_lock t.write_m
      (fun () ->
        (* RFC 1035 4.2.2 TCP Usage: 2 byte length field *)
        let header = Cstruct.create 2 in
        Cstruct.BE.set_uint16 header 0 (Cstruct.len buffer);
        C.write_buffer t.c header;
        C.write_buffer t.c buffer;
        C.flush t.c
      )
end

module Udp(Flow: V1_LWT.FLOW) = struct
  type request = Cstruct.t
  type response = Cstruct.t
  type flow = Flow.flow
  type t = Flow.flow

  let connect flow = flow
  let close t = Flow.close t
  let read t = Flow.read t
  let write t buf = Flow.write t buf
end
