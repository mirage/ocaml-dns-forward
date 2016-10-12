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
open Lwt.Infix

let src =
  let src = Logs.Src.create "Dns_forward_lwt_unix" ~doc:"Lwt_unix-based I/O" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Tcp_client = struct
  type 'a io = 'a Lwt.t

  type flow = {
    mutable fd: Lwt_unix.file_descr option;
    read_buffer_size: int;
    mutable read_buffer: Cstruct.t;
  }

  type error = [
   | `Msg of string
  ]

  let error_message = function
   | `Msg x -> x

  let errorf fmt = Printf.ksprintf (fun s -> Lwt.return (`Error (`Msg s))) fmt

  type address = Ipaddr.t * int

  let connect ?(read_buffer_size=65536) (dst, dst_port) =
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string @@ Ipaddr.to_string dst, dst_port) in
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    Lwt.catch
      (fun () ->
         Lwt_unix.connect fd sockaddr
         >>= fun () ->
         let read_buffer = Cstruct.create read_buffer_size in
         Lwt.return (`Ok { fd = Some fd; read_buffer_size; read_buffer })
      )
      (fun e ->
         Lwt_unix.close fd
         >>= fun () ->
         errorf "Lwt_unix.connect: caught %s" (Printexc.to_string e)
      )

  type buffer = Cstruct.t

  let read t = match t.fd with
    | None -> Lwt.return `Eof
    | Some fd ->
      if Cstruct.len t.read_buffer = 0 then t.read_buffer <- Cstruct.create t.read_buffer_size;
      Lwt.catch
        (fun () ->
           Lwt_bytes.read fd t.read_buffer.Cstruct.buffer t.read_buffer.Cstruct.off t.read_buffer.Cstruct.len
           >>= function
           | 0 -> Lwt.return `Eof
           | n ->
             let results = Cstruct.sub t.read_buffer 0 n in
             t.read_buffer <- Cstruct.shift t.read_buffer n;
             Lwt.return (`Ok results)
        ) (fun e ->
            Log.err (fun f -> f "Socket.TCPV4.read: caught %s returning Eof" (Printexc.to_string e));
            Lwt.return `Eof
          )

  let write t buf = match t.fd with
    | None -> Lwt.return `Eof
    | Some fd ->
      Lwt.catch
        (fun () ->
           Lwt_cstruct.(complete (write fd) buf)
           >>= fun () ->
           Lwt.return (`Ok ())
        ) (fun _e ->
            Lwt.return `Eof
          )

  let writev t bufs = match t.fd with
    | None -> Lwt.return `Eof
    | Some fd ->
      Lwt.catch
        (fun () ->
           let rec loop = function
             | [] -> Lwt.return (`Ok ())
             | buf :: bufs ->
               Lwt_cstruct.(complete (write fd) buf)
               >>= fun () ->
               loop bufs in
           loop bufs
        ) (fun _e ->
            Lwt.return `Eof
          )

  let close t = match t.fd with
    | None -> Lwt.return_unit
    | Some fd ->
      t.fd <- None;
      Lwt_unix.close fd

  let shutdown_read t = match t.fd with
    | None -> Lwt.return_unit
    | Some fd ->
      Lwt.catch
        (fun () ->
           Lwt_unix.shutdown fd Unix.SHUTDOWN_RECEIVE;
           Lwt.return_unit
        ) (function
          | Unix.Unix_error(Unix.ENOTCONN, _, _) -> Lwt.return_unit
          | _e ->
            Lwt.return_unit
          )

  let shutdown_write t = match t.fd with
    | None -> Lwt.return_unit
    | Some fd ->
      Lwt.catch
        (fun () ->
           Lwt_unix.shutdown fd Unix.SHUTDOWN_SEND;
           Lwt.return_unit
        ) (function
          | Unix.Unix_error(Unix.ENOTCONN, _, _) -> Lwt.return_unit
          | _e ->
            Lwt.return_unit
          )
 end

 module Tcp_server = struct
 end
