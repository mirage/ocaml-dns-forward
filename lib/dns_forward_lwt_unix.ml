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

let default_read_buffer_size = 65536
let max_udp_length = 65507 (* IP datagram (65535) - IP header(20) - UDP header(8) *)

module Common = struct
  (** Both UDP and TCP *)

  type error = [
   | `Msg of string
  ]

  let error_message = function
   | `Msg x -> x

  let errorf fmt = Printf.ksprintf (fun s -> Lwt.return (`Error (`Msg s))) fmt

  type address = Ipaddr.t * int

  type buffer = Cstruct.t

  let sockaddr_of_address (dst, dst_port) =
    Unix.ADDR_INET(Unix.inet_addr_of_string @@ Ipaddr.to_string dst, dst_port)

  let string_of_address (dst, dst_port) =
    Ipaddr.to_string dst ^ ":" ^ (string_of_int dst_port)

  type 'a io = 'a Lwt.t

  let getsockname fn_name fd_opt = match fd_opt with
    | None -> failwith (fn_name ^ ": socket is closed")
    | Some fd ->
      begin match Lwt_unix.getsockname fd with
      | Lwt_unix.ADDR_INET(iaddr, port) ->
        Ipaddr.V4 (Ipaddr.V4.of_string_exn (Unix.string_of_inet_addr iaddr)), port
      | _ -> invalid_arg (fn_name ^ ": passed a non-TCP socket")
      end
end

module Tcp = struct
  include Common

  type flow = {
    mutable fd: Lwt_unix.file_descr option;
    read_buffer_size: int;
    mutable read_buffer: Cstruct.t;
  }

  let of_fd ~read_buffer_size fd =
    let read_buffer = Cstruct.create read_buffer_size in
    { fd = Some fd; read_buffer_size; read_buffer }

  let connect ?(read_buffer_size=default_read_buffer_size) address =
    let sockaddr = sockaddr_of_address address in
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    Lwt.catch
      (fun () ->
         Lwt_unix.connect fd sockaddr
         >>= fun () ->
         Lwt.return (`Ok (of_fd ~read_buffer_size fd))
      )
      (fun e ->
         Lwt_unix.close fd
         >>= fun () ->
         errorf "Lwt_unix.connect: caught %s" (Printexc.to_string e)
      )

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

  type server = {
    mutable server_fd: Lwt_unix.file_descr option;
    read_buffer_size: int;
  }

  let bind address =
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    Lwt.catch
      (fun () ->
        Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
        Lwt_unix.bind fd (sockaddr_of_address address);
        Lwt.return (`Ok { server_fd = Some fd; read_buffer_size = default_read_buffer_size })
      ) (fun e ->
        Lwt_unix.close fd
        >>= fun () ->
        errorf "Tcp.bind failed to bind to %s: %s" (string_of_address address) (Printexc.to_string e)
      )

  let getsockname server = getsockname "Tcp.getsockname" server.server_fd

  let shutdown server = match server.server_fd with
    | None -> Lwt.return_unit
    | Some fd ->
      server.server_fd <- None;
      Lwt_unix.close fd

  let listen (server: server) cb =
    let rec loop fd =
      Lwt_unix.accept fd
      >>= fun (client, _sockaddr) ->
      let read_buffer_size = server.read_buffer_size in

      Lwt.async
       (fun () ->
         Lwt.catch
           (fun () ->
             Lwt.return (Some (of_fd ~read_buffer_size client))
           ) (fun _e ->
             Lwt_unix.close client
             >>= fun () ->
             Lwt.return_none
           )
         >>= function
         | None -> Lwt.return_unit
         | Some flow ->
          Lwt.finalize
            (fun () ->
              Lwt.catch
                (fun () -> cb flow)
                (fun e ->
                  Log.info (fun f -> f "caught %s so closing flow" (Printexc.to_string e));
                  Lwt.return_unit)
            ) (fun () -> close flow)
      );
      loop fd in
    match server.server_fd with
    | None -> ()
    | Some fd ->
        Lwt.async
          (fun () ->
            Lwt.catch
              (fun () ->
                Lwt.finalize
                  (fun () ->
                    Lwt_unix.listen fd 32;
                    loop fd
                  ) (fun () ->
                    shutdown server
                  )
              ) (fun e ->
                Log.info (fun f -> f "caught %s so shutting down server" (Printexc.to_string e));
                Lwt.return_unit
              )
          )
end

module Udp = struct
  include Common

  type flow = {
    mutable fd: Lwt_unix.file_descr option;
    read_buffer_size: int;
    mutable already_read: Cstruct.t option;
    sockaddr: Unix.sockaddr;
  }

  let connect ?(read_buffer_size = max_udp_length) address =
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    (* Win32 requires all sockets to be bound however macOS and Linux don't *)
    (try Lwt_unix.bind fd (Lwt_unix.ADDR_INET(Unix.inet_addr_any, 0)) with _ -> ());
    let already_read = None in
    let sockaddr = sockaddr_of_address address in
    Lwt.return (`Ok { fd = Some fd; read_buffer_size; already_read; sockaddr })

  let read t = match t.fd, t.already_read with
    | None, _ -> Lwt.return `Eof
    | Some _, Some data when Cstruct.len data > 0 ->
      t.already_read <- Some (Cstruct.sub data 0 0); (* next read is `Eof *)
      Lwt.return (`Ok data)
    | Some _, Some _ ->
      Lwt.return `Eof
    | Some fd, None ->
      let buffer = Cstruct.create t.read_buffer_size in
      let bytes = Bytes.make t.read_buffer_size '\000' in
      Lwt.catch
        (fun () ->
          (* Lwt on Win32 doesn't support Lwt_bytes.recvfrom *)
          Lwt_unix.recvfrom fd bytes 0 (Bytes.length bytes) []
          >>= fun (n, _) ->
          Cstruct.blit_from_bytes bytes 0 buffer 0 n;
          let response = Cstruct.sub buffer 0 n in
          Lwt.return (`Ok response)
        ) (fun e ->
          Log.err (fun f -> f "Udp.read: caught %s returning Eof" (Printexc.to_string e));
          Lwt.return `Eof
        )

  let write t buf = match t.fd with
    | None -> Lwt.return `Eof
    | Some fd ->
      Lwt.catch
        (fun () ->
          (* Lwt on Win32 doesn't support Lwt_bytes.sendto *)
          let bytes = Bytes.make (Cstruct.len buf) '\000' in
          Cstruct.blit_to_bytes buf 0 bytes 0 (Cstruct.len buf);
          Lwt_unix.sendto fd bytes 0 (Bytes.length bytes) [] t.sockaddr
          >>= fun _n ->
          Lwt.return (`Ok ())
        ) (fun e ->
          Log.err (fun f -> f "Udp.write: caught %s returning Eof" (Printexc.to_string e));
          Lwt.return `Eof
        )

  let writev t bufs = write t (Cstruct.concat bufs)

  let close t = match t.fd with
    | None -> Lwt.return_unit
    | Some fd ->
      t.fd <- None;
      Lwt_unix.close fd

  let shutdown_read _t = Lwt.return_unit
  let shutdown_write _t = Lwt.return_unit

  type server = {
    mutable server_fd: Lwt_unix.file_descr option;
  }

  let getsockname server = getsockname "Udp.getsockname" server.server_fd

  let bind address =
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    try
      let sockaddr = sockaddr_of_address address in
      Lwt_unix.bind fd sockaddr;
      Lwt.return (`Ok { server_fd = Some fd })
    with
    | e -> errorf "Udp.bind failed to bind address %s: %s"
      (string_of_address address) (Printexc.to_string e)

  let shutdown t = match t.server_fd with
    | None -> Lwt.return_unit
    | Some fd ->
      t.server_fd <- None;
      Lwt_unix.close fd

  let listen t flow_cb =
    let buffer = Cstruct.create max_udp_length in
    let bytes = Bytes.make max_udp_length '\000' in
    match t.server_fd with
    | None -> ()
    | Some fd ->
      let rec loop () =
        Lwt.catch
          (fun () ->
            (* Lwt on Win32 doesn't support Lwt_bytes.recvfrom *)
            Lwt_unix.recvfrom fd bytes 0 (Bytes.length bytes) []
            >>= fun (n, sockaddr) ->
            Cstruct.blit_from_bytes bytes 0 buffer 0 n;
            let data = Cstruct.sub buffer 0 n in
            (* construct a flow with this buffer available for reading *)
            let flow = { fd = Some fd; read_buffer_size = 0; already_read = Some data; sockaddr } in
            Lwt.async
              (fun () ->
                Lwt.catch
                  (fun () -> flow_cb flow)
                  (fun e ->
                    Log.info (fun f -> f "Udp.listen callback caught: %s" (Printexc.to_string e));
                    Lwt.return_unit
                  )
              );
            Lwt.return true
          ) (fun e ->
            Log.err (fun f -> f "Udp.listen: caught %s shutting down server" (Printexc.to_string e));
            Lwt.return false
          )
        >>= function
        | false -> Lwt.return_unit
        | true -> loop () in
      Lwt.async loop

end
