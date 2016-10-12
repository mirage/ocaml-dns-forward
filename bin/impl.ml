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

let read_lines filename =
  Lwt.catch
    (fun () ->
      Lwt_io.open_file filename ~mode:Lwt_io.input
      >>= fun ic ->
      Lwt.finalize
        (fun () ->
          let s = Lwt_io.read_lines ic in
          Lwt_stream.to_list s
        ) (fun () ->
          Lwt_io.close ic
        )
    ) (function
      | Unix.Unix_error(Unix.ENOENT, _, _) as e ->
        Logs.err (fun f -> f "failed to find file %s" filename);
        Lwt.fail e
      )

let max_udp_length = 65507

let serve port filename =
  if filename = "" then begin
    `Error (true, "please supply the name of a config file")
  end else Lwt_main.run begin
    read_lines filename
    >>= fun lines ->
    let all = String.concat "" lines in
    let _config = Dns_forward_config.t_of_sexp @@ Sexplib.Sexp.of_string all in
    let udp = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    Lwt_unix.bind udp (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port));
    let buf = Cstruct.create max_udp_length in
    let bytes = Bytes.make max_udp_length '\000' in
    let rec loop () =
      Lwt_unix.recvfrom udp bytes 0 (Bytes.length bytes) []
      >>= fun (n, _) ->
      Cstruct.blit_from_bytes bytes 0 buf 0 n;
      let request = Cstruct.sub buf 0 n in
      Cstruct.hexdump request;
      loop () in
    loop ()
    >>= fun () ->
    Lwt.return (`Ok ())
  end
