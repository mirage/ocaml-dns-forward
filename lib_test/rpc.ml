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

type request = Cstruct.t
type response = Cstruct.t
type address = Dns_forward_config.address

type cb = request -> [ `Ok of response | `Error of [ `Msg of string ] ] Lwt.t

type t = {
  mutable cb: cb;
}

let rpc { cb } request = cb request

let disconnect t =
  t.cb <- (fun _ -> Lwt.return (`Error (`Msg "disconnected")));
  Lwt.return_unit

type server = {
  mutable listen_cb: cb;
  address: address;
}
let bound = Hashtbl.create 7

let connect address =
  if Hashtbl.mem bound address then begin
    let cb = (Hashtbl.find bound address).listen_cb in
    Lwt.return (`Ok { cb })
  end else Lwt.return (`Error (`Msg "no bound server"))

let bind address =
  let listen_cb _ = Lwt.return (`Error (`Msg "no callback")) in
  let server = { listen_cb; address } in
  if Hashtbl.mem bound address
  then Lwt.return (`Error (`Msg "address already bound"))
  else begin
    Hashtbl.replace bound address server;
    Lwt.return (`Ok server)
  end
let listen server cb =
  server.listen_cb <- cb;
  Lwt.return (`Ok ())
let shutdown server =
  server.listen_cb <- (fun _ -> Lwt.return (`Error (`Msg "shutdown")));
  Hashtbl.remove bound server.address;
  Lwt.return_unit
