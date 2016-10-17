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
  server_address: address;
}

let rpc { cb; _ } request = cb request

let nr_connects = Hashtbl.create 7

let get_connections () = Hashtbl.fold (fun k v acc -> (k, v) :: acc) nr_connects []

let disconnect t =
  let nr = Hashtbl.find nr_connects t.server_address - 1 in
  if nr = 0 then Hashtbl.remove nr_connects t.server_address else Hashtbl.replace nr_connects t.server_address nr;
  t.cb <- (fun _ -> Lwt.return (`Error (`Msg "disconnected")));
  Lwt.return_unit

type server = {
  mutable listen_cb: cb;
  address: address;
}
let bound = Hashtbl.create 7

let connect address =
  if Hashtbl.mem bound address then begin
    Hashtbl.replace nr_connects address (if Hashtbl.mem nr_connects address then Hashtbl.find nr_connects address else 1);
    let cb = (Hashtbl.find bound address).listen_cb in
    Lwt.return (`Ok { cb; server_address = address })
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
