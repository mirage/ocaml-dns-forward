(*
 * Copyright (C) 2017 Docker Inc
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

(* A fake Time and Clock module for testing the timing without having to actually
   wait. *)

let timeofday = ref 0.
let c = Lwt_condition.create ()

let advance nsecs =
  timeofday := !timeofday +. nsecs;
  Lwt_condition.broadcast c ()

let reset () =
  timeofday := 0.;
  Lwt_condition.broadcast c ()

module Time = struct
  type 'a io = 'a Lwt.t

  let sleep n =
    let open Lwt.Infix in
    (* All sleeping is relative to the start of the program for now *)
    let now = 0. in
    let rec loop () =
      if !timeofday > (now +. n) then Lwt.return_unit else begin
        Lwt_condition.wait c
        >>= fun () ->
        loop ()
      end in
    loop ()
end


module Clock = struct

  type tm =
    { tm_sec: int;
      tm_min: int;
      tm_hour: int;
      tm_mday: int;
      tm_mon: int;
      tm_year: int;
      tm_wday: int;
      tm_yday: int;
      tm_isdst: bool;
    }

  let time () = !timeofday

  let gmtime _ = failwith "gmtime unimplemented"
end
