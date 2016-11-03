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

(* Pervasives.compare is ok because the question consists of a record of
   constant constructors and a string list. Ideally ocaml-dns would provide
   nice `compare` functions. *)
module QMap = Map.Make(struct type t = Dns.Packet.question let compare = Pervasives.compare end)

type result = {
  answers: Dns.Packet.rr list;
  (* We'll use the Lwt scheduler as a priority queue to expire records, one
     timeout thread per record. *)
  timeout: unit Lwt.t;
}

module Make(Time: V1_LWT.TIME) = struct
  type t = {
    max_bindings: int;
    mutable cache: result QMap.t;
  }

  let make ?(max_bindings=1024) () =
    let cache = QMap.empty in
    { max_bindings; cache }

  let answer t question =
    if QMap.mem question t.cache
    then Some (QMap.find question t.cache).answers
    else None

  let remove t question =
    if QMap.mem question t.cache then begin
      Lwt.cancel (QMap.find question t.cache).timeout;
      t.cache <- QMap.remove question t.cache;
    end

  let destroy t =
    QMap.iter (fun _ { timeout; _ } -> Lwt.cancel timeout) t.cache;
    t.cache <- QMap.empty

  let insert t question answers =
    (* If there's an existing binding we'll remove it now *)
    remove t question;
    (* If we already have the maximum number of bindings then remove one at
       random *)
    if QMap.cardinal t.cache >= t.max_bindings then begin
      let choice = Random.int (QMap.cardinal t.cache) in
      match QMap.fold (fun question _ (i, existing) ->
        i + 1, if i = choice then Some question else existing
      ) t.cache (0, None) with
      | _, None -> (* should never happen *) ()
      | _, Some question -> remove t question
    end;
    (* We'll expire all records when the lowest TTL is hit to make the code simpler *)
    let min_ttl = List.fold_left (min) Int32.max_int (List.map (fun rr -> rr.Dns.Packet.ttl) answers) in
    let timeout =
      let open Lwt.Infix in
      Time.sleep (Int32.to_float min_ttl)
      >>= fun () ->
      t.cache <- QMap.remove question t.cache;
      Lwt.return_unit in
    t.cache <- QMap.add question { answers; timeout } t.cache
end
