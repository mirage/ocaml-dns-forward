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
open Sexplib.Std

module Address = struct
  module M = struct
    type t = {
      ip: Ipaddr.t;
      port: int;
    } [@@deriving sexp]

    let compare a b =
      let ip = Ipaddr.compare a.ip b.ip in
      if ip <> 0 then ip else Pervasives.compare a.port b.port
  end
  include M
  module Set = Set.Make(M)
  module Map = Map.Make(M)
end

module Domain = struct
  module M = struct
    type t = string list [@@deriving sexp]
    let compare (a: t) (b: t) = Pervasives.compare a b
  end
  include M
  module Set = struct
    include Set.Make(M)
    type _t = M.t list [@@deriving sexp]
    let t_of_sexp (sexp: Sexplib.Type.t) : t =
      let _t = _t_of_sexp sexp in
      List.fold_left (fun set elt -> add elt set) empty _t
    let sexp_of_t (t: t) : Sexplib.Type.t =
      let _t = fold (fun elt acc -> elt :: acc) t [] in
      sexp_of__t _t
  end
  module Map = Map.Make(M)
end

module Server = struct
  module M = struct
    type t = {
      zones: Domain.Set.t;
      address: Address.t;
    } [@@deriving sexp]

    let compare (a: t) (b: t) =
      let address = Address.compare a.address b.address in
      if address <> 0 then address else Domain.Set.compare a.zones a.zones
  end
  include M
  module Set = Set.Make(M)
  module Map = Map.Make(M)
end

type t = Server.t list [@@deriving sexp]
