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

module Make(Server: Dns_forward_s.RPC_SERVER): sig
  type t
  (** A DNS server for testing *)

  val make: (string * Ipaddr.t) list -> t
  (** Construct a server with a fixed set of name mappings *)

  val serve: address:Dns_forward_config.address -> t -> [ `Ok of unit | `Error of [ `Msg of string ] ] Lwt.t
  (** Serve requests on the given IP and port forever *)

  val get_nr_queries: t -> int
  (** Return the number of queries which reached this server *)
end
