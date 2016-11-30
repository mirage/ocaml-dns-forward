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

type 'a t = ('a, [ `Msg of string ]) Lwt_result.t

module FromFlowError(Flow: V1_LWT.FLOW): sig
  val ( >>= ) :
    ('a V1.Flow.or_eof, Rresult.R.msg) Result.result Lwt.t ->
    ('a -> ('b, Rresult.R.msg as 'c) Result.result Lwt.t) ->
    ('b, 'c) Result.result Lwt.t

end

val errorf: ('a, unit, string, ('b, [> `Msg of string ]) result Lwt.t) format4 -> 'a

module Infix: sig
  include module type of Lwt_result.Infix
end
