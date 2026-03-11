(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Miscellaneous useful types and functions. This module is re-exported in
    {!Misc}.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val fatal_error: string -> 'a
val fatal_errorf: ('a, Format.formatter, unit, 'b) format4 -> 'a
val fatal_errorf_doc: ('a, Format_doc.formatter, unit, 'b) format4 -> 'a
exception Fatal_error

module Stdlib : sig
  module List : sig
    val merge_iter
       : cmp:('a -> 'b -> int)
      -> left_only:('a -> unit)
      -> right_only:('b -> unit)
      -> both:('a -> 'b -> unit)
      -> 'a list
      -> 'b list
      -> unit

    val merge_fold
      : cmp:('a -> 'b -> int)
      -> left_only:('acc -> 'a -> 'acc)
      -> right_only:('acc -> 'b -> 'acc)
      -> both:('acc -> 'a -> 'b -> 'acc)
      -> init:'acc
      -> 'a list
      -> 'b list
      -> 'acc
  end
end
