(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   David Allsopp, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Types for Cmo_format.compilation_unit_descr.cu_format and
   Cmo_format.compilation_unit_descr.cu_arg_descr (not used by Dynlink) *)
module Lambda : sig
  type arg_descr
  type main_module_block_format
end = struct
  type arg_descr = unit
  type main_module_block_format = unit
end
