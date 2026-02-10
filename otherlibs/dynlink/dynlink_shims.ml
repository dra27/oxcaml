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

module Config = Dynlink_config

(* Types for Cmo_format.compilation_unit_descr.cu_format and
   Cmo_format.compilation_unit_descr.cu_arg_descr (not used by Dynlink) *)
module Lambda : sig
  type arg_descr
  type main_module_block_format
end = struct
  type arg_descr = unit
  type main_module_block_format = unit
end

module Name = struct
    type t = string

    let of_string t = t
    let to_string t = t
  end

module Linkage_name = Name

module Compilation_unit = struct
  module Name = Name

  (* cf. Compilation_unit.t, which is the pseudo-type:
     type t =
     | Bare_name of Name.t [@@unboxed]
     | With_prefix of
         { name : Name.t;
           for_pack_prefix: Name.t list
         }
     | Global of
         { head : Name.t
           args : Global_module.Name.argument list
         }
   *)
  type t = Obj.t

  let full_path_as_string t =
    let tag = Obj.tag t in
    assert (tag < 2 || tag = Obj.string_tag);
    if tag = Obj.string_tag then
      Sys.opaque_identity (Obj.obj t : Name.t)
    else
      let name = Sys.opaque_identity (Obj.obj (Obj.field t 0) : Name.t) in
      let prefix_or_args = Obj.field t 1 in
      if Obj.is_int prefix_or_args then
        name
      else begin
        (* CR dallsopp: not sure this is true: either With_prefix _ or
            Global {args = []} *)
        assert (tag = 0);
        String.concat "." (Obj.obj prefix_or_args : Name.t list) ^ "." ^ name
      end

  module Prefix = struct
    let empty = ()
  end

  let create _prefix name = Obj.repr name
end

module Import_info = struct
  module Intf = struct
    module Nonalias = struct
      module Kind = struct
        type t =
        | Normal of Compilation_unit.t
        | Parameter
      end

      type t = Kind.t * Digest.t
    end
  end
end
