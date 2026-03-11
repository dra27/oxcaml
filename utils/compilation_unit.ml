(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Pierrick Couderc, OCamlPro               *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-40-41-42"]

module List = Misc.Stdlib.List
module Fmt = Format_doc

include Compilation_unit0

module Name = struct
  include Compilation_unit0.Name

  let print_as_inline_code ppf t = Misc.Style.inline_code ppf (to_string t)
end

module Prefix = struct
  include Compilation_unit0.Prefix

  let from_clflags () =
    match !Clflags.for_package with
    | None -> empty
    | Some pack -> parse_for_pack pack

end

let base_filename t =
  (* This is a one-way function. Please don't parse anything out of it. Consider
     the following formatting details to be a state secret (shared only with
     Dune). *)
  let _prefix, name, arguments = flatten t in
  let arg_segments =
    ListLabels.map arguments ~f:(fun (depth, _param, value) ->
        (* Dropping the parameter names in the hopes of keeping the filenames
           _extremely_ long rather than _excruciatingly_ long. I will not be
           surprised if we decide that we're better off with the parameter names
           because the filenames are beyond hope anyway. *)
        String.make (depth + 1) '-' ^ (value |> Name.to_string))
  in
  String.concat "" ((name |> Name.to_string) :: arg_segments)
  |> String.uncapitalize_ascii

let instance_separator = "____"

let instance_separator_depth_char = '_'

let mangle_for_linkage_name ~pack_separator t =
  (* Returns a string to be part of the linkage name, not the full linkage name
     yet; see {!Symbol.linkage_name_for_compilation_unit} for the resulting
     linkage name. *)
  (* CR-someday lmaurer: If at all possible, just use square brackets instead of
     this unholy underscore encoding. For now I'm following the original
     practice of avoiding non-identifier characters. *)
  let for_pack_prefix, name, flattened_instance_args = flatten t in
  let name = Name.to_string name in
  let suffix =
    if not (Prefix.is_empty for_pack_prefix)
    then begin
      assert (match flattened_instance_args with [] -> true | _ -> false);
      let pack_names =
        Prefix.to_list for_pack_prefix |> List.map Name.to_string
      in
      String.concat (pack_separator ()) (pack_names @ [name])
    end
    else begin
      let arg_segments =
        List.map
          (fun (depth, _param, value) ->
            let extra_separators =
              String.make depth instance_separator_depth_char
            in
            let value = value |> Name.to_string in
            String.concat "" [instance_separator; extra_separators; value])
          flattened_instance_args
      in
      String.concat "" arg_segments
    end
  in
  (* Note that [name] is prepended unconditionnally here, so it ends up being
     duplicated in the case of a [-for-pack] prefix, as it appears both at the
     beginning and at the end of the mangled name. This differs from the
     upstream compiler, which doesn't add it at the beginning. *)
  name ^ suffix

let is_parent t ~child =
  List.equal Name.equal (full_path t) (Prefix.to_list (for_pack_prefix child))

let is_strict_prefix list1 ~of_:list2 ~equal =
  (not (List.equal equal list1 list2)) && List.is_prefix list1 ~of_:list2 ~equal

let can_access_by_name t ~accessed_by:me =
  let my_path = full_path me in
  (* Criterion 1 in .mli *)
  let t's_prefix_is_my_ancestor =
    List.is_prefix
      (for_pack_prefix t |> Prefix.to_list)
      ~of_:my_path ~equal:Name.equal
  in
  (* Criterion 2 *)
  let t_is_not_my_strict_ancestor =
    not (is_strict_prefix (full_path t) ~of_:my_path ~equal:Name.equal)
  in
  t's_prefix_is_my_ancestor && t_is_not_my_strict_ancestor

let can_access_cmx_file = can_access_by_name

let print_name ppf t = Fmt.fprintf ppf "%a" Name.print (name t)

let print_debug ppf t =
  let name = name t in
  let for_pack_prefix = for_pack_prefix t in
  if Prefix.is_empty for_pack_prefix
  then Fmt.fprintf ppf "@[<hov 1>(@[<hov 1>(id@ %a)@])@]" Name.print name
  else
    Fmt.fprintf ppf
      "@[<hov 1>(@[<hov 1>(for_pack_prefix@ %a)@]@;@[<hov 1>(name@ %a)@]"
      Prefix.print for_pack_prefix Name.print name

let print_as_inline_code = Misc.Style.as_inline_code print

let fwd_get_current : (unit -> t option) ref = ref (fun () -> assert false)

let get_current () = !fwd_get_current ()

let get_current_or_dummy () = Option.value (get_current ()) ~default:dummy

let get_current_exn () =
  match get_current () with
  | Some t -> t
  | None -> Misc.fatal_error "No compilation unit set"

let is_current t =
  match get_current () with None -> false | Some t' -> equal t t'

module Private = struct
  let fwd_get_current = fwd_get_current
end
