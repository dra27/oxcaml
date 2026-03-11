module Name : sig
  type t

  include Identifiable.S with type t := t

  val dummy : t
  val of_string : string -> t
  val to_string : t -> string
  val of_head_of_global_name : Global_module.Name.t -> t
  val of_parameter_name : Global_module.Parameter_name.t -> t
  val to_global_name : t -> Global_module.Name.t
  val predef_exn : t
  val print : Format_doc.formatter -> t -> unit
end

module Prefix : sig
  type t

  include Identifiable.S with type t := t

  val empty : t
  val parse_for_pack : string -> t
  val to_list : t -> Name.t list
  val to_string : t -> string
  val is_empty : t -> bool
  val print : Format_doc.formatter -> t -> unit
end

(** The name of a compilation unit qualified with any "-for-pack" prefix that
    was specified when the unit was compiled. For example if compiling foo.ml
    with "-for-pack Baz.Bar", the corresponding value of type [t] would
    represent "Baz.Bar.Foo", with its [name] representing "Foo" and its [prefix]
    representing "Baz.Bar". *)
type t

include Identifiable.S with type t := t

val print : Format_doc.formatter -> t -> unit

val create : Prefix.t -> Name.t -> t

val to_prefix : t -> Prefix.t

val create_child : t -> Name.t -> t

type argument =
  { param : Name.t;
    value : t
  }

val create_instance : t -> argument list -> t

val to_global_name : t -> Global_module.Name.t option

val to_global_name_exn : t -> Global_module.Name.t

val to_global_name_without_prefix : t -> Global_module.Name.t

val of_complete_global_exn : Global_module.t -> t

val of_string : string -> t

val which_cmx_file : t -> accessed_by:t -> t

val dummy : t

val predef_exn : t

val name : t -> Name.t

val name_as_string : t -> string

val is_plain_name : t -> bool

val equal_to_name : t -> Name.t -> bool

val for_pack_prefix : t -> Prefix.t

val with_for_pack_prefix : t -> Prefix.t -> t

val is_packed : t -> bool

val full_path : t -> Name.t list

val full_path_as_string : t -> string

val flatten : t -> Prefix.t * Name.t * (int * Name.t * Name.t) list

val instance_arguments : t -> argument list

val is_instance : t -> bool

val split_instance_exn : t -> t * argument list

type error =
    Invalid_character of char * string
  | Bad_compilation_unit_name of string
  | Child_of_instance of { parent_name : string; }
  | Packed_instance of { name : string; }
  | Already_an_instance of { name : string; }
exception Error of error
