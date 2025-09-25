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

(* XXX TODO UPDATE-> Type inference for the core language *)

open Asttypes
open Types
open Mode
open Typedtree

type comprehension_type =
  | List_comprehension
  | Array_comprehension of mutability

type type_forcing_context =
  | If_conditional
  | If_no_else_branch
  | While_loop_conditional
  | While_loop_body
  | For_loop_start_index
  | For_loop_stop_index
  | For_loop_body
  | Assert_condition
  | Sequence_left_hand_side
  | When_guard
  | Comprehension_in_iterator of comprehension_type
  | Comprehension_for_start
  | Comprehension_for_stop
  | Comprehension_when
  | Error_message_attr of string

type type_expected = private {
  ty: type_expr;
  explanation: type_forcing_context option;
}

module Datatype_kind : sig
  type t = Record | Record_unboxed_product | Variant
  val type_name : t -> string
  val label_name : t -> string
end

type wrong_name = {
  type_path: Path.t;
  kind: Datatype_kind.t;
  name: string loc;
  valid_names: string list;
}

type wrong_kind_context =
  | Pattern
  | Expression of type_forcing_context option

type wrong_kind_sort =
  | Constructor
  | Record
  | Record_unboxed_product
  | Boolean
  | List
  | Unit

type pattern_variable_kind =
  | Std_var
  | As_var
  | Continuation_var

type pattern_variable =
  {
    pv_id: Ident.t;
    pv_uid: Uid.t;
    pv_mode: Value.l;
    pv_kind : value_kind;
    pv_type: type_expr;
    pv_loc: Location.t;
    pv_as_var: bool;
    pv_attributes: attributes;
    pv_sort: Jkind_types.Sort.t;
  }

val mk_expected:
  ?explanation:type_forcing_context ->
  type_expr ->
  type_expected

type existential_restriction =
  | At_toplevel
  | In_group
  | In_rec
  | With_attributes
  | In_class_args
  | In_class_def
  | In_self_pattern

val type_binding:
       (Env.t -> mutable_flag -> rec_flag ->
         ?force_toplevel:bool ->
          Parsetree.value_binding list ->
          Typedtree.value_binding list * Env.t) ref
val type_let:
       (existential_restriction -> Env.t -> mutable_flag -> rec_flag ->
          Parsetree.value_binding list ->
          Typedtree.value_binding list * Env.t) ref
val type_expression:
       (Env.t -> Parsetree.expression -> Typedtree.expression) ref
val type_class_arg_pattern:
       (string -> Env.t -> Env.t -> arg_label -> Parsetree.pattern ->
        Typedtree.pattern *
        (Ident.t * Ident.t * type_expr) list *
        Env.t * Env.t) ref
val type_self_pattern:
       (Env.t -> Parsetree.pattern ->
        Typedtree.pattern * pattern_variable list) ref
val check_partial:
       (?lev:int -> Env.t -> type_expr ->
        Location.t -> Typedtree.value Typedtree.case list -> Typedtree.partial) ref
val type_expect:
       (Env.t -> ?mode:Mode.Value.r -> Parsetree.expression -> type_expected -> Typedtree.expression) ref
val type_exp:
       (Env.t -> ?mode: Mode.Value.r -> Parsetree.expression -> Typedtree.expression) ref

val type_approx:
       (Env.t -> Parsetree.expression -> type_expr -> unit) ref
val type_argument:
       (Env.t -> Parsetree.expression ->
        type_expr -> type_expr -> Typedtree.expression) ref

val type_option_some:
        (Env.t -> Parsetree.expression ->
        type_expr-> type_expr -> Typedtree.expression) ref
val type_option_none:
        (Env.t -> type_expr -> Location.t -> Typedtree.expression) ref
val extract_option_type: (Env.t -> type_expr -> type_expr) ref
val generalizable: (int -> type_expr -> bool) ref
val generalize_structure_exp: (Typedtree.expression -> unit) ref
val reset_allocations: (unit -> unit) ref
val optimise_allocations: (unit -> unit) ref
val reset_delayed_checks: (unit -> unit) ref
val force_delayed_checks: (unit -> unit) ref

type existential_binding =
  | Bind_already_bound
  | Bind_not_in_scope
  | Bind_non_locally_abstract

type mutable_restriction =
  | In_group
  | In_rec

type submode_reason =
  | Application of type_expr
  | Constructor of Longident.t
  | Other

type unsupported_stack_allocation =
  | Lazy
  | Module
  | Object
  | List_comprehension
  | Array_comprehension

type error =
  | Constructor_arity_mismatch of Longident.t * int * int
  | Constructor_labeled_arg
  | Partial_tuple_pattern_bad_type
  | Extra_tuple_label of string option * type_expr
  | Missing_tuple_label of string option * type_expr
  | Label_mismatch of
      record_form_packed * Longident.t * Errortrace.unification_error
  | Pattern_type_clash :
      Errortrace.unification_error * Parsetree.pattern_desc option -> error
  | Or_pattern_type_clash of Ident.t * Errortrace.unification_error
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of
      Errortrace.unification_error * type_forcing_context option
      * Parsetree.expression_desc option
  | Function_arity_type_clash of
      { syntactic_arity :  int;
        type_constraint : type_expr;
        trace : Errortrace.unification_error;
      }
  (* [Function_arity_type_clash { syntactic_arity = n; type_constraint; trace }]
     is the type error for the specific case where an n-ary function is
     constrained at a type with an arity less than n, e.g.:
     {[
       type (_, _) eq = Eq : ('a, 'a) eq
       let bad : type a. ?opt:(a, int -> int) eq -> unit -> a =
         fun ?opt:(Eq = assert false) () x -> x + 1
     ]}

     [type_constraint] is the user-written polymorphic type (in this example
     [?opt:(a, int -> int) eq -> unit -> a]) that causes this type clash, and
     [trace] is the unification error that signaled the issue.
  *)
  | Apply_non_function of {
      funct : Typedtree.expression;
      func_ty : type_expr;
      res_ty : type_expr;
      previous_arg_loc : Location.t;
      extra_arg_loc : Location.t;
    }
  | Apply_wrong_label of arg_label * type_expr * bool
  | Label_multiply_defined of string
  | Label_missing of record_form_packed * Ident.t list
  | Label_not_mutable of Longident.t
  | Wrong_name of string * type_expected * wrong_name
  | Name_type_mismatch of
      Datatype_kind.t * Longident.t * (Path.t * Path.t) * (Path.t * Path.t) list
  | Invalid_format of string
  | Not_an_object of type_expr * type_forcing_context option
  | Non_value_object of Jkind.Violation.t * type_forcing_context option
  | Non_value_let_rec of Jkind.Violation.t * type_expr
  | Undefined_method of type_expr * string * string list option
  | Undefined_self_method of string * string list
  | Virtual_class of Longident.t
  | Private_type of type_expr
  | Private_label of Longident.t * type_expr
  | Private_constructor of constructor_description * type_expr
  | Unbound_instance_variable of string * string list
  | Instance_variable_not_mutable of string
  | Not_subtype of Errortrace.Subtype.error
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of
      Errortrace.expanded_type * Errortrace.unification_error * bool
  | Not_a_function of type_expr * type_forcing_context option
  | Too_many_arguments of type_expr * type_forcing_context option
  | Abstract_wrong_label of
      { got           : arg_label
      ; expected      : arg_label
      ; expected_type : type_expr
      ; explanation   : type_forcing_context option
      }
  | Scoping_let_module of string * type_expr
  | Not_a_polymorphic_variant_type of Longident.t
  | Incoherent_label_order
  | Less_general of string * Errortrace.unification_error
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module of type_expr
  | Unexpected_existential of existential_restriction * string
  | Unexpected_mutable of mutable_restriction
  | Invalid_interval
  | Invalid_for_loop_index
  | Invalid_comprehension_for_range_iterator_index
  | No_value_clauses
  | Exception_pattern_disallowed
  | Mixed_value_and_exception_patterns_under_guard
  | Inlined_record_escape
  | Inlined_record_expected
  | Unrefuted_pattern of pattern
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Probe_format
  | Probe_name_format of string
  | Probe_name_undefined of string
  | Probe_is_enabled_format
  | Extension_not_enabled : _ Language_extension.t -> error
  | Atomic_in_pattern of Longident.t
  | Invalid_atomic_loc_payload
  | Label_not_atomic of Longident.t
  | Modalities_on_atomic_field of Longident.t
  | Literal_overflow of string
  | Unknown_literal of string * char
  | Float32_literal of string
  | Int8_literal of string
  | Int16_literal of string
  | Untagged_char_literal of char
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_mutable_pat
  | Illegal_class_expr
  | Letop_type_clash of string * Errortrace.unification_error
  | Andop_type_clash of string * Errortrace.unification_error
  | Bindings_type_clash of Errortrace.unification_error
  | Unbound_existential of Ident.t list * type_expr
  | Missing_type_constraint
  | Wrong_expected_kind of wrong_kind_sort * wrong_kind_context * type_expr
  | Wrong_expected_record_boxing of
      wrong_kind_context * record_form_packed * type_expr
  | Expr_not_a_record_type of record_form_packed * type_expr
  | Expr_record_type_has_wrong_boxing of record_form_packed * type_expr
  | Invalid_unboxed_access of
      { prev_el_type : type_expr; ua : Parsetree.unboxed_access }
  | Block_access_record_unboxed
  | Block_access_private_record
  | Block_index_modality_mismatch of
      { mut : bool; err : Modality.equate_error }
  | Submode_failed of Value.error * submode_reason * Env.shared_context option
  | Submode_failed_alloc of Alloc.error
  | Curried_application_complete of
      arg_label * Mode.Alloc.error * [`Prefix|`Single_arg|`Entire_apply]
  | Param_mode_mismatch of Alloc.equate_error
  | Uncurried_function_escapes of Alloc.error
  | Local_return_annotation_mismatch of Location.t
  | Function_returns_local
  | Tail_call_local_returning
  | Bad_tail_annotation of [`Conflict|`Not_a_tailcall]
  | Optional_poly_param
  | Exclave_in_nontail_position
  | Exclave_returns_not_local
  | Unboxed_int_literals_not_supported
  | Function_type_not_rep of type_expr * Jkind.Violation.t
  | Record_projection_not_rep of type_expr * Jkind.Violation.t
  | Record_not_rep of type_expr * Jkind.Violation.t
  | Mutable_var_not_rep of type_expr * Jkind.Violation.t
  | Invalid_label_for_src_pos of arg_label
  | Nonoptional_call_pos_label of string
  | Unsupported_stack_allocation of unsupported_stack_allocation
  | Not_allocation
  | Impossible_function_jkind of
      { some_args_ok : bool; ty_fun : type_expr; jkind : jkind_lr }
  | Overwrite_of_invalid_term
  | Unexpected_hole

exception Error of Location.t * Env.t * error

val has_poly_constraint : (Parsetree.pattern -> bool) ref

val name_pattern : string -> Typedtree.pattern list -> Ident.t * Uid.t
val name_cases : string -> Typedtree.value Typedtree.case list -> Ident.t * Uid.t

type escape_fn =
  {mutable escape: 'r. loc:Location.t -> env:Env.t -> reason:submode_reason -> (Mode.allowed * 'r) Mode.Value.t -> unit}

val escape : escape_fn

val self_coercion : (Path.t * Location.t list ref) list ref

val annotate_recursive_bindings :
  (Env.t -> Typedtree.value_binding list -> Typedtree.value_binding list) ref
val check_recursive_class_bindings :
  (Env.t -> Ident.t list -> Typedtree.class_expr list -> unit) ref

(* Forward declaration, to be filled in by Typemod.type_module *)
val type_module:
  (Env.t -> Parsetree.module_expr -> Typedtree.module_expr * Shape.t) ref
(* Forward declaration, to be filled in by Typemod.type_open *)
val type_open:
  (?used_slot:bool ref -> override_flag -> Env.t -> Location.t ->
   Longident.t loc -> Path.t * Env.t)
    ref
(* Forward declaration, to be filled in by Typemod.type_open_decl *)
val type_open_decl:
  (?used_slot:bool ref -> Env.t -> Parsetree.open_declaration ->
   Typedtree.open_declaration * Env.t)
    ref
(* Forward declaration, to be filled in by Typeclass.class_structure *)
val type_object:
  (Env.t -> Location.t -> Parsetree.class_structure ->
   Typedtree.class_structure * string list) ref
val type_package:
  (Env.t -> Parsetree.module_expr -> Path.t -> (Longident.t * type_expr) list ->
  Typedtree.module_expr * (Longident.t * type_expr) list) ref

val src_pos : Location.t -> Typedtree.attributes -> Env.t -> Typedtree.expression

val type_representable_expression:
       (why:Jkind.History.concrete_creation_reason ->
        Env.t -> Parsetree.expression -> Typedtree.expression * Jkind.sort) ref
