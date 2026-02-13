(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

(* Upstream compatible usages of immediate/immediate64 are allowed *)
module type S1 = sig
  type t_immediate : immediate
  type t_immediate64 : immediate64
end;;
[%%expect {|
module type S1 =
  sig type t_immediate : immediate type t_immediate64 : immediate64 end
|}];;

(* Same is true when constraining type vars *)
(* immediate *)
module type S = sig
  val f_immediate : ('a : immediate). 'a -> 'a -> 'a
end;;
[%%expect {|
module type S = sig val f_immediate : ('a : immediate). 'a -> 'a -> 'a end
|}];;


(* Other annotations are not affected by this flag *)
module type S = sig
  val f_any : ('a : any). ('a : any) -> (('a : any)[@error_message ""])
  type ('a : any) t_any : any
  type (_ : any) t_any_ = MkG : ('a : any). 'a t_any_
  val f_bits64 : ('a : bits64). ('a : bits64) -> (('a : bits64)[@error_message ""])
  type ('a : bits64) t_bits64 : bits64
  type (_ : bits64) t_bits64_ = MkG : ('a : bits64). 'a t_bits64_
  val f_bits32 : ('a : bits32). ('a : bits32) -> (('a : bits32)[@error_message ""])
  type ('a : bits32) t_bits32 : bits32
  type (_ : bits32) t_bits32_ = MkG : ('a : bits32). 'a t_bits32_
  val f_float64 : ('a : float64). ('a : float64) -> (('a : float64)[@error_message ""])
  type ('a : float64) t_float64 : float64
  type (_ : float64) t_float64_ = MkG : ('a : float64). 'a t_float64_
  val f_word : ('a : word). ('a : word) -> (('a : word)[@error_message ""])
  type ('a : word) t_word : word
  type (_ : word) t_word_ = MkG : ('a : word). 'a t_word_
end

module M = struct
  let f_any (type a : any) = ()
  let f_bits64 (type a : bits64) = ()
  let f_bits32 (type a : bits32) = ()
  let f_float64 (type a : float64) = ()
  let f_word (type a : word) = ()
end
[%%expect {|
module type S =
  sig
    val f_any : ('a : any). 'a -> 'a
    type ('a : any) t_any : any
    type (_ : any) t_any_ = MkG : ('a : any). 'a t_any_
    val f_bits64 : ('a : bits64). 'a -> 'a
    type ('a : bits64) t_bits64 : bits64
    type (_ : bits64) t_bits64_ = MkG : ('a : bits64). 'a t_bits64_
    val f_bits32 : ('a : bits32). 'a -> 'a
    type ('a : bits32) t_bits32 : bits32
    type (_ : bits32) t_bits32_ = MkG : ('a : bits32). 'a t_bits32_
    val f_float64 : ('a : float64). 'a -> 'a
    type ('a : float64) t_float64 : float64
    type (_ : float64) t_float64_ = MkG : ('a : float64). 'a t_float64_
    val f_word : ('a : word). 'a -> 'a
    type ('a : word) t_word : word
    type (_ : word) t_word_ = MkG : ('a : word). 'a t_word_
  end
module M :
  sig
    val f_any : unit
    val f_bits64 : unit
    val f_bits32 : unit
    val f_float64 : unit
    val f_word : unit
  end
|}];;

(* Externals *)

external f_1 : int -> bool -> int64# = "foo" "bar";;
[%%expect{|
Line 1, characters 30-36:
1 | external f_1 : int -> bool -> int64# = "foo" "bar";;
                                  ^^^^^^
Warning 187 [incompatible-with-upstream]: [@unboxed] attribute must be added to external declaration
argument type with layout bits64 for upstream compatibility.

external f_1 : int -> bool -> (int64# [@unboxed]) = "foo" "bar"
|}];;

external f_2 : int32# -> bool -> int = "foo" "bar";;
[%%expect{|
Line 1, characters 15-21:
1 | external f_2 : int32# -> bool -> int = "foo" "bar";;
                   ^^^^^^
Warning 187 [incompatible-with-upstream]: [@unboxed] attribute must be added to external declaration
argument type with layout bits32 for upstream compatibility.

external f_2 : (int32# [@unboxed]) -> bool -> int = "foo" "bar"
|}];;

external f_3 : (float#[@unboxed]) -> bool -> string  = "foo" "bar";;
[%%expect{|
external f_3 : (float# [@unboxed]) -> bool -> string = "foo" "bar"
|}];;

external f_4 : string -> (nativeint#[@unboxed])  = "foo" "bar";;
[%%expect{|
external f_4 : string -> (nativeint# [@unboxed]) = "foo" "bar"
|}];;

external f_5 : int64 -> int64#  = "foo" "bar" [@@unboxed];;
[%%expect{|
external f_5 : int64 -> int64# = "foo" "bar" [@@unboxed]
|}];;

external f_6 : (int32#[@untagged]) -> bool -> string  = "foo" "bar";;
[%%expect{|
Line 1, characters 16-22:
1 | external f_6 : (int32#[@untagged]) -> bool -> string  = "foo" "bar";;
                    ^^^^^^
Error: Don't know how to untag this type. Only "int8", "int16", "int", and
       other immediate types can be untagged.
|}];;

external f_7 : string -> (int64#[@untagged])  = "foo" "bar";;
[%%expect{|
Line 1, characters 26-32:
1 | external f_7 : string -> (int64#[@untagged])  = "foo" "bar";;
                              ^^^^^^
Error: Don't know how to untag this type. Only "int8", "int16", "int", and
       other immediate types can be untagged.
|}];;

(* Aliases *)

type ('a : any) int64' = int64
type int32' = int32
type float' = float
type 'a nativeint' = nativeint
type nativeint'' = int nativeint'
[%%expect{|
type ('a : any) int64' = int64
type int32' = int32
type float' = float
type 'a nativeint' = nativeint
type nativeint'' = int nativeint'
|}]

external f_1 : int -> bool -> int int64'# = "foo" "bar";;
[%%expect{|
Line 1, characters 30-41:
1 | external f_1 : int -> bool -> int int64'# = "foo" "bar";;
                                  ^^^^^^^^^^^
Warning 187 [incompatible-with-upstream]: [@unboxed] attribute must be added to external declaration
argument type with layout bits64 for upstream compatibility.

external f_1 : int -> bool -> (int int64'# [@unboxed]) = "foo" "bar"
|}];;

external f_2 : int32'# -> bool -> int = "foo" "bar";;
[%%expect{|
Line 1, characters 15-22:
1 | external f_2 : int32'# -> bool -> int = "foo" "bar";;
                   ^^^^^^^
Warning 187 [incompatible-with-upstream]: [@unboxed] attribute must be added to external declaration
argument type with layout bits32 for upstream compatibility.

external f_2 : (int32'# [@unboxed]) -> bool -> int = "foo" "bar"
|}];;

external f_3 : (float'#[@unboxed]) -> bool -> string  = "foo" "bar";;
[%%expect{|
external f_3 : (float'# [@unboxed]) -> bool -> string = "foo" "bar"
|}];;

external f_4 : string -> (nativeint''#[@unboxed])  = "foo" "bar";;
[%%expect{|
external f_4 : string -> (nativeint''# [@unboxed]) = "foo" "bar"
|}];;

external f_4b : string -> (int nativeint'#[@unboxed])  = "foo" "bar";;
[%%expect{|
external f_4b : string -> (int nativeint'# [@unboxed]) = "foo" "bar"
|}];;

external f_5 : int64 -> string int64'#  = "foo" "bar" [@@unboxed];;
[%%expect{|
external f_5 : int64 -> string int64'# = "foo" "bar" [@@unboxed]
|}];;

external f_5b : int64 -> (string int64'# [@unboxed])  = "foo" "bar";;
[%%expect{|
external f_5b : int64 -> (string int64'# [@unboxed]) = "foo" "bar"
|}];;

external f_6 : (int32'[@untagged]) -> bool -> string  = "foo" "bar";;
[%%expect{|
Line 1, characters 16-22:
1 | external f_6 : (int32'[@untagged]) -> bool -> string  = "foo" "bar";;
                    ^^^^^^
Error: Don't know how to untag this type. Only "int8", "int16", "int", and
       other immediate types can be untagged.
|}];;

external f_7 : string -> (int64# int64'#[@untagged])  = "foo" "bar";;
[%%expect{|
Line 1, characters 26-40:
1 | external f_7 : string -> (int64# int64'#[@untagged])  = "foo" "bar";;
                              ^^^^^^^^^^^^^^
Error: Don't know how to untag this type. Only "int8", "int16", "int", and
       other immediate types can be untagged.
|}];;

(* With [@layout_poly] *)

external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
|}];;


external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity" [@@unboxed]
[%%expect{|
Line 1, characters 40-42:
1 | external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity" [@@unboxed]
                                            ^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}];;


external[@layout_poly] id : ('a : any). ('a[@unboxed]) -> 'a = "%identity"
[%%expect{|
Line 1, characters 41-43:
1 | external[@layout_poly] id : ('a : any). ('a[@unboxed]) -> 'a = "%identity"
                                             ^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}];;

(* module and abstract types *)
module M : sig
  type t : float64
end = struct
  type t = float#
end

external f_1 : M.t -> M.t = "%identity";;
[%%expect{|
module M : sig type t : float64 end
Line 7, characters 15-18:
7 | external f_1 : M.t -> M.t = "%identity";;
                   ^^^
Warning 187 [incompatible-with-upstream]: [@unboxed] attribute must be added to external declaration
argument type with layout float64 for upstream compatibility.

Line 7, characters 22-25:
7 | external f_1 : M.t -> M.t = "%identity";;
                          ^^^
Warning 187 [incompatible-with-upstream]: [@unboxed] attribute must be added to external declaration
argument type with layout float64 for upstream compatibility.

external f_1 : M.t -> M.t = "%identity" [@@unboxed]
|}];;

external f_2 : M.t -> M.t = "%identity" [@@unboxed];;
[%%expect{|
Line 1, characters 15-18:
1 | external f_2 : M.t -> M.t = "%identity" [@@unboxed];;
                   ^^^
Warning 187 [incompatible-with-upstream]: External declaration here is not upstream compatible.
The only types with non-value layouts allowed are float#,
int32#, int64#, and nativeint#. Unknown type with layout
float64 encountered.

Line 1, characters 22-25:
1 | external f_2 : M.t -> M.t = "%identity" [@@unboxed];;
                          ^^^
Warning 187 [incompatible-with-upstream]: External declaration here is not upstream compatible.
The only types with non-value layouts allowed are float#,
int32#, int64#, and nativeint#. Unknown type with layout
float64 encountered.

external f_2 : M.t -> M.t = "%identity" [@@unboxed]
|}];;

module M2 : sig
  type t = float#
end = struct
  type t = float#
end

external f_3 : M2.t -> M2.t = "%identity" [@@unboxed];;
[%%expect{|
module M2 : sig type t = float# end
external f_3 : M2.t -> M2.t = "%identity" [@@unboxed]
|}];;

(* should also work with private types *)
module M3 : sig
  type t = private float#
end = struct
  type t = float#
end

external f_4 : M3.t -> M3.t = "%identity" [@@unboxed]
[%%expect{|
module M3 : sig type t = private float# end
external f_4 : M3.t -> M3.t = "%identity" [@@unboxed]
|}];;

(* Disabling warnings *)

module M4 : sig
  type ('a : immediate) t = Something of 'a

  val f : ('a : immediate). 'a t -> 'a
end = struct
  type ('a : immediate) t = Something of 'a

  let f (Something x) = x
end;;

[%%expect{|
module M4 :
  sig
    type ('a : immediate) t = Something of 'a
    val f : ('a : immediate). 'a t -> 'a
  end
|}]

module M5 = struct
  let f (type a : immediate): a -> a = fun x -> x
end;;

[%%expect{|
module M5 : sig val f : ('a : immediate). 'a -> 'a end
|}]

(* Just disabling the warning on the expression level doesn't work
   if the declaration has a type variable annotation. *)

let fails (type a : immediate): a -> a = fun x -> x
;;

[%%expect{|
val fails : ('a : immediate). 'a -> 'a = <fun>
|}]

module type S1 = sig
  type ('a : immediate) fails = int
end;;

[%%expect{|
module type S1 = sig type ('a : immediate) fails = int end
|}]

(* Disabling the warning just in the signature isn't sufficient. *)
module M6 : sig
  type ('a : immediate) t = 'a * 'a
end = struct
  type ('a : immediate) t = 'a * 'a
end;;
[%%expect{|
module M6 : sig type ('a : immediate) t = 'a * 'a end
|}]

(* More disabled warnings. *)
external[@warning "-187"] f_ok : int -> bool -> int64# = "foo" "bar";;

[%%expect{|
external f_ok : int -> bool -> (int64# [@unboxed]) = "foo" "bar"
|}]

external f_2_ok : M.t -> M.t = "%identity" [@@unboxed] [@@warning "-187"];;

[%%expect{|
external f_2_ok : M.t -> M.t = "%identity" [@@unboxed]
|}]
