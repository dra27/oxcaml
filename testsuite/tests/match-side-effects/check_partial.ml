(* TEST
 flags = "-dlambda";
 expect;
*)

(* This test exercises pattern-matching examples that mix mutable
   state with code execution (through guards or lazy patterns). Some
   of those tests appear to be exhaustive to the type-checker but are
   in fact not exhaustive, forcing the pattern-matching compiler to
   add Match_failure clauses for soundness. The pattern-matching
   compiler also sometimes conservatively add Match_failure clauses in
   cases that were in fact exhaustive.
*)

type _ t =
  | Int : int -> int t
  | True : bool t
  | False : bool t

let lazy_total : _ * bool t -> int = function
  | ({ contents = _ }, True) -> 0
  | ({ contents = lazy () }, False) -> 12
(* This pattern-matching is in fact total: a Match_failure case is
   not necessary for soundness. *)
[%%expect {|
0
type _ t = Int : int -> int t | True : bool t | False : bool t
(let
  (lazy_total/289 =
     (function {nlocal = 0}
       param/291[value<
                  (consts ())
                   (non_consts ([0: *,
                                 value<
                                  (consts (1 0))
                                   (non_consts ([0: value<int>]))>]))>]
       : int
       (let
         (*match*/293 =o? (field_mut 0 (field_imm 0 param/291))
          *match*/294 =a? (field_imm 1 param/291))
         (if (isint *match*/294)
           (if *match*/294
             (let
               (*match*/301 =?
                  (let (tag/296 =a[value<int>] (caml_obj_tag *match*/293))
                    (if (%int_equal tag/296 250) (field_mut 0 *match*/293)
                      (if
                        (|| (%int_equal tag/296 246)
                          (%int_equal tag/296 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/293) never_inline)
                        *match*/293))))
               12)
             0)
           (raise (makeblock 0 (getpredef Match_failure/49!!) [0: "" 6 37]))))))
  (apply (field_imm 1 (global Toploop!)) "lazy_total" lazy_total/289))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}];;

let lazy_needs_partial : _ * bool t ref -> int = function
  | (_, { contents = True }) -> 0
  | (lazy (), { contents = False }) -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (lazy_needs_partial/303 =
     (function {nlocal = 0}
       param/305[value<(consts ()) (non_consts ([0: *, *]))>] : int
       (catch
         (let
           (*match*/306 =a? (field_imm 0 param/305)
            *match*/308 =o? (field_mut 0 (field_imm 1 param/305)))
           (if (isint *match*/308)
             (if *match*/308
               (let
                 (*match*/311 =?
                    (let (tag/310 =a[value<int>] (caml_obj_tag *match*/306))
                      (if (%int_equal tag/310 250) (field_mut 0 *match*/306)
                        (if
                          (|| (%int_equal tag/310 246)
                            (%int_equal tag/310 244))
                          (apply (field_imm 1 (global CamlinternalLazy!))
                            (opaque *match*/306) never_inline)
                          *match*/306)))
                  *match*/313 =o? (field_mut 0 (field_imm 1 param/305)))
                 (if (isint *match*/313) (if *match*/313 12 (exit 3))
                   (exit 3)))
               0)
             (exit 3)))
        with (3)
         (raise (makeblock 0 (getpredef Match_failure/49!!) [0: "" 1 49])))))
  (apply (field_imm 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/303))
val lazy_needs_partial : unit lazy_t * bool t ref -> int = <fun>
|}];;

let guard_total : bool t ref -> int = function
  | _ when Sys.opaque_identity false -> 1
  | { contents = True } -> 0
  | { contents = False } -> 12
(* This pattern-matching is total: a Match_failure case is not
   necessary for soundness. *)
[%%expect {|
(let
  (guard_total/314 =
     (function {nlocal = 0} param/408 : int
       (if (opaque 0) 1
         (let (*match*/409 =o? (field_mut 0 param/408))
           (if (isint *match*/409) (if *match*/409 12 0)
             (raise
               (makeblock 0 (getpredef Match_failure/49!!) [0: "" 1 38])))))))
  (apply (field_imm 1 (global Toploop!)) "guard_total" guard_total/314))
val guard_total : bool t ref -> int = <fun>
|}];;

let guard_needs_partial : bool t ref -> int = function
  | { contents = True } -> 0
  | _ when Sys.opaque_identity false -> 1
  | { contents = False } -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (guard_needs_partial/410 =
     (function {nlocal = 0} param/412 : int
       (let (*match*/413 =o? (field_mut 0 param/412))
         (catch (if (isint *match*/413) (if *match*/413 (exit 9) 0) (exit 9))
          with (9)
           (if (opaque 0) 1
             (if (isint *match*/413) 12
               (raise
                 (makeblock 0 (getpredef Match_failure/49!!) [0: "" 1 46]))))))))
  (apply (field_imm 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/410))
val guard_needs_partial : bool t ref -> int = <fun>
|}];;
