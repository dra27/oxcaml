(* TEST
 flags = "-dlambda";
 stack-allocation;
 expect;
*)

(* The original example of unsoundness in #7421. *)
type t = {a: bool; mutable b: int option}

let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = _;     b = _} when (x.b <- None; false) -> 2
  | {a = true;  b = Some y} -> y
;;
(* Correctness condition: there should either be a single
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   PASS: the second access includes a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (f/290 =
     (function {nlocal = 0} x/292 : int
       (if (field_int 0 x/292)
         (let (*match*/296 =o? (field_mut 1 x/292))
           (if *match*/296
             (if (seq (setfield_ptr 1 x/292 0) 0) 2
               (let (*match*/297 =o? (field_mut 1 x/292))
                 (if *match*/297 (field_imm 0 *match*/297)
                   (raise
                     (makeblock 0 (getpredef Match_failure/49!!) [0: "" 4 2])))))
             1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/290))
val f : t -> int = <fun>
|}]



(* A simple example of a complete switch
   inside a mutable position. *)
type t = {a: bool; mutable b: int option}

let simple x =
  match x with
  | {b = None} -> 1
  | {b = Some y} -> y
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (simple/303 =
     (function {nlocal = 0} x/305 : int
       (let (*match*/308 =o? (field_mut 1 x/305))
         (if *match*/308 (field_imm 0 *match*/308) 1))))
  (apply (field_imm 1 (global Toploop!)) "simple" simple/303))
val simple : t -> int = <fun>
|}]

(* This more complex case has the switch on [b] split across two cases
   on [a], so it may need a [Match_failure] for soundness -- it does
   if the two accesses to [b] are done on different reads of the same
   mutable field.

   PASS: a single read of [field_mut 1 x], no Match_failure case. *)
let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = true;  b = Some y} -> y
;;
[%%expect {|
(let
  (f/309 =
     (function {nlocal = 0} x/310 : int
       (if (field_int 0 x/310)
         (let (*match*/314 =o? (field_mut 1 x/310))
           (if *match*/314 (field_imm 0 *match*/314) 1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/309))
val f : t -> int = <fun>
|}]



(* A variant of the #7421 example. *)
let f r =
  match Some r with
  | Some { contents = None } -> 0
  | _ when (r := None; false) -> 1
  | Some { contents = Some n } -> n
  | None -> 3
;;
(* Correctness condition: there should either be a single
   (field_mut 0) access, or the second access should include
   a Match_failure case.

   FAIL: the second occurrence of (field_mut 0) is used with a direct
   (field_imm 0) access without a constructor check. The compiler is
   unsound here. *)
[%%expect {|
(let
  (f/316 =
     (function {nlocal = 0} r/317 : int
       (region
         (let
           (*match*/319 =[value<(consts (0)) (non_consts ([0: ?]))>]
              (makelocalblock 0 (*) r/317))
           (catch
             (if *match*/319
               (let (*match*/321 =o? (field_mut 0 (field_imm 0 *match*/319)))
                 (if *match*/321 (exit 13) 0))
               (exit 13))
            with (13)
             (if (seq (setfield_ptr 0 r/317 0) 0) 1
               (if *match*/319
                 (let
                   (*match*/323 =o? (field_mut 0 (field_imm 0 *match*/319)))
                   (field_imm 0 *match*/323))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/316))
val f : int option ref -> int = <fun>
|}]



(* This example has an ill-typed counter-example: the type-checker
   finds it Total, but the pattern-matching compiler cannot see that
   (Some (Some (Bool b))) cannot occur. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | None -> 0
  | Some (Int n) -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/327 =
     (function {nlocal = 0}
       param/330[value<(consts (0)) (non_consts ([0: ?]))>] : int
       (if param/330 (field_imm 0 (field_imm 0 param/330)) 0)))
  (apply (field_imm 1 (global Toploop!)) "test" test/327))
val test : int t option -> int = <fun>
|}]


(* This example has an ill-typed counter-example, inside
   a mutable position.  *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | { contents = None } -> 0
  | { contents = Some (Int n) } -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/335 =
     (function {nlocal = 0} param/337 : int
       (let (*match*/338 =o? (field_mut 0 param/337))
         (if *match*/338 (field_imm 0 (field_imm 0 *match*/338)) 0))))
  (apply (field_imm 1 (global Toploop!)) "test" test/335))
val test : int t option ref -> int = <fun>
|}]



(* This example has a ill-typed counter-example,
   and also mutable sub-patterns, but in different places. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test n =
  match Some (ref true, Int 42) with
  | Some ({ contents = true }, Int n) -> n
  | Some ({ contents = false }, Int n) -> -n
  | None -> 3
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/343 =
     (function {nlocal = 0} n/344? : int
       (region
         (let
           (*match*/347 =[value<(consts (0)) (non_consts ([0: ?]))>]
              (makelocalblock 0 (value<
                                  (consts ())
                                   (non_consts ([0: *,
                                                 value<
                                                  (consts ())
                                                   (non_consts ([1:
                                                                 value<int>]
                                                   [0: value<int>]))>]))>)
                (makelocalblock 0 (*,value<
                                      (consts ())
                                       (non_consts ([1: value<int>]
                                       [0: value<int>]))>)
                  (makelocalmutable 0 (value<int>) 1) [0: 42])))
           (if *match*/347
             (let
               (*match*/348 =a? (field_imm 0 *match*/347)
                *match*/350 =o? (field_mut 0 (field_imm 0 *match*/348)))
               (if *match*/350 (field_imm 0 (field_imm 1 *match*/348))
                 (%int_neg (field_imm 0 (field_imm 1 *match*/348)))))
             3)))))
  (apply (field_imm 1 (global Toploop!)) "test" test/343))
val test : 'a -> int = <fun>
|}]



(* In this example, the constructor on which unsound assumptions could
   be made is not located directly below a mutable constructor, but
   one level deeper inside an immutable pair constructor (below the
   mutable constructor). This checks that there is a form of
   "transitive" propagation of mutability.

   Correctness condition: either there is a single mutable field read,
   or the accesses below the second mutable read have a Match_failure
   case.
*)
let deep r =
  match Some r with
  | Some { contents = ((), None) } -> 0
  | _ when (r := ((), None); false) -> 1
  | Some { contents = ((), Some n) } -> n
  | None -> 3
;;
(* FAIL: two different reads (field_mut 0), but no Match_failure case. *)
[%%expect {|
(let
  (deep/353 =
     (function {nlocal = 0} r/355 : int
       (region
         (let
           (*match*/357 =[value<(consts (0)) (non_consts ([0: ?]))>]
              (makelocalblock 0 (*) r/355))
           (catch
             (if *match*/357
               (let (*match*/359 =o? (field_mut 0 (field_imm 0 *match*/357)))
                 (if (field_imm 1 *match*/359) (exit 21) 0))
               (exit 21))
            with (21)
             (if (seq (setfield_ptr 0 r/355 [0: 0 0]) 0) 1
               (if *match*/357
                 (let
                   (*match*/363 =o? (field_mut 0 (field_imm 0 *match*/357)))
                   (field_imm 0 (field_imm 1 *match*/363)))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "deep" deep/353))
val deep : (unit * int option) ref -> int = <fun>
|}]
