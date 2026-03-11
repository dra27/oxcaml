(* Errors *)

exception Fatal_error

let fatal_errorf fmt =
  Format.kfprintf
    (fun _ -> raise Fatal_error)
    Format.err_formatter
    ("@?>> Fatal error: " ^^ fmt ^^ "@.")

let fatal_error msg = fatal_errorf "%s" msg

let fatal_errorf_doc fmt =
  Format_doc.kdoc_printf (fun doc ->
    fatal_errorf "%t" (fun ppf -> Format_doc.Doc.format ppf doc)
  ) fmt

module Stdlib = struct
  module List = struct
    let [@inline] merge_fold ~cmp ~left_only ~right_only ~both ~init t1 t2 =
      let rec loop acc t1 t2 =
        match t1, t2 with
        | [], [] -> acc
        | a :: t1', [] -> loop (left_only acc a) t1' []
        | [], b :: t2' -> loop (right_only acc b) [] t2'
        | a :: t1', b :: t2' ->
            match cmp a b with
            | 0 -> loop (both acc a b) t1' t2'
            | c when c < 0 -> loop (left_only acc a) t1' t2
            | _ -> loop (right_only acc b) t1 t2'
      in
      loop init t1 t2

    let [@inline] merge_iter ~cmp ~left_only ~right_only ~both t1 t2 =
      merge_fold t1 t2 ~cmp
        ~init:()
        ~left_only:(fun () a -> left_only a)
        ~right_only:(fun () b -> right_only b)
        ~both:(fun () a b -> both a b)
  end
end
