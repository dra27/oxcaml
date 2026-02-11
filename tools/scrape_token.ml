(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let () =
  try
    In_channel.with_open_text Sys.argv.(1) (fun ic ->
      while true do
        let line = input_line ic in
        if String.starts_with ~prefix:"  |" line then
          print_endline line
      done
    )
  with End_of_file -> ()
