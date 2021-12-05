open Ocaml_common

[@@@warning "-34"]
let () = Printexc.record_backtrace true

let loc = Location.none

open Code

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

(* Inject the defined stdlib into the environment via type_structure *)
let env = O2l.type_structure ~env stdlib |> snd

(* Pass the AST generated from the input to type_structure using the generated env.
   Extract the structure item list and stringify it, then print the result *)
let () =
  (* Sys.argv.(1) (* .ml file to convert to .mligo *)
     |> Pparse.parse_implementation ~tool_name:"O2L" *)
  code |> O2l.type_structure ~env |> fst
  |> (*O2l.module_erasure |>*) O2l.typed_string_of_code |> print_endline
