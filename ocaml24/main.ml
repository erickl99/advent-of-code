open Stdio
open Base

let () =
  let day, part, file =
    match Sys.get_argv () with
    | [| _; day; part |] -> Int.of_string day, Int.of_string part, "input"
    | [| _; day; part; "test" |] ->
      Int.of_string day, Int.of_string part, "test"
    | _ ->
      eprintf "Usage: aoc <day> <part> [test]\n";
      Stdlib.exit 1
  in
  let result = (match day with
  | 1 -> One.run file part
  | 2 -> Two.run file part
  | 3 -> Three.run file part
  | 4 -> Four.run file part
  | 5 -> Five.run file part
  | 6 -> Six.run file part
  | _ ->
    eprintf "Invalid day given";
    Stdlib.exit 1)
  in printf "The result for part %d is %d\n" part result
;;
