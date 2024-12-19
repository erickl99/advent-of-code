open Stdio
open Base

let () =
  let day, part, file_name =
    match Sys.get_argv () with
    | [| _; day; part |] ->
      let file_name = if String.length day = 1 then "0" ^ day else day in
      Int.of_string day, Int.of_string part, file_name
    | [| _; day; part; "test" |] ->
      let file_name = if String.length day = 1 then "0" ^ day else day in
      Int.of_string day, Int.of_string part, file_name ^ ".test"
    | _ ->
      eprintf "Usage: aoc <day> <part> [test]\n";
      Stdlib.exit 1
  in
  let file_name = "input/day" ^ file_name ^ ".txt" in
  let result =
    match day with
    | 1 -> One.run file_name part
    | 2 -> Two.run file_name part
    | 3 -> Three.run file_name part
    | 4 -> Four.run file_name part
    | 5 -> Five.run file_name part
    | 6 -> Six.run file_name part
    | 7 -> Seven.run file_name part
    | 8 -> Eight.run file_name part
    | 9 -> Nine.run file_name part
    | 10 -> Ten.run file_name part
    | 11 -> Eleven.run file_name part
    | 12 -> Twelve.run file_name part
    | 13 -> Thirteen.run file_name part
    | 14 -> Fourteen.run file_name part
    | 15 -> Fifteen.run file_name part
    | 16 -> Sixteen.run file_name part
    | 17 -> Seventeen.run file_name part
    | 18 -> Eighteen.run file_name part
    | 19 -> Nineteen.run file_name part
    | _ ->
      eprintf "Invalid day given\n";
      Stdlib.exit 1
  in
  printf "The result for part %d is %d\n" part result
;;
