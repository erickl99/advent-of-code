open Stdio
open Base

let () =
  let day, file_name =
    match Sys.get_argv () with
    | [| _; day |] -> Int.of_string day, "input/day" ^ day
    | [| _; day; "test" |] -> Int.of_string day, "input/day" ^ day ^ "test"
    | _ ->
      eprintf "Usage: aoc <day> [test]";
      Stdlib.exit 1
  in
  let result_one, result_two =
    match day with
    | 1 -> Dayone.run file_name
    | 2 -> Daytwo.run file_name
    | 3 -> Daythree.run file_name
    | 4 -> Dayfour.run file_name
    | 5 -> Dayfive.run file_name
    | _ ->
      eprintf "Choose a valid day!";
      Stdlib.exit 1
  in
  printf "Part one: %d\nPart two: %d\n" result_one result_two
;;
