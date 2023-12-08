open Base
open Stdio

let sum_file filename func =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun sum line -> sum + func line))
;;

let run_day file_name = function
  | 1 -> sum_file file_name Dayone.calibrate_one
  | 2 -> sum_file file_name Daytwo.game_sum
  | 3 -> In_channel.with_file file_name ~f:Daythree.process_file
  | 4 -> In_channel.with_file file_name ~f:Dayfour.process_file
  | _ -> raise (Failure "Not a valid day!")
;;

let () =
  let args = Sys.get_argv () in
  if Array.length args < 2
  then print_endline "Please provide a day to run!"
  else (
    match Int.of_string (Array.get args 1) with
    | day -> printf "The answer is %d\n" (run_day "test" day)
    | exception Failure _ -> print_endline "Please provide a valid day!")
;;
