open Base
open Stdio

let sum_file filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun sum line ->
      sum + Dayfour.total_points line))
;;

let () =
  printf "The answer is %d\n" (sum_file "input");
  printf
    "The second answer is %d\n"
    (In_channel.with_file "input" ~f:Dayfour.process_file)
;;
