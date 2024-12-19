open Base
open Stdio

let count_arrangments towels pattern =
  let len = String.length pattern in
  let dp = Array.create ~len:(len + 1) 0 in
  dp.(0) <- 1;
  for idx = 0 to len - 1 do
    List.iter towels ~f:(fun towel ->
      if idx + 1 >= String.length towel
         && String.is_substring_at
              pattern
              ~pos:(idx + 1 - String.length towel)
              ~substring:towel
      then dp.(idx + 1) <- dp.(idx + 1) + dp.(idx - String.length towel + 1))
  done;
  dp.(len)
;;

let part_one towels patterns =
  List.count patterns ~f:(fun pattern -> count_arrangments towels pattern > 0)
;;

let part_two towels patterns =
  List.fold patterns ~init:0 ~f:(fun acc pattern ->
    acc + count_arrangments towels pattern)
;;

let run file part =
  let input = In_channel.read_lines file in
  let towels =
    List.hd_exn input |> String.split ~on:',' |> List.map ~f:String.strip
  in
  let patterns = List.drop input 2 in
  if part = 1 then part_one towels patterns else part_two towels patterns
;;
