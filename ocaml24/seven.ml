open Base
open Stdio

let rec part_two target acc numbers =
  if acc > target
  then false
  else (
    match numbers with
    | value :: tail ->
      part_two target (acc * value) tail
      || part_two target (acc + value) tail
      ||
      let concat = Int.to_string acc ^ Int.to_string value |> Int.of_string in
      part_two target concat tail
    | [] -> target = acc)
;;

let rec part_one target acc numbers =
  if acc > target
  then false
  else (
    match numbers with
    | value :: tail ->
      part_one target (acc * value) tail
      || part_one target (acc + value) tail
    | [] -> target = acc)
;;

let parse_line line =
  let numbers =
    String.split_on_chars line ~on:[ ':'; ' ' ]
    |> List.filter ~f:(fun x -> String.length x > 0)
    |> List.map ~f:Int.of_string
  in
  match numbers with
  | target :: rest -> target, rest
  | _ -> failwith "Invalid input"
;;


let run file part =
  let validator = if part = 1 then part_one else part_two in
  In_channel.with_file file ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      let target, numbers = parse_line line in
      match numbers with
      | head :: tail ->
        if validator target head tail then acc + target else acc
      | _ -> failwith "Invalid_input"))
;;
