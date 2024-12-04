open Base
open Stdio

let verify_report list =
  let rec is_monotonic increasing prev list =
    match list, increasing with
    | value :: tail, true ->
      value > prev && value - prev <= 3 && is_monotonic increasing value tail
    | value :: tail, false ->
      value < prev && prev - value <= 3 && is_monotonic increasing value tail
    | [], _ -> true
  in
  match list with
  | first :: second :: tail ->
    if first < second && second - first <= 3
    then is_monotonic true second tail
    else if first > second && first - second <= 3
    then is_monotonic false second tail
    else false
  | _ -> failwith "Invalid input"
;;

let part_one file =
  In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
    let valid =
      let numbers = String.split line ~on:' ' |> List.map ~f:Int.of_string in
      verify_report numbers
    in
    if valid then acc + 1 else acc)
;;

let rec create_sublist acc list idx =
  match list with
  | head :: tail ->
    if List.length acc = idx
    then List.rev acc @ tail
    else create_sublist (head :: acc) tail idx
  | [] -> failwith "Invalid index"
;;

let check_subreports list =
  let length = List.length list in
  let idx_sequence =
    Sequence.unfold ~init:0 ~f:(fun idx ->
      if idx < length then Some (idx, idx + 1) else None)
  in
  Sequence.exists idx_sequence ~f:(fun idx ->
    let sublist = create_sublist [] list idx in
    verify_report sublist)
;;

let part_two file =
  let result =
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      let valid =
        let numbers = String.split line ~on:' ' |> List.map ~f:Int.of_string in
        if verify_report numbers then true else check_subreports numbers
      in
      if valid then acc + 1 else acc)
  in
  result
;;

let run file part =
  let solver = if part = 1 then part_one else part_two in
  In_channel.with_file file ~f:solver
;;
