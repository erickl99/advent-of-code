open Base
open Stdio

let extract_nums (list_one, list_two) line =
  match String.split ~on:' ' line with
  | [ left; _; _; right ] ->
    Int.of_string left :: list_one, Int.of_string right :: list_two
  | _ -> failwith "Invalid input"
;;

let part_one file =
  let list_one, list_two =
    In_channel.fold_lines file ~init:([], []) ~f:extract_nums
  in
  let sorted_one, sorted_two =
    ( List.sort list_one ~compare:Int.compare
    , List.sort list_two ~compare:Int.compare )
  in
  List.fold2_exn sorted_one sorted_two ~init:0 ~f:(fun acc left right ->
    acc + Int.abs (left - right))
;;

let part_two file =
  let count = Hashtbl.create (module Int) in
  let extract_left_count_right acc line =
    match String.split line ~on:' ' with
    | [ left; _; _; right ] ->
      let left, right = Int.of_string left, Int.of_string right in
      (match Hashtbl.find count right with
       | Some previous ->
         Hashtbl.set count ~key:right ~data:(previous + 1);
         left :: acc
       | None ->
         Hashtbl.set count ~key:right ~data:1;
         left :: acc)
    | _ -> failwith "Invalid input"
  in
  let locations =
    In_channel.fold_lines file ~init:[] ~f:extract_left_count_right
  in
  List.fold locations ~init:0 ~f:(fun acc loc ->
    match Hashtbl.find count loc with
    | Some value -> acc + (loc * value)
    | None -> acc)
;;

let run file part =
  let solver = if part = 1 then part_one else part_two in
  In_channel.with_file file ~f:solver
;;
