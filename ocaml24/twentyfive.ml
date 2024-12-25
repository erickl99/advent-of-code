open Base
open Stdio

let rec build_key_lock acc = function
  | "" :: tail -> List.rev acc |> List.to_array, tail
  | line :: tail -> build_key_lock (line :: acc) tail
  | [] -> List.rev acc |> List.to_array, []
;;

let key_lock_to_nums key_lock keys locks =
  let is_lock =
    let row_one = key_lock.(0) in
    Char.equal '#' (String.get row_one 0)
  in
  let heights = Array.create ~len:5 (-1) in
  Array.iter key_lock ~f:(fun row ->
    String.iteri row ~f:(fun idx c ->
      if Char.equal '#' c then heights.(idx) <- heights.(idx) + 1));
  if is_lock then keys, heights :: locks else heights :: keys, locks
;;

let rec collect_key_lock keys locks = function
  | head :: tail ->
    if String.length head = 0
    then collect_key_lock keys locks tail
    else (
      let key_lock, rest = build_key_lock [ head ] tail in
      let keys, locks = key_lock_to_nums key_lock keys locks in
      collect_key_lock keys locks rest)
  | [] -> List.to_array keys, List.to_array locks
;;

let valid_key_lock key lock =
  print_endline "Checking lock:";
  Array.iter lock ~f:(printf "%d ");
  printf "\n";
  Array.for_alli key ~f:(fun idx height -> height + lock.(idx) < 6)
;;

let find_valid_pairs keys locks =
  Array.fold keys ~init:0 ~f:(fun acc key ->
    print_endline "Checking key:";
    Array.iter key ~f:(printf "%d ");
    printf "\n";
    acc + Array.count locks ~f:(fun lock -> valid_key_lock key lock))
;;

let run file _ =
  let lines = In_channel.read_lines file in
  let keys, locks = collect_key_lock [] [] lines in
  find_valid_pairs keys locks
;;
