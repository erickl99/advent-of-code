open Base
open Stdio

let rec split_even acc idx len string =
  if idx = String.length string
  then List.rev acc
  else (
    let substr = String.sub string ~pos:idx ~len in
    let shortened = Int.to_string (Int.of_string substr) in
    split_even (shortened :: acc) (idx + len) len string)
;;

let rec prepend left right =
  match left with
  | head :: tail -> prepend tail (head :: right)
  | [] -> right
;;

let rec change_stones acc = function
  | stone :: tail ->
    if String.equal stone "0"
    then change_stones ("1" :: acc) tail
    else if String.length stone % 2 = 0
    then (
      let new_stones = split_even [] 0 (String.length stone / 2) stone in
      change_stones (prepend new_stones acc) tail)
    else (
      let new_stone = Int.to_string (Int.of_string stone * 2024) in
      change_stones (new_stone :: acc) tail)
  | [] -> List.rev acc
;;

let part_one stones =
  let final_stones =
    Fn.apply_n_times ~n:25 (fun x -> change_stones [] x) stones
  in
  List.length final_stones
;;

let count_stones list =
  List.fold
    list
    ~init:(Map.empty (module String))
    ~f:(fun acc x ->
      match Map.find acc x with
      | Some prev -> Map.set acc ~key:x ~data:(prev + 1)
      | None -> Map.set acc ~key:x ~data:1)
;;

let change_and_collect stone_counts =
  Map.fold
    stone_counts
    ~init:(Map.empty (module String))
    ~f:(fun ~key ~data acc ->
      let stones_created =
        Fn.apply_n_times ~n:25 (fun x -> change_stones [] x) [ key ]
      in
      let new_stone_count = count_stones stones_created in
      Map.merge acc new_stone_count ~f:(fun ~key ->
          function
          | `Left value ->
            ignore key;
            Some value
          | `Right value ->
            ignore key;
            Some (value * data)
          | `Both (left, right) ->
            ignore key;
            Some (left + (data * right))))
;;

let part_two stones =
  let init_count = count_stones stones in
  let result = Fn.apply_n_times ~n:3 change_and_collect init_count in
  Map.data result |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
;;

let run file part =
  let tokens =
    In_channel.read_all file |> String.strip |> String.split ~on:' '
  in
  if part = 1 then part_one tokens else part_two tokens
;;
