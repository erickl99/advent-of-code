open Base
open Stdio

let split_stone stone =
  let len = String.length stone / 2 in
  let left, right =
    String.sub stone ~pos:0 ~len, String.sub stone ~pos:len ~len
  in
  Int.of_string left, Int.of_string right
;;

let get_or_default map key = Option.value ~default:0 (Map.find map key)

let blink stones =
  let init = Map.empty (module Int) in
  let created =
    Map.fold stones ~init ~f:(fun ~key:stone ~data:count acc ->
      if stone = 0
      then (
        let prev = get_or_default acc 1 in
        Map.set acc ~key:1 ~data:(count + prev))
      else if String.length (Int.to_string stone) % 2 = 0
      then (
        let left, right = split_stone (Int.to_string stone) in
        let prev_left = get_or_default acc left in
        let left_added = Map.set acc ~key:left ~data:(prev_left + count) in
        let prev_right = get_or_default left_added right in
        Map.set left_added ~key:right ~data:(prev_right + count))
      else (
        let new_stone = stone * 2024 in
        let prev = get_or_default acc new_stone in
        Map.set acc ~key:new_stone ~data:(prev + count)))
  in
  created
;;

let count_stones list =
  List.fold
    list
    ~init:(Map.empty (module Int))
    ~f:(fun acc x ->
      match Map.find acc x with
      | Some prev -> Map.set acc ~key:x ~data:(prev + 1)
      | None -> Map.set acc ~key:x ~data:1)
;;

let part_one stones =
  let init = count_stones stones in
  let result = Fn.apply_n_times ~n:25 blink init in
  Map.data result |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
;;

let part_two stones =
  let init = count_stones stones in
  let result = Fn.apply_n_times ~n:75 blink init in
  Map.data result |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
;;

let run file part =
  let tokens =
    In_channel.read_all file
    |> String.strip
    |> String.split ~on:' '
    |> List.map ~f:Int.of_string
  in
  if part = 1 then part_one tokens else part_two tokens
;;
