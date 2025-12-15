open Base
open Stdio

let run file_name =
  let ranges, ingredients =
    In_channel.read_lines file_name
    |> List.split_while ~f:(fun x -> not @@ String.equal x "")
  in
  let ingredients = List.drop ingredients 1 |> List.map ~f:Int.of_string in
  let ranges =
    List.map ranges ~f:(fun range ->
      let start, stop = String.lsplit2_exn range ~on:'-' in
      Int.of_string start, Int.of_string stop)
    |> List.sort ~compare:(fun (x_1, x_2) (y_1, y_2) ->
      if x_1 = y_1 then x_2 - y_2 else x_1 - y_1)
  in
  let first, rest = List.hd_exn ranges, List.tl_exn ranges in
  let ranges =
    List.fold rest ~init:[ first ] ~f:(fun acc (start, stop) ->
      match acc with
      | (x, y) :: tail when y >= start -> (x, Int.max y stop) :: tail
      | _ -> (start, stop) :: acc)
    |> List.rev
  in
  let result_one =
    List.count ingredients ~f:(fun x ->
      List.exists ranges ~f:(fun (start, stop) -> start <= x && stop >= x))
  in
  let result_two =
    List.fold ranges ~init:0 ~f:(fun acc (start, stop) ->
      acc + (stop - start) + 1)
  in
  result_one, result_two
;;
