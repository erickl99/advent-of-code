open Base
open Stdio

let deltas =
  [| [| -1; 0 |]
   ; [| -1; 1 |]
   ; [| 0; 1 |]
   ; [| 1; 1 |]
   ; [| 1; 0 |]
   ; [| 1; -1 |]
   ; [| 0; -1 |]
   ; [| -1; -1 |]
  |]
;;

let accessible grid i j =
  let valid =
    Array.count deltas ~f:(fun delta ->
      let new_i, new_j = delta.(0) + i, delta.(1) + j in
      new_i >= 0
      && new_i < Array.length grid
      && new_j >= 0
      && new_j < Array.length grid.(0)
      && Char.equal grid.(new_i).(new_j) '@')
  in
  valid < 4
;;

let find_valid_rolls grid =
  Array.foldi
    grid
    ~init:(Array.create ~len:0 (0, 0))
    ~f:(fun i acc row ->
      let valid_rolls_in_row =
        Array.filter_mapi row ~f:(fun j cell ->
          if Char.equal cell '@' && accessible grid i j
          then Some (i, j)
          else None)
      in
      Array.append acc valid_rolls_in_row)
;;

let run file_name =
  let grid =
    In_channel.read_lines file_name |> Array.of_list_map ~f:String.to_array
  in
  let valid_rolls = find_valid_rolls grid in
  let result_one = Array.length valid_rolls in
  let result_two =
    Sequence.unfold ~init:(valid_rolls, grid) ~f:(fun (valid_rolls, grid) ->
      if Array.length valid_rolls = 0
      then None
      else (
        Array.iter valid_rolls ~f:(fun (i, j) -> grid.(i).(j) <- '.');
        let valid_rolls = find_valid_rolls grid in
        Some (valid_rolls, (valid_rolls, grid))))
    |> Sequence.fold ~init:0 ~f:(fun acc valid_rolls ->
      acc + Array.length valid_rolls)
  in
  result_one, result_one + result_two
;;
