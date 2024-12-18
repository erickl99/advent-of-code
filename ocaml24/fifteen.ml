open Base
open Stdio

let sum_gps_coords grid =
  Array.foldi grid ~init:0 ~f:(fun row_idx total row ->
    total
    + Array.foldi row ~init:0 ~f:(fun col_idx acc point ->
      match point with
      | '#' | '.' | '@' | ']' -> acc
      | 'O' -> acc + (100 * row_idx) + col_idx
      | '[' -> acc + (100 * row_idx) + col_idx
      | _ -> failwith "Input was modified incorrectly"))
;;

let print_grid = Array.iter ~f:(fun row -> printf "%s\n" (String.of_array row))

module PartOne = struct
  let rec open_space x y delta_x delta_y grid =
    match grid.(y).(x) with
    | '#' -> None
    | 'O' -> open_space (x + delta_x) (y + delta_y) delta_x delta_y grid
    | '.' -> Some (x, y)
    | _ -> failwith "Invalid input"
  ;;

  let can_move x y delta_x delta_y grid =
    let next_x, next_y = x + delta_x, y + delta_y in
    if Char.equal grid.(next_y).(next_x) '.'
    then true
    else (
      match open_space next_x next_y delta_x delta_y grid with
      | Some (open_x, open_y) ->
        grid.(open_y).(open_x) <- 'O';
        true
      | None -> false)
  ;;

  let move_robot x y grid = function
    | '>' ->
      if can_move !x !y 1 0 grid
      then (
        grid.(!y).(!x) <- '.';
        Int.incr x;
        grid.(!y).(!x) <- '@')
    | '<' ->
      if can_move !x !y (-1) 0 grid
      then (
        grid.(!y).(!x) <- '.';
        Int.decr x;
        grid.(!y).(!x) <- '@')
    | '^' ->
      if can_move !x !y 0 (-1) grid
      then (
        grid.(!y).(!x) <- '.';
        Int.decr y;
        grid.(!y).(!x) <- '@')
    | 'v' ->
      if can_move !x !y 0 1 grid
      then (
        grid.(!y).(!x) <- '.';
        Int.incr y;
        grid.(!y).(!x) <- '@')
    | _ -> failwith "Invalid direction"
  ;;

  let rec read_map start_x start_y acc = function
    | "" :: tail -> List.rev acc, tail
    | row :: tail ->
      let position = String.index row '@' in
      if Option.is_some position
      then (
        start_x := Option.value_exn position;
        start_y := List.length acc);
      read_map start_x start_y (String.to_array row :: acc) tail
    | [] -> failwith "Reached end of input"
  ;;

  let solve input =
    let start_x, start_y = ref 0, ref 0 in
    let raw_grid, move_list = read_map start_x start_y [] input in
    let grid, moves = List.to_array raw_grid, String.concat move_list in
    String.iter moves ~f:(fun c -> move_robot start_x start_y grid c);
    sum_gps_coords grid
  ;;
end

module PartTwo = struct
  let double_char = function
    | '#' -> "##"
    | 'O' -> "[]"
    | '.' -> ".."
    | '@' -> "@."
    | _ -> failwith "Invalid input"
  ;;

  let rec read_map start_x start_y acc = function
    | "" :: tail -> List.rev acc, tail
    | row :: tail ->
      let doubled_row = String.concat_map row ~f:double_char in
      let position = String.index doubled_row '@' in
      if Option.is_some position
      then (
        start_x := Option.value_exn position;
        start_y := List.length acc);
      read_map start_x start_y (String.to_array doubled_row :: acc) tail
    | [] -> failwith "Reached end of input"
  ;;

  let rec move_rocks_ver x y delta grid =
    let left, right = grid.(y + delta).(x), grid.(y + delta).(x + 1) in
    let () =
      match left, right with
      | '.', '.' -> ()
      | '[', ']' -> move_rocks_ver x (y + delta) delta grid
      | '.', '[' -> move_rocks_ver (x + 1) (y + delta) delta grid
      | ']', '.' -> move_rocks_ver (x - 1) (y + delta) delta grid
      | ']', '[' ->
        move_rocks_ver (x - 1) (y + delta) delta grid;
        move_rocks_ver (x + 1) (y + delta) delta grid
      | _ -> failwith "Impossible case"
    in
    grid.(y + delta).(x) <- '[';
    grid.(y + delta).(x + 1) <- ']';
    grid.(y).(x) <- '.';
    grid.(y).(x + 1) <- '.'
  ;;

  let rec try_move x y delta grid =
    let left, right = grid.(y + delta).(x), grid.(y + delta).(x + 1) in
    match left, right with
    | '[', ']' -> try_move x (y + delta) delta grid
    | ']', '[' ->
      try_move (x - 1) (y + delta) delta grid
      && try_move (x + 1) (y + delta) delta grid
    | ']', '.' -> try_move (x - 1) (y + delta) delta grid
    | '.', '[' -> try_move (x + 1) (y + delta) delta grid
    | '.', '.' -> true
    | '#', _ -> false
    | _, '#' -> false
    | _ -> failwith "Uhhhhh"
  ;;

  let can_move_ver x y delta grid =
    let next_y = y + delta in
    match grid.(next_y).(x) with
    | '.' -> true
    | '#' -> false
    | ']' ->
      if try_move (x - 1) next_y delta grid
      then (
        move_rocks_ver (x - 1) next_y delta grid;
        true)
      else false
    | '[' ->
      if try_move x next_y delta grid
      then (
        move_rocks_ver x next_y delta grid;
        true)
      else false
    | _ -> failwith "Invalid map object"
  ;;

  let rec move_rocks_hor start_x start_y end_x delta grid =
    if start_x <> end_x
    then (
      let temp = grid.(start_y).(start_x) in
      grid.(start_y).(start_x) <- grid.(start_y).(start_x + delta);
      grid.(start_y).(start_x + delta) <- temp;
      move_rocks_hor (start_x + delta) start_y end_x delta grid)
  ;;

  let rec open_space_hor x y delta_x grid =
    match grid.(y).(x) with
    | '#' -> None
    | '[' | ']' -> open_space_hor (x + delta_x) y delta_x grid
    | '.' -> Some (x, y)
    | _ -> failwith "Invalid input"
  ;;

  let can_move_hor x y delta grid =
    let next_x = x + delta in
    if Char.equal grid.(y).(next_x) '.'
    then true
    else (
      match open_space_hor next_x y (2 * delta) grid with
      | Some (open_x, open_y) ->
        move_rocks_hor open_x open_y next_x (-delta) grid;
        true
      | None -> false)
  ;;

  let move_robot x y grid = function
    | '>' ->
      if can_move_hor !x !y 1 grid
      then (
        grid.(!y).(!x) <- '.';
        Int.incr x;
        grid.(!y).(!x) <- '@')
    | '<' ->
      if can_move_hor !x !y (-1) grid
      then (
        grid.(!y).(!x) <- '.';
        Int.decr x;
        grid.(!y).(!x) <- '@')
    | '^' ->
      if can_move_ver !x !y (-1) grid
      then (
        grid.(!y).(!x) <- '.';
        Int.decr y;
        grid.(!y).(!x) <- '@')
    | 'v' ->
      if can_move_ver !x !y 1 grid
      then (
        grid.(!y).(!x) <- '.';
        Int.incr y;
        grid.(!y).(!x) <- '@')
    | _ -> failwith "Invalid direction"
  ;;

  let solve input =
    let start_x, start_y = ref 0, ref 0 in
    let raw_grid, move_list = read_map start_x start_y [] input in
    let grid, moves = List.to_array raw_grid, String.concat move_list in
    String.iter moves ~f:(fun c -> move_robot start_x start_y grid c);
    sum_gps_coords grid
  ;;
end

let run file part =
  let input = In_channel.read_lines file in
  if part = 1 then PartOne.solve input else PartTwo.solve input
;;
