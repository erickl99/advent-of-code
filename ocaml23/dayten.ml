open Base

module Pair = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) = if x1 <> x2 then x1 - x2 else y1 - y2
    let sexp_of_t (x, y) = sexp_of_int (x + y)
  end

  include T
  include Comparator.Make (T)
end

type pipe =
  | DOWN_UP
  | LEFT_RIGHT
  | LEFT_UP
  | UP_RIGHT
  | DOWN_RIGHT
  | LEFT_DOWN
  | GROUND

type direction =
  | LEFT
  | RIGHT
  | UP
  | DOWN

let walk_pipe p d =
  match p, d with
  | LEFT_UP, RIGHT -> UP
  | LEFT_UP, DOWN -> LEFT
  | UP_RIGHT, DOWN -> RIGHT
  | UP_RIGHT, LEFT -> UP
  | DOWN_RIGHT, UP -> RIGHT
  | DOWN_RIGHT, LEFT -> DOWN
  | LEFT_DOWN, RIGHT -> DOWN
  | LEFT_DOWN, UP -> LEFT
  | _ -> d
;;

let same_tuple (x, y) (a, b) = x = a && y = b

let rec traverse_loop current d map start =
  if same_tuple current start
  then 1
  else (
    let y, x = current in
    match walk_pipe (Map.find_exn map (y, x)) d with
    | UP -> 1 + traverse_loop (y - 1, x) UP map start
    | DOWN -> 1 + traverse_loop (y + 1, x) DOWN map start
    | LEFT -> 1 + traverse_loop (y, x - 1) LEFT map start
    | RIGHT -> 1 + traverse_loop (y, x + 1) RIGHT map start)
;;

let process_file file =
  let build_map (i : int) map start line =
    String.foldi
      ~f:(fun j (m, s) c ->
        match c with
        | '|' -> Map.set m ~key:(i, j) ~data:DOWN_UP, s
        | '-' -> Map.set m ~key:(i, j) ~data:LEFT_RIGHT, s
        | 'L' -> Map.set m ~key:(i, j) ~data:UP_RIGHT, s
        | 'F' -> Map.set m ~key:(i, j) ~data:DOWN_RIGHT, s
        | '7' -> Map.set m ~key:(i, j) ~data:LEFT_DOWN, s
        | 'J' -> Map.set m ~key:(i, j) ~data:LEFT_UP, s
        | 'S' -> Map.set m ~key:(i, j) ~data:LEFT_DOWN, (i, j)
        | _ -> Map.set m ~key:(i, j) ~data:GROUND, s)
      ~init:(map, start)
      line
  in
  let rec fold_lines idx map start file =
    match In_channel.input_line file with
    | Some line ->
      let new_map, start = build_map idx map start line in
      fold_lines (idx + 1) new_map start file
    | None -> map, start
  in
  let pipe_map, (y, x) = fold_lines 0 (Map.empty (module Pair)) (-1, -1) file in
  let loop = traverse_loop (y + 1, x) DOWN pipe_map (y, x) in
  loop / 2
;;
