open Stdio
open Base

module Pair = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) = if x1 <> x2 then x1 - x2 else y1 - y2
    let sexp_of_t (x, y) = Sexp.List [ sexp_of_int x; sexp_of_int y ]
  end

  include T
  include Comparator.Make (T)
end

type direction =
  | North
  | East
  | South
  | West

let same_dir one two =
  match one, two with
  | North, North -> true
  | East, East -> true
  | South, South -> true
  | West, West -> true
  | _ -> false
;;

let rec walk_path y x max_row max_col direction obstacles visited =
  let new_y, new_x, next_direction =
    match direction with
    | North -> y - 1, x, East
    | East -> y, x + 1, South
    | South -> y + 1, x, West
    | West -> y, x - 1, North
  in
  if new_x >= max_col || new_x < 0 || new_y >= max_row || new_y < 0
  then visited, false
  else if List.exists visited ~f:(fun (past_y, past_x, dir) ->
            Pair.compare (new_y, new_x) (past_y, past_x) = 0
            && same_dir dir direction)
  then visited, true
  else if Set.mem obstacles (new_y, new_x)
  then walk_path y x max_row max_col next_direction obstacles visited
  else
    walk_path
      new_y
      new_x
      max_row
      max_col
      direction
      obstacles
      ((new_y, new_x, direction) :: visited)
;;

let part_one start_y start_x max_row max_col obstacles =
  let init = [ start_y, start_x, North ] in
  let visited, _ =
    walk_path start_y start_x max_row max_col North obstacles init
  in
  let set =
    List.fold
      visited
      ~init:(Set.empty (module Pair))
      ~f:(fun set (y, x, _) -> Set.add set (y, x))
  in
  Set.length set
;;

let part_two start_y start_x max_row max_col obstacles =
  let init = [ start_y, start_x, North ] in
  let visited, _ =
    walk_path start_y start_x max_row max_col North obstacles init
  in
  let set =
    List.fold
      visited
      ~init:(Set.empty (module Pair))
      ~f:(fun set (y, x, _) -> Set.add set (y, x))
  in
  Set.fold set ~init:0 ~f:(fun acc point ->
    if Pair.compare point (start_y, start_x) = 0
    then acc
    else (
      let new_obstacles = Set.add obstacles point in
      match
        walk_path start_y start_x max_row max_col North new_obstacles init
      with
      | _, true -> acc + 1
      | _, false -> acc))
;;

let rec process_row set x y row_idx col_idx = function
  | '.' :: tail -> process_row set x y row_idx (col_idx + 1) tail
  | '#' :: tail ->
    process_row (Set.add set (row_idx, col_idx)) x y row_idx (col_idx + 1) tail
  | '^' :: tail -> process_row set row_idx col_idx row_idx (col_idx + 1) tail
  | [] -> set, x, y
  | _ -> failwith "Invalid input"
;;

let rec read_grid set x y idx lines =
  match lines with
  | line :: tail ->
    let set, x, y = process_row set x y idx 0 line in
    read_grid set x y (idx + 1) tail
  | [] -> set, x, y
;;

let run file part =
  let lines = In_channel.read_lines file in
  let max_row, max_col = List.length lines, String.length (List.hd_exn lines) in
  let lines = List.map lines ~f:String.to_list in
  let obstacles, start_y, start_x =
    read_grid (Set.empty (module Pair)) 0 0 0 lines
  in
  if part = 1
  then part_one start_y start_x max_row max_col obstacles
  else part_two start_y start_x max_row max_col obstacles
;;
