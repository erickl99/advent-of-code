open Base
open Stdio

type point =
  { x : int
  ; y : int
  }

module Point = struct
  module T = struct
    type t = point

    let compare one two =
      if one.x <> two.x then one.x - two.x else one.y - two.y
    ;;

    let sexp_of_t p = Sexp.List [ sexp_of_int p.x; sexp_of_int p.y ]
    let hash p = Int.hash p.x + Int.hash p.y
  end

  include T
  include Comparator.Make (T)
end

let deltas = [ -1, 0; 0, 1; 1, 0; 0, -1 ]

let rec collect_paths position grid =
  if grid.(position.y).(position.x) = 9
  then 1
  else
    List.fold deltas ~init:0 ~f:(fun acc delta ->
      let delta_y, delta_x = delta in
      let next_x, next_y = position.x + delta_x, position.y + delta_y in
      if next_x >= 0
         && next_x < Array.length grid.(0)
         && next_y >= 0
         && next_y < Array.length grid
         && grid.(next_y).(next_x) - grid.(position.y).(position.x) = 1
      then acc + collect_paths { x = next_x; y = next_y } grid
      else acc)
;;

let part_two grid =
  Array.foldi grid ~init:0 ~f:(fun row_idx total row ->
    total
    + Array.foldi row ~init:0 ~f:(fun col_idx acc height ->
      if height = 0
      then acc + collect_paths { x = col_idx; y = row_idx } grid
      else acc))
;;

let rec search_path position trails visited grid =
  if grid.(position.y).(position.x) = 9
  then Hash_set.add trails position
  else if not @@ Hash_set.mem visited position
  then
    List.iter deltas ~f:(fun delta ->
      let delta_y, delta_x = delta in
      let next_x, next_y = position.x + delta_x, position.y + delta_y in
      if next_x >= 0
         && next_x < Array.length grid.(0)
         && next_y >= 0
         && next_y < Array.length grid
         && grid.(next_y).(next_x) - grid.(position.y).(position.x) = 1
      then search_path { x = next_x; y = next_y } trails visited grid);
  Hash_set.add visited position
;;

let part_one grid =
  Array.foldi grid ~init:0 ~f:(fun row_idx total row ->
    total
    + Array.foldi row ~init:0 ~f:(fun col_idx acc height ->
      if height = 0
      then (
        let trails = Hash_set.create (module Point) in
        let visited = Hash_set.create (module Point) in
        search_path { x = col_idx; y = row_idx } trails visited grid;
        Hash_set.length trails + acc)
      else acc))
;;

let run file part =
  let grid =
    In_channel.read_lines file
    |> List.map ~f:(fun line ->
      String.to_array line
      |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0'))
    |> List.to_array
  in
  if part = 1 then part_one grid else part_two grid
;;
