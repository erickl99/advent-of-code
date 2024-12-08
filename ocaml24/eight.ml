open Base
open Stdio

type point =
  { x : int
  ; y : int
  }

module Point = struct
  module T = struct
    type t = point

    let compare left right =
      if left.x <> right.x then left.x - right.x else left.y - right.y
    ;;

    let sexp_of_t point = Sexp.List [ sexp_of_int point.x; sexp_of_int point.y ]
  end

  include T
  include Comparator.Make (T)
end

let rec find_antinodes set antenna max_x min_y = function
  | other :: tail ->
    let delta_x, delta_y = other.x - antenna.x, other.y - antenna.y in
    let right_node = { x = other.x + delta_x; y = other.y + delta_y } in
    let left_node = { x = antenna.x - delta_x; y = antenna.y - delta_y } in
    let updated =
      if right_node.x >= 0
         && right_node.x < max_x
         && right_node.y <= 0
         && right_node.y > min_y
      then Set.add set right_node
      else set
    in
    let new_set =
      if left_node.x >= 0
         && left_node.x < max_x
         && left_node.y <= 0
         && left_node.y > min_y
      then Set.add updated left_node
      else updated
    in
    find_antinodes new_set antenna max_x min_y tail
  | [] -> set
;;

let rec part_one acc points max_x min_y =
  match points with
  | head :: tail ->
    let new_acc = find_antinodes acc head max_x min_y tail in
    part_one new_acc tail max_x min_y
  | [] -> acc
;;

let rec gcd a b = if b = 0 then a else gcd b (a % b)

let rec find_antinodes_two set antenna max_x min_y = function
  | other :: tail ->
    let delta_x, delta_y = other.x - antenna.x, other.y - antenna.y in
    let factor = gcd delta_x delta_y in
    let delta_x, delta_y = delta_x / factor, delta_y / factor in
    let new_set =
      let open Sequence in
      let left =
        unfold ~init:antenna ~f:(fun point ->
          let next = { x = point.x - delta_x; y = point.y - delta_y } in
          if next.x >= 0 && next.x < max_x && next.y <= 0 && next.y > min_y
          then Some (next, next)
          else None)
      in
      let right =
        unfold ~init:antenna ~f:(fun point ->
          let next = { x = point.x + delta_x; y = point.y + delta_y } in
          if next.x >= 0 && next.x < max_x && next.y <= 0 && next.y > min_y
          then Some (next, next)
          else None)
      in
      let left_added =
        Sequence.fold left ~init:set ~f:(fun acc point -> Set.add acc point)
      in
      let right_added =
        Sequence.fold right ~init:left_added ~f:(fun acc point ->
          Set.add acc point)
      in
      Set.add right_added antenna
    in
    find_antinodes_two new_set antenna max_x min_y tail
  | [] -> set
;;

let rec part_two acc points max_x min_y =
  match points with
  | head :: tail ->
    let new_acc = find_antinodes_two acc head max_x min_y tail in
    part_two new_acc tail max_x min_y
  | [] -> acc
;;

let find_antennas y acc line =
  String.foldi line ~init:acc ~f:(fun x acc char ->
    if Char.is_alphanum char
    then (
      match Map.find acc char with
      | Some list -> Map.set acc ~key:char ~data:({ x; y = -y } :: list)
      | None -> Map.set acc ~key:char ~data:[ { x; y = -y } ])
    else acc)
;;

let run file part =
  let lines = In_channel.read_lines file in
  let init = Map.empty (module Char) in
  let max_x, min_y = String.length (List.hd_exn lines), -List.length lines in
  let antennas = List.foldi lines ~init ~f:find_antennas in
  let collect_antinodes = if part = 1 then part_one else part_two in
  let antinode_set =
    Map.data antennas
    |> List.fold
         ~init:(Set.empty (module Point))
         ~f:(fun acc antennas -> collect_antinodes acc antennas max_x min_y)
  in
  Set.length antinode_set
;;
