open Base
open Stdio

type point =
  { x : int
  ; y : int
  }

module Pair = struct
  module T = struct
    type t = point

    let compare left right =
      if left.x <> right.x then left.x - right.x else left.y - right.y
    ;;

    let sexp_of_t p = Sexp.List [ sexp_of_int p.x; sexp_of_int p.y ]
    let hash p = Int.hash p.x + Int.hash p.y
  end

  include T
  include Comparator.Make (T)
end

let deltas = [ -1, 0; 0, 1; 1, 0; 0, -1 ]

let rec part_one plant position visited perimeter grid =
  List.iter deltas ~f:(fun delta ->
    let delta_x, delta_y = delta in
    let next = { x = position.x + delta_x; y = position.y + delta_y } in
    if next.x < 0
       || next.x >= Array.length grid.(0)
       || next.y < 0
       || next.y >= Array.length grid
       || (not @@ Char.equal plant grid.(next.y).(next.x))
    then Int.incr perimeter
    else if not @@ Hash_set.mem visited next
    then (
      Hash_set.add visited next;
      part_one plant next visited perimeter grid))
;;

let corner_deltas =
  [ { x = 0; y = -1 }, { x = 1; y = 0 }, { x = 1; y = -1 }
  ; { x = 1; y = 0 }, { x = 0; y = 1 }, { x = 1; y = 1 }
  ; { x = 0; y = 1 }, { x = -1; y = 0 }, { x = -1; y = 1 }
  ; { x = -1; y = 0 }, { x = 0; y = -1 }, { x = -1; y = -1 }
  ]
;;

let is_border plant position grid =
  position.x < 0
  || position.x >= Array.length grid.(0)
  || position.y < 0
  || position.y >= Array.length grid
  || (not @@ Char.equal plant grid.(position.y).(position.x))
;;

let count_vertices plant position grid =
  List.fold corner_deltas ~init:0 ~f:(fun acc delta ->
    let delta_one, delta_two, diag = delta in
    let neighbor_one =
      { x = position.x + delta_one.x; y = position.y + delta_one.y }
    in
    let neighbor_two =
      { x = position.x + delta_two.x; y = position.y + delta_two.y }
    in
    let diag_neighbor = { x = position.x + diag.x; y = position.y + diag.y } in
    if is_border plant neighbor_one grid && is_border plant neighbor_two grid
    then acc + 1
    else if (not @@ is_border plant neighbor_one grid)
            && (not @@ is_border plant neighbor_two grid)
            && is_border plant diag_neighbor grid
    then acc + 1
    else acc)
;;

let diag_deltas = [ -1, 1; 1, 1; 1, -1; -1, -1 ]

let count_vertices_bad plant position grid =
  let borders =
    List.fold deltas ~init:[] ~f:(fun acc delta ->
      let delta_y, delta_x = delta in
      let next = { x = position.x + delta_x; y = position.y + delta_y } in
      let is_border =
        next.x < 0
        || next.x >= Array.length grid.(0)
        || next.y < 0
        || next.y >= Array.length grid
        || (not @@ Char.equal plant grid.(next.y).(next.x))
      in
      is_border :: acc)
  in
  let diags =
    List.fold diag_deltas ~init:[] ~f:(fun acc delta ->
      let delta_y, delta_x = delta in
      let next = { x = position.x + delta_x; y = position.y + delta_y } in
      let is_diag =
        next.x >= 0
        && next.x < Array.length grid.(0)
        && next.y >= 0
        && next.y < Array.length grid
        && (not @@ Char.equal plant grid.(next.y).(next.x))
      in
      is_diag :: acc)
  in
  let count_true list = List.count list ~f:(fun x -> x) in
  match List.rev borders, List.rev diags with
  (* single square *)
  | x, _ when count_true x = 4 -> 4
  (* three borders *)
  | x, _ when count_true x = 3 -> 2
  (* no borders, but still need to check diagonals *)
  | x, _ when count_true x = 0 -> count_true diags
  (* top right corner, check if bottom left is in garden*)
  | [ true; true; false; false ], [ _; _; x; _ ] -> if x then 2 else 1
  (* top left corner, check if bottom right is in garden *)
  | [ true; false; false; true ], [ _; x; _; _ ] -> if x then 2 else 1
  (* bottom right corner,  check if top left is in garden*)
  | [ false; true; true; false ], [ _; _; _; x ] -> if x then 2 else 1
  (* bottom left corner, check if top right is in garden *)
  | [ false; false; true; true ], [ x; _; _; _ ] -> if x then 2 else 1
  (* Left and right are inside garden *)
  | [ true; false; true; false ], _ -> 0
  (* up and down are inside garden *)
  | [ false; true; false; true ], _ -> 0
  (* top is a border, need to check bottom diagonals *)
  | [ true; false; false; false ], [ _; one; two; _ ] -> count_true [ one; two ]
  (* right is a border, need to check left diagonals *)
  | [ false; true; false; false ], [ _; _; one; two ] -> count_true [ one; two ]
  (* bottom is a border, need to check top diagonals *)
  | [ false; false; true; false ], [ one; _; _; two ] -> count_true [ one; two ]
  (* left is a border, need to check right diagonals *)
  | [ false; false; false; true ], [ one; two; _; _ ] -> count_true [ one; two ]
  | _ -> 0
;;

let rec part_two plant position visited vertices grid =
  let counted = count_vertices plant position grid in
  vertices := !vertices + counted;
  List.iter deltas ~f:(fun delta ->
    let delta_y, delta_x = delta in
    let next = { x = position.x + delta_x; y = position.y + delta_y } in
    if next.x >= 0
       && next.x < Array.length grid.(0)
       && next.y >= 0
       && next.y < Array.length grid
       && Char.equal plant grid.(next.y).(next.x)
       && (not @@ Hash_set.mem visited next)
    then (
      Hash_set.add visited next;
      part_two plant next visited vertices grid))
;;

let run file part =
  let grid =
    In_channel.read_lines file |> List.map ~f:String.to_array |> List.to_array
  in
  let visited = Hash_set.create (module Pair) in
  let rows, cols = Array.length grid - 1, Array.length grid.(0) - 1 in
  let pricer = if part = 1 then part_one else part_two in
  let result = ref 0 in
  for i = 0 to rows do
    for j = 0 to cols do
      let plant, point = grid.(i).(j), { x = j; y = i } in
      if not @@ Hash_set.mem visited point
      then (
        let original = Hash_set.length visited in
        let other = ref 0 in
        Hash_set.add visited point;
        pricer plant point visited other grid;
        result := !result + (!other * (Hash_set.length visited - original)))
    done
  done;
  !result
;;
