open Base
open Stdio

module Pair = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) = if x1 <> x2 then x1 - x2 else y1 - y2
    let sexp_of_t (x, y) = Sexp.List [ sexp_of_int x; sexp_of_int y ]
    let hash (x, y) = Int.hash x + Int.hash y
  end

  include T
  include Comparator.Make (T)
end

let distance (y_one, x_one) (y_two, x_two) =
  Int.abs (y_two - y_one) + Int.abs (x_two - x_one)
;;

let find_time_saves path min_save cheat_time =
  let path_array = List.to_array path in
  let length = Array.length path_array in
  let num_time_saves = ref 0 in
  for x = 0 to length - 1 do
    for y = x + min_save to length - 1 do
      let shortest_path = distance path_array.(x) path_array.(y) in
      let time_save = y - x - shortest_path in
      if time_save >= min_save && shortest_path <= cheat_time
      then Int.incr num_time_saves
    done
  done;
  !num_time_saves
;;

let inside_grid x y grid =
  x >= 0
  && x < Array.length grid.(0)
  && y >= 0
  && y < Array.length grid
  && (not @@ Char.equal grid.(y).(x) '#')
;;

let visited x y = function
  | (prev_y, prev_x) :: _ -> x = prev_x && y = prev_y
  | [] -> false
;;

let deltas = [| 0, -1; 1, 0; 0, 1; -1, 0 |]

let rec build_path acc point map =
  let y, x = Hashtbl.find_exn map point in
  if x = -1 && y = -1 then acc else build_path ((y, x) :: acc) (y, x) map
;;

let bfs start_x start_y grid =
  let queue = Queue.create () in
  let visited = Hashtbl.create (module Pair) in
  let ending = ref (0, 0) in
  Queue.enqueue queue (start_y, start_x);
  Hashtbl.set visited ~key:(start_y, start_x) ~data:(-1, -1);
  while not @@ Queue.is_empty queue do
    let size = Queue.length queue in
    for _ = 1 to size do
      let y, x = Queue.dequeue_exn queue in
      Array.iter deltas ~f:(fun (delta_x, delta_y) ->
        let next_x, next_y = x + delta_x, y + delta_y in
        if inside_grid next_x next_y grid
           && (not @@ Hashtbl.mem visited (next_y, next_x))
        then (
          if Char.equal grid.(next_y).(next_x) 'E' then ending := next_y, next_x;
          Hashtbl.set visited ~key:(next_y, next_x) ~data:(y, x);
          Queue.enqueue queue (next_y, next_x)))
    done
  done;
  build_path [ !ending ] !ending visited
;;

let rec search_row idx row =
  if idx = Array.length row
  then None
  else (
    let cell = Array.get row idx in
    if Char.equal cell 'S' then Some idx else search_row (idx + 1) row)
;;

let rec find_start grid row_idx =
  if row_idx = Array.length grid
  then failwith "Could not find start position"
  else (
    match search_row 0 (Array.get grid row_idx) with
    | Some col_idx -> row_idx, col_idx
    | None -> find_start grid (row_idx + 1))
;;

let run file part =
  let grid =
    In_channel.read_lines file |> List.map ~f:String.to_array |> List.to_array
  in
  let start_y, start_x = find_start grid 0 in
  let path = bfs start_x start_y grid in
  let testing = String.is_substring file ~substring:"test" in
  let min_save, cheat_time =
    match part, testing with
    | 1, true -> 1, 2
    | 1, false -> 100, 2
    | 2, true -> 50, 20
    | 2, false -> 100, 20
    | _ -> failwith "Invalid combination"
  in
  find_time_saves path min_save cheat_time
;;
