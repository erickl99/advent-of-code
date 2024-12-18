open Base
open Stdio

let inside_maze idx grid =
  let rows, cols = Array.length grid, Array.length grid.(0) in
  let row_idx, col_idx = idx / cols, idx % cols in
  row_idx >= 0
  && row_idx < rows
  && col_idx >= 0
  && col_idx < cols
  && (not @@ Char.equal grid.(row_idx).(col_idx) '#')
;;

let solve grid part =
  let rows, cols = Array.length grid, Array.length grid.(0) in
  let deltas = [| -cols; 1; cols; -1 |] in
  let num_nodes = rows * cols in
  let distances = Array.init (4 * num_nodes) ~f:(fun _ -> Int.max_value) in
  let predecessor = Array.init (4 * num_nodes) ~f:(fun _ -> []) in
  let queue = Queue.create () in
  let start_x, start_y = 1, rows - 2 in
  let start_idx = num_nodes + (start_y * cols) + start_x in
  distances.(start_idx) <- 0;
  Queue.enqueue queue start_idx;
  while not @@ Queue.is_empty queue do
    let abs_idx = Queue.dequeue_exn queue in
    let direction_idx = abs_idx / num_nodes in
    let rel_idx = abs_idx % num_nodes in
    let cost = distances.(abs_idx) in
    Array.iteri deltas ~f:(fun delta_idx delta ->
      let next_idx = rel_idx + delta in
      if inside_maze next_idx grid
      then (
        let new_cost =
          if direction_idx = delta_idx then cost + 1 else cost + 1001
        in
        let new_idx = (num_nodes * delta_idx) + next_idx in
        if new_cost < distances.(new_idx)
        then (
          distances.(new_idx) <- new_cost;
          predecessor.(new_idx) <- [ abs_idx ];
          Queue.enqueue queue new_idx)
        else if new_cost = distances.(new_idx)
        then (
          let updated = abs_idx :: predecessor.(new_idx) in
          predecessor.(new_idx) <- updated)))
  done;
  let north_idx = cols + cols - 2 in
  let east_idx = num_nodes + cols + cols - 2 in
  let min_path, end_idx =
    if distances.(north_idx) < distances.(east_idx)
    then distances.(north_idx), north_idx
    else distances.(east_idx), east_idx
  in
  let rec get_shortest_paths index =
    if index = start_idx
    then [ [ start_idx ] ]
    else
      List.fold predecessor.(index) ~init:[] ~f:(fun all_paths pred ->
        let pred_paths = get_shortest_paths pred in
        List.fold pred_paths ~init:all_paths ~f:(fun acc path ->
          (index :: path) :: acc))
  in
  let idx_to_string idx =
    let row_idx, col_idx = idx % num_nodes / cols, idx % num_nodes % cols in
    "(" ^ Int.to_string row_idx ^ "," ^ Int.to_string col_idx ^ ")"
  in
  let init = Set.empty (module String) in
  let shortest_paths = get_shortest_paths end_idx in
  let unique =
    List.fold shortest_paths ~init ~f:(fun total path ->
      List.fold path ~init:total ~f:(fun acc idx ->
        Set.add acc (idx_to_string idx)))
  in
  if part = 1 then min_path else Set.length unique
;;

let run file part =
  let grid =
    In_channel.read_lines file |> List.map ~f:String.to_array |> List.to_array
  in
  solve grid part
;;
