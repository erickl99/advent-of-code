open Base
open Stdio

let letter_re = Str.regexp {|[^A-Z]+|}

type node =
  { left : string
  ; right : string
  }

let add_node map line start_nodes =
  match Str.split letter_re line with
  | [ node; left; right ] ->
    let updated_nodes =
      if Char.equal (String.get node 2) 'A'
      then node :: start_nodes
      else start_nodes
    in
    Map.set map ~key:node ~data:{ left; right }, updated_nodes
  | _ -> map, start_nodes
;;

let rec traverse_map idx directions map = function
  | "ZZZ" -> 0
  | node_name ->
    let next_idx = (idx + 1) % String.length directions in
    (match String.get directions idx with
     | 'L' ->
       let next_node = (Map.find_exn map node_name).left in
       1 + traverse_map next_idx directions map next_node
     | 'R' ->
       let next_node = (Map.find_exn map node_name).right in
       1 + traverse_map next_idx directions map next_node
     | _ -> 0)
;;

let process_file file =
  match In_channel.input_line file with
  | Some directions ->
    ignore (In_channel.input_line file);
    let process_line map line =
      let new_map, _ = add_node map line [] in
      new_map
    in
    let node_map =
      In_channel.fold_lines
        file
        ~init:(Map.empty (module String))
        ~f:process_line
    in
    traverse_map 0 directions node_map "AAA"
  | None -> 0
;;

let process_line acc line =
  let map, start_nodes = acc in
  match Str.split letter_re line with
  | [ node; left; right ] ->
    let updated_nodes =
      if Char.equal (String.get node 2) 'A'
      then node :: start_nodes
      else start_nodes
    in
    Map.set map ~key:node ~data:{ left; right }, updated_nodes
  | _ -> map, start_nodes
;;

let rec traverse_map2 idx directions map current_node =
  if Char.equal (String.get current_node 2) 'Z'
  then 0
  else (
    let next_idx = (idx + 1) % String.length directions in
    match String.get directions idx with
    | 'L' ->
      let next_node = (Map.find_exn map current_node).left in
      1 + traverse_map2 next_idx directions map next_node
    | 'R' ->
      let next_node = (Map.find_exn map current_node).right in
      1 + traverse_map2 next_idx directions map next_node
    | _ -> 0)
;;

let process_file2 file =
  match In_channel.input_line file with
  | Some directions ->
    ignore (In_channel.input_line file);
    let node_map, start_nodes =
      In_channel.fold_lines
        ~init:(Map.empty (module String), [])
        ~f:process_line
        file
    in
    let cycles =
      List.map ~f:(fun x -> traverse_map2 0 directions node_map x) start_nodes
    in
    List.iter ~f:(Stdio.printf "%d ") cycles;
    0
  | None -> 0
;;
