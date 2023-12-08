open Base
open Stdio

let letter_re = Str.regexp {|[^A-Z]+|}

type node =
  { left : string
  ; right : string
  }

let process_line map line =
  match Str.split letter_re line with
  | [ node; left; right ] -> Map.set map ~key:node ~data:{ left; right }
  | _ -> map
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
    let node_map =
      In_channel.fold_lines
        file
        ~init:(Map.empty (module String))
        ~f:process_line
    in
    traverse_map 0 directions node_map "AAA"
  | None -> 0
;;
