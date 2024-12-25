open Base
open Stdio

module Point = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) = if x1 <> x2 then x1 - x2 else y1 - y2
    let sexp_of_t (x, y) = Sexp.List [ sexp_of_int x; sexp_of_int y ]
    let hash (x, y) = Int.hash x + Int.hash y
  end

  include T
  include Comparator.Make (T)
end

let num_pad =
  [| [| '7'; '8'; '9' |]
   ; [| '4'; '5'; '6' |]
   ; [| '1'; '2'; '3' |]
   ; [| 'X'; '0'; 'A' |]
  |]
;;

let num_chars = [| 'A'; '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]

let num_char_to_pos = function
  | '7' -> 0, 0
  | '8' -> 0, 1
  | '9' -> 0, 2
  | '4' -> 1, 0
  | '5' -> 1, 1
  | '6' -> 1, 2
  | '1' -> 2, 0
  | '2' -> 2, 1
  | '3' -> 2, 2
  | '0' -> 3, 1
  | 'A' -> 3, 2
  | _ -> failwith "Invalid char given"
;;

let arrow_pad = [| [| 'X'; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]
let arrow_chars = [| 'A'; '^'; '>'; 'v'; '<' |]

let arrow_char_to_pos = function
  | '^' -> 0, 1
  | 'A' -> 0, 2
  | '<' -> 1, 0
  | 'v' -> 1, 1
  | '>' -> 1, 2
  | _ -> failwith "Invalid char given"
;;

let inside_pad x y pad =
  y >= 0
  && y < Array.length pad
  && x >= 0
  && x < Array.length pad.(0)
  && (not @@ Char.equal pad.(y).(x) 'X')
;;

let deltas = [| 0, -1, '^'; 1, 0, '>'; 0, 1, 'v'; -1, 0, '<' |]

let rec dfs x y start ending len visited paths path pad =
  if Char.equal pad.(y).(x) ending
  then (
    if List.length path = len
    then (
      let key = String.of_list [ start; ending ] in
      let final_path = List.rev path |> String.of_list in
      match Hashtbl.find paths key with
      | Some prev -> Hashtbl.set paths ~key ~data:(final_path :: prev)
      | None -> Hashtbl.set paths ~key ~data:[ final_path ]))
  else
    Array.iter deltas ~f:(fun (delta_x, delta_y, dir) ->
      let next_x, next_y = x + delta_x, y + delta_y in
      if inside_pad next_x next_y pad
         && (not @@ Set.mem visited (next_x, next_y))
      then (
        let new_visited = Set.add visited (next_x, next_y) in
        let updated_path = dir :: path in
        dfs next_x next_y start ending len new_visited paths updated_path pad))
;;

let get_paths grid chars char_to_pos =
  let paths = Hashtbl.create (module String) in
  let len = Array.length chars - 1 in
  for i = 0 to len do
    for j = i to len do
      let start, ending = chars.(i), chars.(j) in
      let start_y, start_x = char_to_pos start in
      let end_y, end_x = char_to_pos ending in
      let len = Int.abs (end_y - start_y) + Int.abs (end_x - start_x) in
      let visited = Set.of_list (module Point) [ start_x, start_y ] in
      dfs start_x start_y start ending len visited paths [] grid;
      let visited = Set.of_list (module Point) [ end_x, end_y ] in
      dfs end_x end_y ending start len visited paths [] grid
    done
  done;
  paths
;;

let rec build_sequence prev sequence all_sequences keypad_graph = function
  | head :: tail ->
    let key = String.of_list [ prev; head ] in
    let valid_paths = Hashtbl.find_exn keypad_graph key in
    List.iter valid_paths ~f:(fun path ->
      let sequence = sequence ^ path ^ String.of_char 'A' in
      build_sequence head sequence all_sequences keypad_graph tail)
  | [] -> Hash_set.add all_sequences sequence
;;

let indent n =
  let times = List.range n 3 in
  List.iter times ~f:(fun _ -> printf "    ")
;;

let rec shortest_sequence keys depth cache keypad_graph =
  if depth = 0
  then (
    indent (-1);
    printf "Answer is %s: %d\n" keys (String.length keys);
    String.length keys)
  else (
    let cache_key = keys ^ " " ^ Int.to_string depth in
    match Hashtbl.find cache cache_key with
    | Some value -> value
    | None ->
      indent depth;
      printf "Computing for %s at depth %d\n" keys depth;
      let subkeys =
        String.split keys ~on:'A'
        |> List.filter ~f:(fun x -> String.length x > 0)
      in
      let result =
        (*Total will sum up min length for each subkey*)
        List.fold subkeys ~init:0 ~f:(fun total subkey ->
          indent (depth - 1);
          printf "Checking subkey %sA\n" subkey;
          let computed =
            let subkey_a = subkey ^ String.of_char 'A' in
            let subkey_sequences = Hash_set.create (module String) in
            build_sequence
              'A'
              ""
              subkey_sequences
              keypad_graph
              (String.to_list subkey_a);
            Hash_set.fold
              subkey_sequences
              ~init:Int.max_value
              ~f:(fun acc subkey_seq ->
                Int.min
                  acc
                  (shortest_sequence subkey_seq (depth - 1) cache keypad_graph))
          in
          indent (depth - 1);
          printf "Got %d for subkey %sA\n" computed subkey;
          total + computed)
      in
      (* printf "Result for key %s is %d\n" keys result; *)
      Hashtbl.set cache ~key:cache_key ~data:result;
      result)
;;

let solve codes max_depth =
  let num_char_paths = get_paths num_pad num_chars num_char_to_pos in
  let arrow_char_paths = get_paths arrow_pad arrow_chars arrow_char_to_pos in
  let cache = Hashtbl.create (module String) in
  let result =
    List.fold codes ~init:0 ~f:(fun total code ->
      printf "Doing code %s\n" code;
      let shortest =
        let init_sequences = Hash_set.create (module String) in
        build_sequence
          'A'
          ""
          init_sequences
          num_char_paths
          (String.to_list code);
        Hash_set.fold ~init:Int.max_value init_sequences ~f:(fun acc sequence ->
          let computed =
            shortest_sequence sequence max_depth cache arrow_char_paths
          in
          indent 1;
          printf "Got %d for sequence %s\n" computed sequence;
          Int.min acc computed)
      in
      let numeric =
        String.sub code ~pos:0 ~len:(String.length code - 1) |> Int.of_string
      in
      let complexity = shortest * numeric in
      total + complexity)
  in
  result
;;

let run file _ =
  let codes = In_channel.read_lines file in
  solve codes 2
;;
