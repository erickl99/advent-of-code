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

let num_pad =
  [| [| '7'; '8'; '9' |]
   ; [| '4'; '5'; '6' |]
   ; [| '1'; '2'; '3' |]
   ; [| 'X'; '0'; 'A' |]
  |]
;;

let arrow_pad = [| [| 'X'; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let inside_pad x y pad =
  y >= 0
  && y < Array.length pad
  && x >= 0
  && x < Array.length pad.(0)
  && (not @@ Char.equal pad.(y).(x) 'X')
;;

let deltas = [| 0, -1, '^'; 1, 0, '>'; 0, 1, 'v'; -1, 0, '<' |]

let rec dfs x y target path all_paths visited grid =
  if Char.equal grid.(y).(x) target
  then all_paths := (List.rev path |> String.of_list) :: !all_paths
  else
    Array.iter deltas ~f:(fun (delta_x, delta_y, c) ->
      let next_x, next_y = x + delta_x, y + delta_y in
      if inside_pad next_x next_y grid
         && (not @@ Set.mem visited (next_y, next_x))
      then (
        let updated = Set.add visited (next_y, next_x) in
        dfs next_x next_y target (c :: path) all_paths updated grid))
;;

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
  | _ -> failwith "Invalid char"
;;

let arrow_char_to_pos = function
  | '^' -> 0, 1
  | 'A' -> 0, 2
  | '<' -> 1, 0
  | 'v' -> 1, 1
  | '>' -> 1, 2
  | _ -> failwith "Invalid char"
;;

let rec gen_sequence acc pos grid char_to_pos = function
  | head :: tail ->
    let start_y, start_x = char_to_pos pos in
    let end_y, end_x = char_to_pos head in
    let all_paths = ref [] in
    let visited = Set.of_list (module Pair) [ start_y, start_x ] in
    dfs start_x start_y head [] all_paths visited grid;
    let length = Int.abs (end_y - start_y) + Int.abs (end_x - start_x) in
    let valid_paths =
      List.filter !all_paths ~f:(fun path -> String.length path = length)
    in
    let updated =
      List.fold acc ~init:[] ~f:(fun total prefix ->
        List.fold valid_paths ~init:total ~f:(fun acc path ->
          (prefix ^ path ^ "A") :: acc))
    in
    gen_sequence updated head grid char_to_pos tail
  | [] -> acc
;;

let arrow_char_to_delta = function
  | '^' -> 0, -1
  | '<' -> -1, 0
  | 'v' -> 0, 1
  | '>' -> 1, 0
  | _ -> failwith "Invalid char"
;;

let pos_to_num_char = function
  | 0, 1 -> '^'
  | 0, 2 -> 'A'
  | 1, 0 -> '<'
  | 1, 1 -> 'v'
  | 1, 2 -> '>'
  | _ -> failwith "Invalid pair"
;;

let rec revert acc x y = function
  | head :: tail ->
    if Char.equal head 'A'
    then (
      let updated = pos_to_num_char (y, x) :: acc in
      revert updated x y tail)
    else (
      let delta_x, delta_y = arrow_char_to_delta head in
      revert acc (x + delta_x) (y + delta_y) tail)
  | [] -> List.rev acc |> String.of_list
;;

let collect_sequence acc sequence =
  let generated =
    gen_sequence
      [ "" ]
      'A'
      arrow_pad
      arrow_char_to_pos
      (String.to_list sequence)
  in
  acc @ generated
;;

let get_complexity code =
  let chars = String.to_list code in
  let first = gen_sequence [ "" ] 'A' num_pad num_char_to_pos chars in
  let second = List.fold first ~init:[] ~f:collect_sequence in
  let all_sequences = List.fold second ~init:[] ~f:collect_sequence in
  let min_sequence =
    Option.value_exn
    @@ List.min_elt all_sequences ~compare:(fun left right ->
      String.length left - String.length right)
  in
  let numeric = String.sub code ~pos:0 ~len:(String.length code - 1) in
  let len = String.length min_sequence in
  let num = Int.of_string numeric in
  len * num
;;

let run _file _ =
  let codes = In_channel.read_lines _file in
  List.fold codes ~init:0 ~f:(fun acc code -> acc + get_complexity code)
;;
