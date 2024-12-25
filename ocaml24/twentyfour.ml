open Base
open Stdio

type wire =
  | Literal of int
  | Variable of string

type gate =
  | And of wire * wire
  | Or of wire * wire
  | Xor of wire * wire

let rec get_initial_values map = function
  | [] -> failwith "Reached end of input"
  | line :: tail ->
    if String.length line = 0
    then map, tail
    else (
      match String.split_on_chars line ~on:[ ':'; ' ' ] with
      | [ wire; ""; value ] ->
        let updated_map = Map.set map ~key:wire ~data:(Int.of_string value) in
        get_initial_values updated_map tail
      | _ -> failwith "Invalid input")
;;

let build_wire input init_values =
  match Map.find init_values input with
  | Some value -> Literal value
  | None -> Variable input
;;

let rec build_gates map init_values = function
  | [] -> map
  | line :: tail ->
    (match String.split line ~on:' ' with
     | [ left; "AND"; right; "->"; target ] ->
       let left_input, right_input =
         build_wire left init_values, build_wire right init_values
       in
       let gate = And (left_input, right_input) in
       let updated_map = Map.set map ~key:target ~data:gate in
       build_gates updated_map init_values tail
     | [ left; "OR"; right; "->"; target ] ->
       let left_input, right_input =
         build_wire left init_values, build_wire right init_values
       in
       let gate = Or (left_input, right_input) in
       let updated_map = Map.set map ~key:target ~data:gate in
       build_gates updated_map init_values tail
     | [ left; "XOR"; right; "->"; target ] ->
       let left_input, right_input =
         build_wire left init_values, build_wire right init_values
       in
       let gate = Xor (left_input, right_input) in
       let updated_map = Map.set map ~key:target ~data:gate in
       build_gates updated_map init_values tail
     | _ -> failwith "Invalid input")
;;

let rec evaluate gates = function
  | And (Literal x, Literal y) -> Int.bit_and x y
  | And (Literal x, Variable y) ->
    let y_value = Map.find_exn gates y in
    Int.bit_and x (evaluate gates y_value)
  | And (Variable x, Literal y) ->
    let x_value = Map.find_exn gates x in
    Int.bit_and (evaluate gates x_value) y
  | And (Variable x, Variable y) ->
    let x_value = Map.find_exn gates x in
    let y_value = Map.find_exn gates y in
    Int.bit_and (evaluate gates x_value) (evaluate gates y_value)
  | Or (Literal x, Literal y) -> Int.bit_or x y
  | Or (Literal x, Variable y) ->
    let y_value = Map.find_exn gates y in
    Int.bit_or x (evaluate gates y_value)
  | Or (Variable x, Literal y) ->
    let x_value = Map.find_exn gates x in
    Int.bit_or (evaluate gates x_value) y
  | Or (Variable x, Variable y) ->
    let x_value = Map.find_exn gates x in
    let y_value = Map.find_exn gates y in
    Int.bit_or (evaluate gates x_value) (evaluate gates y_value)
  | Xor (Literal x, Literal y) -> Int.bit_xor x y
  | Xor (Literal x, Variable y) ->
    let y_value = Map.find_exn gates y in
    Int.bit_xor x (evaluate gates y_value)
  | Xor (Variable x, Literal y) ->
    let x_value = Map.find_exn gates x in
    Int.bit_xor (evaluate gates x_value) y
  | Xor (Variable x, Variable y) ->
    let x_value = Map.find_exn gates x in
    let y_value = Map.find_exn gates y in
    Int.bit_xor (evaluate gates x_value) (evaluate gates y_value)
;;

let of_binary =
  List.fold ~init:0 ~f:(fun acc bit ->
    if bit = 1 then (2 * acc) + 1 else 2 * acc)
;;

let part_one gates num_bits =
  let target_gates =
    List.init num_bits ~f:(fun idx ->
      if idx < 10 then "z0" ^ Int.to_string idx else "z" ^ Int.to_string idx)
  in
  let bits =
    List.map target_gates ~f:(fun target ->
      evaluate gates (Map.find_exn gates target))
    |> List.rev
  in
  of_binary bits
;;

let part_two values _gates _num_bits =
  let x_bits =
    Map.fold values ~init:[] ~f:(fun ~key ~data acc ->
      if String.is_substring key ~substring:"x" then data :: acc else acc)
  in
  let y_bits =
    Map.fold values ~init:[] ~f:(fun ~key ~data acc ->
      if String.is_substring key ~substring:"y" then data :: acc else acc)
  in
  let x, y = of_binary x_bits, of_binary y_bits in
  x + y
;;

let run file part =
  let lines = In_channel.read_lines file in
  let init_values = Map.empty (module String) in
  let values, rest = get_initial_values init_values lines in
  let init_gates = Map.empty (module String) in
  let gates = build_gates init_gates values rest in
  let num_bits =
    if String.is_substring file ~substring:"test" then 13 else 46
  in
  if part = 1 then part_one gates num_bits else part_two values gates num_bits
;;
