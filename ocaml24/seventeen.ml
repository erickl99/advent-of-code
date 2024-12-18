open Base
open Stdio
open Angstrom

let number =
  take_while1 Char.is_digit >>= fun digits -> return (Int.of_string digits)
;;

let computer =
  let* register_a = string "Register A: " *> number <?> "register a" in
  let* register_b = string "Register B: " *> number <?> "register c" in
  let* register_c = string "Register C: " *> number <?> "register c" in
  let* instructions = string "Program: " *> sep_by (char ',') number in
  return (register_a, register_b, register_c, instructions)
;;

type state =
  { isp : int
  ; ra : int
  ; rb : int
  ; rc : int
  }

let get_operand computer program =
  match program.(computer.isp + 1) with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> computer.ra
  | 5 -> computer.rb
  | 6 -> computer.rc
  | _ -> failwith "Invalid operand"
;;

let print_state computer =
  printf
    "(isp:%d ra:%d rb:%d rc:%d)\n"
    computer.isp
    computer.ra
    computer.rb
    computer.rc;
  Stdlib.flush Out_channel.stdout
;;

let rec execute_program output computer program =
  if computer.isp >= Array.length program
  then output
  else (
    let updated, output =
      match program.(computer.isp) with
      | 0 ->
        let operand = get_operand computer program in
        let value = computer.ra / Int.pow 2 operand in
        { computer with ra = value; isp = computer.isp + 2 }, output
      | 1 ->
        let value = Int.bit_xor computer.rb program.(computer.isp + 1) in
        { computer with rb = value; isp = computer.isp + 2 }, output
      | 2 ->
        let operand = get_operand computer program in
        let value = operand % 8 in
        { computer with rb = value; isp = computer.isp + 2 }, output
      | 3 ->
        let new_address =
          if computer.ra = 0
          then computer.isp + 2
          else program.(computer.isp + 1)
        in
        { computer with isp = new_address }, output
      | 4 ->
        let value = Int.bit_xor computer.rb computer.rc in
        { computer with rb = value; isp = computer.isp + 2 }, output
      | 5 ->
        let operand = get_operand computer program in
        let value = operand % 8 in
        { computer with isp = computer.isp + 2 }, value :: output
      | 6 ->
        let operand = get_operand computer program in
        let value = computer.ra / Int.pow 2 operand in
        { computer with rb = value; isp = computer.isp + 2 }, output
      | 7 ->
        let operand = get_operand computer program in
        let value = computer.ra / Int.pow 2 operand in
        { computer with rc = value; isp = computer.isp + 2 }, output
      | _ -> failwith "Invalid opcode"
    in
    execute_program output updated program)
;;

let to_binary n =
  let rec aux n acc =
    if n = 0 then acc else aux (n / 2) (Char.(of_int_exn ((n % 2) + 48)) :: acc)
  in
  if n = 0
  then String.pad_left ~char:'0' "0" ~len:64
  else (
    let result = String.of_char_list (aux n []) in
    String.pad_left result ~char:'0' ~len:64)
;;

let rec execute_all starting ending program =
  if starting <= ending
  then (
    let output =
      execute_program [] { isp = 0; ra = starting; rb = 0; rc = 0 } program
      |> List.rev
    in
    printf "For register A=%s:" (to_binary starting);
    List.iter output ~f:(printf " %d");
    printf "\n";
    execute_all (starting + 1) ending program)
;;


let rec execute_bit_shift starting idx ending program =
  if idx <= ending
  then (
    let init = Int.shift_left idx 38 in
    let output =
      execute_program
        []
        { isp = 0; ra = starting + init; rb = 0; rc = 0 }
        program
      |> List.rev
    in
    printf "For register A=%s:" (to_binary (starting + init));
    List.iter output ~f:(printf " %d");
    printf "\n";
    execute_bit_shift starting (idx + 1) ending program)
;;

let lowest_geuss = 168601598991
let added = Int.pow 2 14

let part_one input =
  let start = lowest_geuss in
  match parse_string ~consume:All computer input with
  | Ok (_, _, _, program) ->
    execute_bit_shift start 0 (added) (List.to_array program);
    69
  | Error msg -> failwith msg
;;

let run file _ =
  let input = In_channel.read_lines file |> String.concat in
  part_one input
;;
