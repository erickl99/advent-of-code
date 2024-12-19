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
    if n = 0 then acc else aux (n / 2) (Char.of_int_exn ((n % 2) + 48) :: acc)
  in
  if n = 0 then "0" else String.of_char_list (aux n [])
;;

let rec recover_register value depth program remaining =
  let rec check_values iter target rest =
    if iter = 8
    then None
    else (
      let start = (8 * value) + iter in
      let output =
        execute_program [] { isp = 0; ra = start; rb = 0; rc = 0 } program
      in
      if List.nth_exn output depth = target
      then (
        match recover_register start (depth + 1) program rest with
        | Some result -> Some result
        | None -> check_values (iter + 1) target rest)
      else check_values (iter + 1) target rest)
  in
  match remaining with
  | [] -> Some value
  | target :: rest -> check_values 0 target rest
;;

let part_two program =
  let reversed = List.rev program in
  let program = List.to_array program in
  Option.value_exn @@ recover_register 0 0 program reversed
;;

let part_one ra rb rc program =
  let output = execute_program [] { isp = 0; ra; rb; rc } program |> List.rev in
  List.iter output ~f:(printf "%d,");
  printf "\n";
  List.length output
;;

let run file part =
  let input = In_channel.read_lines file |> String.concat in
  match parse_string ~consume:All computer input with
  | Ok (ra, rb, rc, program) ->
    if part = 1
    then part_one ra rb rc (List.to_array program)
    else part_two program
  | Error msg -> failwith msg
;;
