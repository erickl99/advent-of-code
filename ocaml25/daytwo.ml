open Base
open Stdio

let num_digits num =
  Float.of_int num
  |> Float.log10
  |> Float.add 1.
  |> Float.round_down
  |> Int.of_float
;;

let twice num =
  let digits = num_digits num in
  if digits % 2 = 1
  then false
  else (
    let multiple = 1 + Int.pow 10 (digits / 2) in
    0 = Int.rem num multiple)
;;

let residues num base =
  let init = num - (num % base) in
  Sequence.unfold ~init ~f:(function
    | x when x < base -> None
    | x -> Some (x / base, x / base))
;;

let repeating num =
  let digits = num_digits num in
  Sequence.range 1 ((digits / 2) + 1)
  |> Sequence.exists ~f:(fun base ->
    Int.rem digits base = 0
    &&
    let base = Int.pow 10 base in
    let remainder = num % base in
    Sequence.for_all (residues num base) ~f:(fun x -> x % base = remainder))
;;

let run file_name =
  let line = String.drop_suffix (In_channel.read_all file_name) 1 in
  let intervals = String.split line ~on:',' in
  List.fold intervals ~init:(0, 0) ~f:(fun (result_one, result_two) interval ->
    let left, right = String.lsplit2_exn interval ~on:'-' in
    let left, right = Int.of_string left, Int.of_string right in
    let acc_one, acc_two =
      Sequence.range left (right + 1)
      |> Sequence.fold ~init:(0, 0) ~f:(fun (acc_one, acc_two) num ->
        if twice num
        then acc_one + num, acc_two + num
        else if repeating num
        then acc_one, acc_two + num
        else acc_one, acc_two)
    in
    result_one + acc_one, result_two + acc_two)
;;
