open Base
open Stdio

let modulus = 100

let run file_name =
  let location = ref 50 in
  In_channel.with_file file_name ~f:(fun file ->
    In_channel.fold_lines
      file
      ~init:(0, 0)
      ~f:(fun (result_one, result_two) line ->
        let num = Int.of_string (String.drop_prefix line 1) in
        let quotient, shift = num / modulus, num % modulus in
        let sign, passed_zero =
          match String.get line 0 with
          | 'L' -> -1, if !location > 0 && !location - shift < 0 then 1 else 0
          | 'R' -> 1, if !location + shift > 100 then 1 else 0
          | _ -> failwith "Invalid direction given!"
        in
        location := (!location + (shift * sign)) % modulus;
        let at_zero = if !location = 0 then 1 else 0 in
        result_one + at_zero, result_two + at_zero + passed_zero + quotient))
;;
