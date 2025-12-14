open Base
open Stdio

let max_joltage bank limit =
  let bank_size = Array.length bank in
  let current = Array.create 0 ~len:bank_size in
  let maxes = Array.create 0 ~len:(bank_size + 1) in
  let max = ref 0 in
  for x = limit - 1 downto 0 do
    for i = bank_size - (limit - x) downto x do
      current.(i) <- (bank.(i) * Int.pow 10 (limit - 1 - x)) + maxes.(i + 1)
    done;
    for i = bank_size - (limit - x) downto x do
      if current.(i) > !max then max := current.(i);
      maxes.(i) <- !max
    done;
    max := 0
  done;
  Array.max_elt current ~compare:Int.compare |> Option.value_exn
;;

let run file_name =
  In_channel.with_file file_name ~f:(fun file ->
    In_channel.fold_lines
      file
      ~init:(0, 0)
      ~f:(fun (result_one, result_two) line ->
        let bank =
          Array.init (String.length line) ~f:(fun idx ->
            String.get line idx |> Char.get_digit |> Option.value_exn)
        in
        let one, two = max_joltage bank 2, max_joltage bank 12 in
        result_one + one, result_two + two))
;;
