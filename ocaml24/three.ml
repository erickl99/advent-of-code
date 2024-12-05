open Base
open Stdio
open Angstrom

let number =
  take_while1 Char.is_digit
  >>= fun digits ->
  if String.length digits <= 3
  then return (Int.of_string digits)
  else fail "Number has more than 3 digits"
;;

let mul_parser =
  string "mul(" *> number
  >>= fun x -> char ',' *> number >>= fun y -> char ')' *> return (x * y)
;;

let part_one =
  let ignore_corrupt = take_while (fun x -> not (Char.equal x 'm')) in
  many (ignore_corrupt *> mul_parser <|> take 1 *> ignore_corrupt *> return 0)
;;

let reduce_one = List.fold ~init:0 ~f:(fun acc x -> acc + x)

let rec reduce_two acc state list =
  match list, state with
  | -1 :: tail, _ -> reduce_two acc false tail
  | -2 :: tail, _ -> reduce_two acc true tail
  | x :: tail, true -> reduce_two (acc + x) true tail
  | _ :: tail, false -> reduce_two acc false tail
  | _ -> acc
;;

let part_two =
  let ignore_corrupt_two =
    take_while (fun x -> not (Char.equal x 'm' || Char.equal x 'd'))
  in
  let keyword =
    mul_parser
    <|> string "don't()" *> return (-1)
    <|> string "do()" *> return (-2)
  in
  many
    (ignore_corrupt_two *> keyword <|> take 1 *> ignore_corrupt_two *> return 0)
;;

let run file part =
  let memory = In_channel.read_all file in
  let parser, reducer =
    if part = 1 then part_one, reduce_one else part_two, reduce_two 0 true
  in
  match parse_string ~consume:All parser memory with
  | Ok collected -> reducer collected
  | Error _ -> failwith "Parsing error"
;;
