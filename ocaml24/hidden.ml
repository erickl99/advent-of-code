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

let skip_non_mul = skip_while (fun c -> not (Char.equal c 'm')) *> return ()

let all_mul_products =
  many
    (skip_non_mul *> (mul_parser <|> end_of_input *> return 0)
     <|> end_of_input *> return 0)
;;

let parse_and_collect_products input =
  match parse_string ~consume:All all_mul_products input with
  | Ok results -> results
  | Error err -> failwith ("Parsing failed: " ^ err)
;;

let run _ part =
  let input =
    "some random text mul(2, 3) more text mul(10, 4) and mul(123, 5)"
  in
  let result = parse_and_collect_products input in
  List.iter result ~f:(fun x -> printf "%d\n" x);
  printf "\n";
  part
;;
