open Base

let numre = Str.regexp {|[1-9]|}

let re2 =
  Str.regexp {|[1-9]\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|}
;;

let extract_int str =
  match str with
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | _ -> Int.of_string str
;;

let get_first str re =
  let _ = Str.search_forward re str 0 in
  extract_int (Str.matched_string str)
;;

let get_last str re =
  let len = String.length str in
  let _ = Str.search_backward re str len - 1 in
  extract_int (Str.matched_string str)
;;

let calibrate_one str =
  let d_one, d_two = get_first str numre, get_last str numre in
  let num = (10 * d_one) + d_two in
  num
;;

let calibrate_two str =
  let d_one, d_two = get_first str re2, get_last str re2 in
  let num = (10 * d_one) + d_two in
  num
;;
