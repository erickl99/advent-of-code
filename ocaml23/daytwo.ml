open Base

let extract_num str =
  if Char.equal (String.get str 1) ' '
  then Int.of_string (String.prefix str 1)
  else Int.of_string (String.prefix str 2)
;;

let valid_round rnd =
  let num = extract_num rnd in
  let res =
    (String.contains rnd 'g' && num > 13)
    || (String.contains rnd 'd' && num > 12)
    || (String.contains rnd 'b' && num > 14)
  in
  res
;;

let re = Str.regexp ": "
let re2 = Str.regexp ", \\|; "

let game_sum str =
  let tokens = Str.split re str in
  match tokens with
  | [ game; rounds ] ->
    let tokens = Str.split re2 rounds in
    (match List.find ~f:valid_round tokens with
     | Some _ -> 0
     | None -> Int.of_string (String.drop_prefix game 5))
  | _ -> 0
;;

let get_min acc round =
  let redc, greenc, bluec = acc in
  let num = extract_num round in
  if String.contains round 'd'
  then Int.max redc num, greenc, bluec
  else if String.contains round 'g'
  then redc, Int.max greenc num, bluec
  else redc, greenc, Int.max bluec num
;;

let power_sum str =
  let tokens = Str.split re str in
  match tokens with
  | [ _; rounds ] ->
    let tokens = Str.split re2 rounds in
    let redc, greenc, bluec = List.fold tokens ~init:(-1, -1, -1) ~f:get_min in
    redc * greenc * bluec
  | _ -> 0
;;
