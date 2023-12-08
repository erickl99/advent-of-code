open Base

let whitespace_re = Str.regexp {| +|}
let nums_re = Str.regexp {|[0-9]+|}

let compute_points nums_str win_nums =
  let nums = Str.split whitespace_re nums_str in
  List.map ~f:Int.of_string nums
  |> List.fold ~f:(fun y x -> if Set.mem win_nums x then y + 1 else y) ~init:0
;;

let get_nums nums_str =
  let nums = Str.split whitespace_re nums_str in
  Set.of_list (module Int) (List.map ~f:Int.of_string nums)
;;

let total_points line =
  let tokens = String.split_on_chars ~on:[ ':'; '|' ] line in
  match tokens with
  | [ _; left; right ] ->
    let win_nums = get_nums left in
    let points = compute_points right win_nums in
    if points > 0 then Int.pow 2 (points - 1) else 0
  | _ -> -1
;;

let rec offer stack base num =
  if num = 0 then stack else (base + num) :: offer stack base (num - 1)
;;

let get_matches nums_str win_nums =
  let nums = Str.split whitespace_re nums_str in
  List.map ~f:Int.of_string nums
  |> List.fold ~init:0 ~f:(fun acc x ->
    if Set.mem win_nums x then acc + 1 else acc)
;;

let get_game_num game =
  ignore (Str.search_forward nums_re game 0);
  Str.matched_string game |> Int.of_string
;;

let process_file file_name =
  let rec process_line file map stack =
    let line = In_channel.input_line file in
    match line with
    | Some s ->
      let tokens = String.split_on_chars ~on:[ ':'; '|' ] s in
      (match tokens with
       | [ game; left; right ] ->
         let win_nums = get_nums left in
         let matches = get_matches right win_nums in
         let game_num = get_game_num game in
         process_line
           file
           (Map.set map ~key:game_num ~data:matches)
           (offer stack game_num matches)
       | _ -> map, stack)
    | None -> map, stack
  in
  let map, stack = process_line file_name (Map.empty (module Int)) [] in
  let rec consume_stack acc = function
    | [] -> acc
    | h :: t ->
      (match Map.find map h with
       | None -> consume_stack (acc + 1) t
       | Some data -> consume_stack (acc + 1) (offer t h data))
  in
  consume_stack (Map.length map) stack
;;
