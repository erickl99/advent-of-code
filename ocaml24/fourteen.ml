open Base
open Stdio
open Angstrom

type point =
  { x : int
  ; y : int
  }

module Point = struct
  module T = struct
    type t = point

    let compare left right =
      if left.x <> right.x then left.x - right.x else left.y - right.y
    ;;

    let sexp_of_t p = Sexp.List [ sexp_of_int p.x; sexp_of_int p.y ]
  end

  include T
  include Comparator.Make (T)
end

let number =
  take_while1 Char.is_digit >>= fun digits -> return (Int.of_string digits)
;;

let integer =
  let* next = peek_char in
  match next with
  | Some '-' ->
    let* value = advance 1 *> number in
    return (-value)
  | Some x when Char.is_digit x -> number
  | _ -> fail "Expected minus or digit"
;;

let robot =
  let* robot_x = string "p=" *> integer in
  let* robot_y = char ',' *> integer in
  let* velocity_x = string " v=" *> integer in
  let* velocity_y = char ',' *> integer in
  return ({ x = robot_x; y = robot_y }, { x = velocity_x; y = velocity_y })
;;

let rec feed_lines buffered_parser = function
  | head :: tail ->
    let updated = Buffered.feed buffered_parser (`String head) in
    feed_lines updated tail
  | [] -> Buffered.feed buffered_parser `Eof
;;

let simulate position velocity steps rows cols =
  let raw_new_x = position.x + (steps * velocity.x) in
  let raw_new_y = position.y + (steps * velocity.y) in
  raw_new_x % cols, raw_new_y % rows
;;

let find_quadrant position velocity steps rows cols =
  let modded_x, modded_y = simulate position velocity steps rows cols in
  let mid_x, mid_y = cols / 2, rows / 2 in
  if modded_x > mid_x && modded_y < mid_y
  then 1
  else if modded_x > mid_x && modded_y > mid_y
  then 2
  else if modded_x < mid_x && modded_y > mid_y
  then 3
  else if modded_x < mid_x && modded_y < mid_y
  then 4
  else 0
;;

let rec count_robots max_seen seen row col robots =
  if col >= 102
  then max_seen
  else if Set.mem robots { x = col; y = row }
  then (
    let max_seen = Int.max max_seen (seen + 1) in
    count_robots max_seen (seen + 1) row (col + 1) robots)
  else count_robots max_seen 0 row (col + 1) robots
;;

let find_line positions =
  let all_rows = Sequence.range 0 103 in
  Sequence.fold ~init:0 all_rows ~f:(fun acc row ->
    let counted = count_robots 0 0 row 0 positions in
    Int.max acc counted)
;;

let part_two robots =
  let rows, cols = 103, 101 in
  let init = Set.empty (module Point) in
  let positions =
    List.fold robots ~init ~f:(fun acc (p, v) ->
      let final_x, final_y = simulate p v 7858 rows cols in
      Set.add acc { x = final_x; y = final_y })
  in
  let output_lines =
    Array.init rows ~f:(fun row ->
      String.init cols ~f:(fun col ->
        if Set.mem positions { x = col; y = row } then '#' else '.'))
  in
  Array.iter output_lines ~f:(printf "%s\n")
;;

let rec find_cycle start current velocity rows cols steps =
  let next_x = (current.x + velocity.x) % cols in
  let next_y = (current.y + velocity.y) % rows in
  if next_x = start.x && next_y = start.y
  then steps
  else find_cycle start { x = next_x; y = next_y } velocity rows cols (steps + 1)
;;

let part_one robots =
  let rows, cols = 103, 101 in
  let one, two, three, four =
    List.fold
      ~init:(0, 0, 0, 0)
      robots
      ~f:(fun (one, two, three, four) (p, v) ->
        match find_quadrant p v 100 rows cols with
        | 1 -> one + 1, two, three, four
        | 2 -> one, two + 1, three, four
        | 3 -> one, two, three + 1, four
        | 4 -> one, two, three, four + 1
        | _ -> one, two, three, four)
  in
  one * two * three * four
;;

let run file part =
  let lines = In_channel.read_lines file in
  let buffered_parser = Buffered.parse (many robot) in
  let finished = feed_lines buffered_parser lines in
  match Buffered.state_to_result finished with
  | Ok values ->
    if part = 1
    then part_one values
    else (
      part_two values;
      7858)
  | Error msg -> failwith msg
;;
