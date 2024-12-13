open Base
open Stdio
open Angstrom

let number =
  take_while1 Char.is_digit >>= fun digits -> return (Int.of_string digits)
;;

let candidates = List.range 0 101

let brute_force_solver equation =
  let row_one_a, row_two_a, row_one_b, row_two_b, target_x, target_y =
    equation
  in
  let x_values = Sequence.of_list candidates in
  let x_found =
    Sequence.find x_values ~f:(fun x ->
      let y_values = Sequence.of_list candidates in
      match
        Sequence.find y_values ~f:(fun y ->
          (row_one_a * x) + (row_one_b * y) = target_x
          && (row_two_a * x) + (row_two_b * y) = target_y)
      with
      | Some _ -> true
      | None -> false)
  in
  match x_found with
  | Some x -> Some (x, (target_x - (row_one_a * x)) / row_one_b)
  | None -> None
;;

let solve_equation equation =
  let row_one_a, row_two_a, row_one_b, row_two_b, target_x, target_y =
    equation
  in
  let coefficient_det = (row_one_a * row_two_b) - (row_one_b * row_two_a) in
  let ex_det = (target_x * row_two_b) - (target_y * row_one_b) in
  let ey_det = (row_one_a * target_y) - (row_two_a * target_x) in
  let candidate_x, candidate_y =
    ex_det / coefficient_det, ey_det / coefficient_det
  in
  if (candidate_x * row_one_a) + (candidate_y * row_one_b) = target_x
     && (candidate_x * row_two_a) + (candidate_y * row_two_b) = target_y
  then Some (candidate_x, candidate_y)
  else None
;;

let part_one =
  let* row_one_a = string "Button A: X+" *> number <?> "first one" in
  let* row_two_a = string ", Y+" *> number <?> "second one" in
  let* row_one_b = string "Button B: X+" *> number <?> "third one" in
  let* row_two_b = string ", Y+" *> number <?> "fourth one" in
  let* target_x = string "Prize: X=" *> number <?> "fifth one" in
  let* target_y = string ", Y=" *> number <?> "sixth one" in
  return (row_one_a, row_two_a, row_one_b, row_two_b, target_x, target_y)
;;

let part_two =
  let* row_one_a = string "Button A: X+" *> number <?> "first one" in
  let* row_two_a = string ", Y+" *> number <?> "second one" in
  let* row_one_b = string "Button B: X+" *> number <?> "third one" in
  let* row_two_b = string ", Y+" *> number <?> "fourth one" in
  let* target_x = string "Prize: X=" *> number <?> "fifth one" in
  let* target_y = string ", Y=" *> number <?> "sixth one" in
  return
    ( row_one_a
    , row_two_a
    , row_one_b
    , row_two_b
    , target_x + 10000000000000
    , target_y + 10000000000000 )
;;

let rec feed_lines buffered_parser = function
  | head :: tail ->
    let updated = Buffered.feed buffered_parser (`String head) in
    feed_lines updated tail
  | [] -> Buffered.feed buffered_parser `Eof
;;

let run file part =
  let content = In_channel.read_lines file in
  let crane_parser = if part = 1 then many part_one else many part_two in
  let buffered_parser = Buffered.parse crane_parser in
  let finished = feed_lines buffered_parser content in
  match Buffered.state_to_result finished with
  | Ok nums ->
    List.fold nums ~init:0 ~f:(fun acc equation ->
      match solve_equation equation with
      | Some (x, y) -> acc + (3 * x) + y
      | None -> acc)
  | Error msg -> failwith msg
;;
