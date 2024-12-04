open Base
open Stdio

let rec count_horizontal acc = function
  | 'X' :: 'M' :: 'A' :: 'S' :: tail -> count_horizontal (acc + 1) ('S' :: tail)
  | 'S' :: 'A' :: 'M' :: 'X' :: tail -> count_horizontal (acc + 1) ('X' :: tail)
  | _ :: tail -> count_horizontal acc tail
  | [] -> acc
;;

let rec count_vertical acc one two three four =
  match one, two, three, four with
  | 'X' :: tail_one, 'M' :: tail_two, 'A' :: tail_three, 'S' :: tail_four ->
    count_vertical (acc + 1) tail_one tail_two tail_three tail_four
  | 'S' :: tail_one, 'A' :: tail_two, 'M' :: tail_three, 'X' :: tail_four ->
    count_vertical (acc + 1) tail_one tail_two tail_three tail_four
  | _ :: tail_one, _ :: tail_two, _ :: tail_three, _ :: tail_four ->
    count_vertical acc tail_one tail_two tail_three tail_four
  | [], [], [], [] -> acc
  | _ -> acc
;;

let rec count_other acc = function
  | a :: b :: c :: d :: tail ->
    let one, two, three, four =
      String.to_list a, String.to_list b, String.to_list c, String.to_list d
    in
    let vertical = count_vertical 0 one two three four in
    let left_diag =
      count_vertical
        0
        one
        (List.drop two 1)
        (List.drop three 2)
        (List.drop four 3)
    in
    let rev_one, rev_two, rev_three, rev_four =
      List.rev one, List.rev two, List.rev three, List.rev four
    in
    let right_diag =
      count_vertical
        0
        rev_one
        (List.drop rev_two 1)
        (List.drop rev_three 2)
        (List.drop rev_four 3)
    in
    let new_acc = acc + vertical + left_diag + right_diag in
    count_other new_acc (b :: c :: d :: tail)
  | _ -> acc
;;

let rec find_cross acc one two three =
  match one, two, three with
  | ( 'M' :: x :: 'M' :: tail_one
    , _ :: 'A' :: y :: tail_two
    , 'S' :: z :: 'S' :: tail_three ) ->
    find_cross
      (acc + 1)
      (x :: 'M' :: tail_one)
      ('A' :: y :: tail_two)
      (z :: 'S' :: tail_three)
  | ( 'S' :: x :: 'S' :: tail_one
    , _ :: 'A' :: y :: tail_two
    , 'M' :: z :: 'M' :: tail_three ) ->
    find_cross
      (acc + 1)
      (x :: 'S' :: tail_one)
      ('A' :: y :: tail_two)
      (z :: 'M' :: tail_three)
  | ( 'M' :: x :: 'S' :: tail_one
    , _ :: 'A' :: y :: tail_two
    , 'M' :: z :: 'S' :: tail_three ) ->
    find_cross
      (acc + 1)
      (x :: 'S' :: tail_one)
      ('A' :: y :: tail_two)
      (z :: 'S' :: tail_three)
  | ( 'S' :: x :: 'M' :: tail_one
    , _ :: 'A' :: y :: tail_two
    , 'S' :: z :: 'M' :: tail_three ) ->
    find_cross
      (acc + 1)
      (x :: 'M' :: tail_one)
      ('A' :: y :: tail_two)
      (z :: 'M' :: tail_three)
  | _ :: tail_one, _ :: tail_two, _ :: tail_three ->
    find_cross acc tail_one tail_two tail_three
  | [], [], [] -> acc
  | _ -> failwith "Lists not of same length"
;;

let rec count_cross acc = function
  | one :: two :: three :: tail ->
    let counted =
      find_cross
        0
        (String.to_list one)
        (String.to_list two)
        (String.to_list three)
    in
    count_cross (acc + counted) (two :: three :: tail)
  | _ -> acc
;;

let day_one file =
  let lines = In_channel.read_lines file in
  let horizontal =
    List.fold lines ~init:0 ~f:(fun acc line ->
      acc + count_horizontal 0 (String.to_list line))
  in
  let other = count_other 0 lines in
  horizontal + other
;;

let day_two file =
  let lines = In_channel.read_lines file in
  count_cross 0 lines
;;

let run file part = if part = 1 then day_one file else day_two file
