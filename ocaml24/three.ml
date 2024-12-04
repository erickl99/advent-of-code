open Base
open Stdio

type parser_state =
  | LetterM
  | LetterU
  | LetterL
  | LeftParen
  | LeftNumOne
  | LeftNumTwo
  | LeftNumThree
  | RightNumOne
  | RightNumTwo
  | RightNumThree
  | Comma
  | RightParen

let to_int char = Char.to_int char - Char.to_int '0'

let rec read_memory_two active state left right acc chars =
  match chars, state with
  | 'd' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: tail, _ ->
    read_memory_two false LetterM 0 0 acc tail
  | 'd' :: 'o' :: '(' :: ')' :: tail, _ ->
    read_memory_two true LetterM 0 0 acc tail
  | 'm' :: tail, LetterM -> read_memory_two active LetterU left right acc tail
  | 'u' :: tail, LetterU -> read_memory_two active LetterL left right acc tail
  | 'l' :: tail, LetterL -> read_memory_two active LeftParen left right acc tail
  | '(' :: tail, LeftParen ->
    read_memory_two active LeftNumOne left right acc tail
  | x :: tail, LeftNumOne when Char.is_digit x ->
    read_memory_two active LeftNumTwo (to_int x) right acc tail
  | ',' :: tail, LeftNumTwo ->
    read_memory_two active RightNumOne left right acc tail
  | x :: tail, LeftNumTwo when Char.is_digit x ->
    read_memory_two active LeftNumThree ((10 * left) + to_int x) right acc tail
  | ',' :: tail, LeftNumThree ->
    read_memory_two active RightNumOne left right acc tail
  | x :: tail, LeftNumThree when Char.is_digit x ->
    read_memory_two active Comma ((10 * left) + to_int x) right acc tail
  | ',' :: tail, Comma -> read_memory_two active RightNumOne left right acc tail
  | x :: tail, RightNumOne when Char.is_digit x ->
    read_memory_two active RightNumTwo left (to_int x) acc tail
  | ')' :: tail, RightNumTwo ->
    let new_acc = if active then acc + (left * right) else acc in
    read_memory_two active LetterM 0 0 new_acc tail
  | x :: tail, RightNumTwo when Char.is_digit x ->
    read_memory_two active RightNumThree left ((10 * right) + to_int x) acc tail
  | ')' :: tail, RightNumThree ->
    let new_acc = if active then acc + (left * right) else acc in
    read_memory_two active LetterM 0 0 new_acc tail
  | x :: tail, RightNumThree when Char.is_digit x ->
    read_memory_two active RightParen left ((10 * right) + to_int x) acc tail
  | ')' :: tail, RightParen ->
    let new_acc = if active then acc + (left * right) else acc in
    read_memory_two active LetterM 0 0 new_acc tail
  | 'm' :: tail, _ -> read_memory_two active LetterU 0 0 acc tail
  | _ :: tail, _ -> read_memory_two active LetterM 0 0 acc tail
  | [], _ -> acc
;;

let rec read_memory state left right acc chars =
  match chars, state with
  | 'm' :: tail, LetterM -> read_memory LetterU left right acc tail
  | 'u' :: tail, LetterU -> read_memory LetterL left right acc tail
  | 'l' :: tail, LetterL -> read_memory LeftParen left right acc tail
  | '(' :: tail, LeftParen -> read_memory LeftNumOne left right acc tail
  | x :: tail, LeftNumOne when Char.is_digit x ->
    read_memory LeftNumTwo (to_int x) right acc tail
  | ',' :: tail, LeftNumTwo -> read_memory RightNumOne left right acc tail
  | x :: tail, LeftNumTwo when Char.is_digit x ->
    read_memory LeftNumThree ((10 * left) + to_int x) right acc tail
  | ',' :: tail, LeftNumThree -> read_memory RightNumOne left right acc tail
  | x :: tail, LeftNumThree when Char.is_digit x ->
    read_memory Comma ((10 * left) + to_int x) right acc tail
  | ',' :: tail, Comma -> read_memory RightNumOne left right acc tail
  | x :: tail, RightNumOne when Char.is_digit x ->
    read_memory RightNumTwo left (to_int x) acc tail
  | ')' :: tail, RightNumTwo ->
    read_memory LetterM 0 0 (acc + (left * right)) tail
  | x :: tail, RightNumTwo when Char.is_digit x ->
    read_memory RightNumThree left ((10 * right) + to_int x) acc tail
  | ')' :: tail, RightNumThree ->
    read_memory LetterM 0 0 (acc + (left * right)) tail
  | x :: tail, RightNumThree when Char.is_digit x ->
    read_memory RightParen left ((10 * right) + to_int x) acc tail
  | ')' :: tail, RightParen ->
    read_memory LetterM 0 0 (acc + (left * right)) tail
  | 'm' :: tail, _ -> read_memory LetterU 0 0 acc tail
  | _ :: tail, _ -> read_memory LetterM 0 0 acc tail
  | [], _ -> acc
;;

let part_one file =
  let memory = In_channel.read_all file |> String.to_list in
  read_memory LetterM 0 0 0 memory
;;

let part_two file =
  let memory = In_channel.read_all file |> String.to_list in
  read_memory_two true LetterM 0 0 0 memory
;;

let run file part = if part = 1 then part_one file else part_two file
