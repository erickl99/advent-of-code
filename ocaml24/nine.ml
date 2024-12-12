open Base
open Stdio

let char_to_int c = Char.to_int c - Char.to_int '0'

let rec cons_n n value list =
  if n = 0 then list else cons_n (n - 1) value (value :: list)
;;

module PartOne = struct
  type block_type =
    | File of int
    | Free

  let checksum list =
    List.foldi list ~init:0 ~f:(fun idx acc id -> acc + (id * idx))
  ;;

  let rec read_disk acc idx id disk =
    match disk with
    | size :: tail ->
      let size = char_to_int size in
      let value, next_id = if idx % 2 = 0 then File id, id + 1 else Free, id in
      read_disk (cons_n size value acc) (idx + 1) next_id tail
    | [] -> acc
  ;;

  let rec compress_files acc left left_idx right right_idx =
    if left_idx > right_idx
    then acc
    else (
      match left with
      | File left_id :: left_tail ->
        compress_files (left_id :: acc) left_tail (left_idx + 1) right right_idx
      | Free :: left_tail ->
        (match right with
         | File right_id :: right_tail ->
           compress_files
             (right_id :: acc)
             left_tail
             (left_idx + 1)
             right_tail
             (right_idx - 1)
         | Free :: right_tail ->
           compress_files
             acc
             (Free :: left_tail)
             left_idx
             right_tail
             (right_idx - 1)
         | [] -> failwith "Reached end of disk from right")
      | [] -> failwith "Reached end of disk from left")
  ;;

  let solve input =
    let contents_rev = read_disk [] 0 0 input in
    let contents = List.rev contents_rev in
    let len = List.length contents in
    let compressed =
      compress_files [] contents 0 contents_rev (len - 1) |> List.rev
    in
    checksum compressed
  ;;
end

module PartTwo = struct
  type block_type =
    | File of int * int
    | Free of int

  let rec read_disk contents all_files idx id disk =
    match disk with
    | size :: tail ->
      let size = char_to_int size in
      let value, all_ids, next_id =
        if idx % 2 = 0
        then File (id, size), (id, size) :: all_files, id + 1
        else Free size, all_files, id
      in
      read_disk (value :: contents) all_ids (idx + 1) next_id tail
    | [] -> contents, all_files
  ;;

  let rec prepend front back =
    match front with
    | head :: tail -> prepend tail (head :: back)
    | [] -> back
  ;;

  let rec remove_old file_id before after =
    match after with
    | File (id, file_size) :: tail ->
      if file_id <> id
      then remove_old file_id (File (id, file_size) :: before) tail
      else (
        match before, tail with
        | [], Free size_one :: rest -> Free (file_size + size_one) :: rest
        | Free size_one :: left, Free size_two :: right ->
          prepend left (Free (size_one + size_two + file_size) :: right)
        | left, Free size_one :: rest ->
          prepend left (Free (size_one + file_size) :: rest)
        | left, right -> prepend left (Free file_size :: right))
    | Free size :: tail -> remove_old file_id (Free size :: before) tail
    | _ -> failwith "Reach end of input"
  ;;

  let rec move_file current_file before after =
    let current_id, current_size = current_file in
    match after with
    | File (id, size) :: tail ->
      if current_id = id
      then prepend (File (id, size) :: before) tail
      else move_file current_file (File (id, size) :: before) tail
    | Free size :: tail ->
      if current_size < size
      then (
        let removed =
          remove_old current_id [ Free (size - current_size) ] tail
        in
        prepend (File (current_id, current_size) :: before) removed)
      else if current_size = size
      then (
        let removed = remove_old current_id [] tail in
        prepend (File (current_id, current_size) :: before) removed)
      else move_file current_file (Free size :: before) tail
    | [] -> failwith "Invalid file given"
  ;;

  let rec compress_files contents = function
    | file :: tail ->
      let updated = move_file file [] contents in
      compress_files updated tail
    | [] -> contents
  ;;

  let rec sum acc start end_ =
    if start = end_ then acc else sum (acc + start) (start + 1) end_
  ;;

  let rec checksum acc idx = function
    | File (id, size) :: tail ->
      let multiplier = sum 0 idx (idx + size) in
      checksum (acc + (id * multiplier)) (idx + size) tail
    | Free size :: tail -> checksum acc (idx + size) tail
    | [] -> acc
  ;;

  let solve input =
    let contents_rev, ids = read_disk [] [] 0 0 input in
    let contents = List.rev contents_rev in
    let compressed = compress_files contents ids in
    checksum 0 0 compressed
  ;;
end

let run file part =
  let input = In_channel.read_all file |> String.strip |> String.to_list in
  if part = 1 then PartOne.solve input else PartTwo.solve input
;;
