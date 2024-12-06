open Base
open Stdio

let rec valid_ordering seen deps = function
  | page :: rest ->
    (match Map.find deps page with
     | Some list ->
       List.for_all seen ~f:(fun x -> not @@ List.mem list x ~equal:Int.equal)
       && valid_ordering (page :: seen) deps rest
     | None ->
       valid_ordering (page :: seen) deps rest)
  | [] -> true
;;

let rec get_valid_orderings acc deps = function
  | line :: tail ->
    let pages = String.split line ~on:',' |> List.map ~f:Int.of_string in
    let acc = if valid_ordering [] deps pages then pages :: acc else acc in
    get_valid_orderings acc deps tail
  | [] -> acc
;;

let part_one lines dependencies =
  let valid_orderings = get_valid_orderings [] dependencies lines in
  List.fold valid_orderings ~init:0 ~f:(fun acc ordering ->
    let len = List.length ordering in
    acc + List.nth_exn ordering (len / 2))
;;

let rec get_invalid_orderings acc deps = function
  | line :: tail ->
    let pages = String.split line ~on:',' |> List.map ~f:Int.of_string in
    let acc = if valid_ordering [] deps pages then acc else pages :: acc in
    get_invalid_orderings acc deps tail
  | [] -> acc
;;

let sorter list deps =
  List.sort list ~compare:(fun a b ->
    if List.mem (Map.find_exn deps b) a ~equal:Int.equal then 1 else -1)
;;

let part_two lines dependencies =
  let invalid_orderings = get_invalid_orderings [] dependencies lines in
  List.map invalid_orderings ~f:(fun list -> sorter list dependencies)
  |> List.fold ~init:0 ~f:(fun acc ordering ->
    let len = List.length ordering in
    acc + List.nth_exn ordering (len / 2))
;;

let update_map map left right =
  match Map.find map left with
  | Some list -> Map.set map ~key:left ~data:(right :: list)
  | None -> Map.set map ~key:left ~data:[ right ]
;;

let rec get_dependencies deps = function
  | line :: tail ->
    (match String.split line ~on:'|' with
     | [ left; right ] ->
       let deps = update_map deps (Int.of_string left) (Int.of_string right) in
       get_dependencies deps tail
     | [ "" ] -> deps, tail
     | _ -> failwith "Invalid input")
  | [] -> failwith "Reached end of file"
;;

let run file part =
  let lines = In_channel.read_lines file in
  let dependencies, lines = get_dependencies (Map.empty (module Int)) lines in
  if part = 1 then part_one lines dependencies else part_two lines dependencies
;;
