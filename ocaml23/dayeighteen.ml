open Base
open Stdio

type dig_state =
  { vertices : (int * int) list
  ; perimeter : int
  ; coordinate : int * int
  }

let get_instruction line =
  match String.split ~on:' ' line with
  | [ direction; distance; _ ] -> direction, Int.of_string distance
  | _ -> failwith "Invalid input"
;;

let hex_instruction line =
  match String.split ~on:' ' line with
  | [ _; _; hex ] ->
    let distance = Int.of_string ("0x" ^ String.sub hex ~pos:2 ~len:5) in
    (match String.sub hex ~pos:7 ~len:1 with
     | "0" -> "R", distance
     | "1" -> "D", distance
     | "2" -> "L", distance
     | "3" -> "U", distance
     | _ -> failwith "Invalid input")
  | _ -> failwith "Invalid input"
;;

let process_file file =
  let process_instruction state line =
    let direction, distance = hex_instruction line in
    let new_coord =
      let x, y = state.coordinate in
      match direction with
      | "R" -> x + distance, y
      | "L" -> x - distance, y
      | "U" -> x, y + distance
      | "D" -> x, y - distance
      | _ -> x, y
    in
    { vertices = new_coord :: state.vertices
    ; perimeter = state.perimeter + distance
    ; coordinate = new_coord
    }
  in
  let dig_plan =
    In_channel.fold_lines
      file
      ~init:{ vertices = []; perimeter = 0; coordinate = 0, 0 }
      ~f:process_instruction
  in
  let rec compute_area prev_x = function
    | (x, y) :: (next_x, next_y) :: t ->
      (y * (prev_x - next_x)) + compute_area x ((next_x, next_y) :: t)
    | _ -> 0
  in
  let (start_x, _), vertices =
    List.hd_exn dig_plan.vertices, List.tl_exn dig_plan.vertices
  in
  let area = compute_area start_x vertices / 2 in
  let interior_points = area + 1 - (dig_plan.perimeter / 2) in
  interior_points + dig_plan.perimeter
;;
