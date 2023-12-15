open Base

let spring_perms line =
  let process_char perms c = 0 in
  match String.split ~on:' ' line with
  | [ row; raw_pattern ] ->
    let pattern =
      String.split ~on:',' raw_pattern |> List.map ~f:Int.of_string
    in
    0
  | _ -> failwith "Invalid input"
;;

let get_key one two three = Printf.sprintf "%d %d %d" one two three

let spring_count line =
  let rec dp_spring row pattern spring_length memo =
    let k = get_key (List.length row) (List.length pattern) spring_length in
    match Hashtbl.find memo k with
    | Some result -> result
    | None ->
      let result =
        match row, pattern, spring_length with
        | [], [], 0 -> 1
        | [], [ x ], y when x = y -> 1
        | [], _, _ -> 0
        | '.' :: t, x :: p, y when x = y -> dp_spring t p 0 memo
        | '.' :: t, p, 0 -> dp_spring t p 0 memo
        | '#' :: t, p, s -> dp_spring t p (s + 1) memo
        | '?' :: t, x :: p, y when x = y ->
          dp_spring t p 0 memo + dp_spring t (x :: p) (y + 1) memo
        | '?' :: t, p, 0 -> dp_spring t p 0 memo + dp_spring t p 1 memo
        | _ -> 0
      in
      Hashtbl.set memo ~key:k ~data:result;
      Stdio.printf "Added key |%s| with result: %d\n" k result;
      result
  in
  match String.split ~on:' ' line with
  | [ row; raw_pattern ] ->
    let pattern =
      String.split ~on:',' raw_pattern |> List.map ~f:Int.of_string
    in
    let combs =
      dp_spring (String.to_list row) pattern 0 (Hashtbl.create (module String))
    in
    combs
  | _ -> failwith "Invalid input"
;;
