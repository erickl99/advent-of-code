open Base

(* let spring_perms line = *)
(*   let process_char perms c = 0 in *)
(*   match String.split ~on:' ' line with *)
(*   | [ row; raw_pattern ] -> *)
(*     let pattern = *)
(*       String.split ~on:',' raw_pattern |> List.map ~f:Int.of_string *)
(*     in *)
(*     0 *)
(*   | _ -> failwith "Invalid input" *)
(* ;; *)
(**)

let get_key one two three = (10000 * one) + (100 * two) + three

let rec unfold_pattern pattern = function
  | 0 -> pattern
  | x -> unfold_pattern pattern (x - 1) |> List.append pattern
;;

let unfold_row row =
  Printf.sprintf "%s?%s?%s?%s?%s" row row row row row |> String.to_list
;;

let spring_count line =
  let rec dp_spring row pattern spring_length memo =
    let k = get_key (List.length row) (List.length pattern) spring_length in
    match Hashtbl.find memo k with
    | Some result -> result
    | None ->
      let result =
        match row, pattern, spring_length with
        | [], [], 0 -> 1
        | [], [ len ], sl when len = sl -> 1
        | '.' :: t, len :: p, sl when sl = len -> dp_spring t p 0 memo
        | '.' :: t, p, 0 -> dp_spring t p 0 memo
        | '#' :: t, p, sl -> dp_spring t p (sl + 1) memo
        | '?' :: t, p, 0 -> dp_spring t p 0 memo + dp_spring t p 1 memo
        | '?' :: t, len :: p, sl ->
          if sl = len
          then dp_spring t p 0 memo
          else dp_spring t (len :: p) (sl + 1) memo
        | _ -> 0
      in
      Hashtbl.set memo ~key:k ~data:result;
      result
  in
  match String.split ~on:' ' line with
  | [ row; raw_pattern ] ->
    let pattern =
      String.split ~on:',' raw_pattern |> List.map ~f:Int.of_string
    in
    dp_spring
      (unfold_row row)
      (unfold_pattern pattern 4)
      0
      (Hashtbl.create (module Int))
  | _ -> failwith "Invalid input"
;;
