open Base
open Stdio

let print_result part result =
  printf "The result for part %d is %d\n" part result
;;

module StringPair = struct
  module T = struct
    type t = string * string

    let compare (x1, y1) (x2, y2) =
      let result = String.compare x1 x2 in
      if result = 0 then String.compare y1 y2 else result
    ;;

    let sexp_of_t (x, y) = Sexp.List [ sexp_of_string x; sexp_of_string y ]
    let hash (x, y) = String.hash x + String.hash y
  end

  include T
  include Comparator.Make (T)
end

module IntPair = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) = if x1 <> x2 then x1 - x2 else y1 - y2
    let sexp_of_t (x, y) = Sexp.List [ sexp_of_int x; sexp_of_int y ]
    let hash (x, y) = Int.hash x + Int.hash y
  end

  include T
  include Comparator.Make (T)
end

let part_nine file part =
  In_channel.with_file file ~f:(fun file ->
    let edges = In_channel.fold_lines file ~init:[] ~f:(fun acc line -> []) in
    edges)
;;

let day_eight file part =
  let rec count_chars acc = function
    | '\\' :: '\\' :: tail -> count_chars (acc + 1) tail
    | '\\' :: '"' :: tail -> count_chars (acc + 1) tail
    | '\\' :: 'x' :: _ :: _ :: tail -> count_chars (acc + 1) tail
    | '"' :: tail -> count_chars acc tail
    | _ :: tail -> count_chars (acc + 1) tail
    | [] -> acc
  in
  let rec encode_size size = function
    | '\\' :: tail -> encode_size (size + 2) tail
    | '"' :: tail -> encode_size (size + 2) tail
    | _ :: tail -> encode_size (size + 1) tail
    | [] -> size
  in
  let part_one acc line =
    acc + String.length line - count_chars 0 (String.to_list line)
  in
  let part_two acc line =
    acc + 2 + encode_size 0 (String.to_list line) - String.length line
  in
  let fold_func = if part = 1 then part_one else part_two in
  In_channel.with_file file ~f:(fun file ->
    let result = In_channel.fold_lines file ~init:0 ~f:fold_func in
    print_result part result)
;;

module Int16 = struct
  let max_value = 65535

  let ( << ) target amount =
    Int.shift_left target amount |> Int.bit_and max_value
  ;;

  let ( >> ) target amount = Int.shift_right_logical target amount
end

type wire =
  | AndLit of int * string
  | AndVar of string * string
  | Or of string * string
  | Not of string
  | LShift of string * int
  | RShift of string * int
  | Var of string
  | Literal of int

let day_seven file part =
  In_channel.with_file file ~f:(fun file ->
    let wire_list =
      In_channel.fold_lines file ~init:[] ~f:(fun acc line ->
        let tokens = String.split ~on:' ' line in
        match tokens with
        | [ dependent; "LSHIFT"; amount; "->"; wire ] ->
          (wire, LShift (dependent, Int.of_string amount)) :: acc
        | [ dependent; "RSHIFT"; amount; "->"; wire ] ->
          (wire, RShift (dependent, Int.of_string amount)) :: acc
        | [ left; "OR"; right; "->"; wire ] -> (wire, Or (left, right)) :: acc
        | [ left; "AND"; right; "->"; wire ] ->
          if Char.is_alpha (String.get left 0)
          then (wire, AndVar (left, right)) :: acc
          else (wire, AndLit (Int.of_string left, right)) :: acc
        | [ "NOT"; dependent; "->"; wire ] -> (wire, Not dependent) :: acc
        | [ value; "->"; wire ] ->
          if Char.is_alpha (String.get value 0)
          then (wire, Var value) :: acc
          else (
            let raw_value = Int.of_string value in
            (wire, Literal raw_value) :: acc)
        | _ -> failwith "Invalid input")
    in
    let wire_values = Hashtbl.create (module String) in
    let find_wire target =
      List.Assoc.find_exn wire_list target ~equal:String.equal
    in
    let rec eval_wire wire =
      match Hashtbl.find wire_values wire with
      | Some value -> value
      | None ->
        let result =
          match find_wire wire with
          | AndLit (value, dependent) -> Int.bit_and value (eval_wire dependent)
          | AndVar (left, right) ->
            Int.bit_and (eval_wire left) (eval_wire right)
          | Or (left, right) -> Int.bit_or (eval_wire left) (eval_wire right)
          | Not dependent -> Int.bit_not (eval_wire dependent)
          | LShift (dependent, amount) ->
            Int16.( << ) (eval_wire dependent) amount
          | RShift (dependent, amount) ->
            Int16.( >> ) (eval_wire dependent) amount
          | Var dependent -> eval_wire dependent
          | Literal value -> value
        in
        Hashtbl.set wire_values ~key:wire ~data:result;
        result
    in
    let init_result = eval_wire "a" in
    if part = 1
    then print_result part init_result
    else (
      Hashtbl.clear wire_values;
      Hashtbl.set wire_values ~key:"b" ~data:init_result;
      print_result part (eval_wire "a")))
;;

let day_six file part =
  In_channel.with_file file ~f:(fun file ->
    let lights = Array.create ~len:(1000 * 1000) 0 in
    let binary_lights command start_x start_y end_x end_y =
      for x = start_x to end_x do
        for y = start_y to end_y do
          let idx = (1000 * x) + y in
          lights.(idx)
          <- (match command with
              | "on" -> 1
              | "off" -> 0
              | "toggle" -> (lights.(idx) + 1) % 2
              | _ -> failwith "Invalid command")
        done
      done
    in
    let fuzzy_lights command start_x start_y end_x end_y =
      for x = start_x to end_x do
        for y = start_y to end_y do
          let idx = (1000 * x) + y in
          lights.(idx)
          <- (match command with
              | "on" -> lights.(idx) + 1
              | "off" -> Int.max 0 (lights.(idx) - 1)
              | "toggle" -> lights.(idx) + 2
              | _ -> failwith "Invalid command")
        done
      done
    in
    let process, count_lights =
      if part = 1
      then binary_lights, Array.count ~f:(fun x -> x > 0)
      else fuzzy_lights, Array.fold ~init:0 ~f:(fun acc x -> acc + x)
    in
    In_channel.iter_lines file ~f:(fun line ->
      let tokens = String.split_on_chars line ~on:[ ' '; ',' ] in
      let tokens =
        if String.equal (List.hd_exn tokens) "toggle"
        then tokens
        else List.tl_exn tokens
      in
      match tokens with
      | [ command; start_x; start_y; _; end_x; end_y ] ->
        process
          command
          (Int.of_string start_x)
          (Int.of_string start_y)
          (Int.of_string end_x)
          (Int.of_string end_y)
      | _ -> failwith "Invalid input");
    let result = count_lights lights in
    print_result part result)
;;

let day_five file part =
  let part_one string =
    let found_bad string =
      List.exists [ "ab"; "cd"; "pq"; "xy" ] ~f:(String.equal string)
    in
    let is_vowel = function
      | 'a' | 'e' | 'i' | 'o' | 'u' -> true
      | _ -> false
    in
    let rec valid_string chars (prev, vowel_count, pair_found) =
      match chars with
      | c :: tail ->
        if found_bad (String.of_char_list [ prev; c ])
        then false
        else (
          let new_count = if is_vowel c then vowel_count + 1 else vowel_count in
          let is_pair = pair_found || Char.equal c prev in
          valid_string tail (c, new_count, is_pair))
      | _ -> vowel_count >= 3 && pair_found
    in
    valid_string (String.to_list string) ('0', 0, false)
  in
  let part_two string =
    let rec find_pair string idx =
      if idx + 1 < String.length string
      then (
        let candidate = String.sub string ~pos:idx ~len:2 in
        let search_space =
          String.suffix string (String.length string - idx - 2)
        in
        String.is_substring ~substring:candidate search_space
        || find_pair string (idx + 1))
      else false
    in
    let find_match string =
      let char_one, char_two = String.get string 0, String.get string 1 in
      let rest = String.suffix string (String.length string - 2) in
      let rec helper (prev_prev, prev) = function
        | curr :: tail -> Char.equal curr prev_prev || helper (prev, curr) tail
        | _ -> false
      in
      helper (char_one, char_two) (String.to_list rest)
    in
    find_pair string 0 && find_match string
  in
  let validate = if part = 1 then part_one else part_two in
  let result =
    In_channel.with_file file ~f:(fun file ->
      In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
        if validate line then acc + 1 else acc))
  in
  print_result part result
;;

let day_four file part =
  let open Digestif in
  let key = In_channel.read_all file |> String.rstrip in
  let prefix = if part = 1 then "00000" else "000000" in
  let hashes = Sequence.unfold ~init:1 ~f:(fun num -> Some (num, num + 1)) in
  let result =
    Sequence.find hashes ~f:(fun num ->
      let hash = MD5.digest_string @@ key ^ Int.to_string num |> MD5.to_hex in
      String.is_prefix hash ~prefix)
  in
  print_result part (Option.value_exn result)
;;

let day_three file part =
  let data = In_channel.read_all file in
  let part_one path =
    let visited = Hash_set.create (module IntPair) in
    let _ =
      String.fold path ~init:(0, 0) ~f:(fun (x, y) c ->
        match c with
        | '>' ->
          Hash_set.add visited (x + 1, y);
          x + 1, y
        | '<' ->
          Hash_set.add visited (x - 1, y);
          x - 1, y
        | 'v' ->
          Hash_set.add visited (x, y - 1);
          x, y - 1
        | '^' ->
          Hash_set.add visited (x, y + 1);
          x, y + 1
        | _ -> x, y)
    in
    Hash_set.length visited
  in
  let part_two path =
    let visited = Hash_set.create (module IntPair) in
    let _ =
      String.foldi
        path
        ~init:(0, 0, 0, 0)
        ~f:(fun index (x_one, y_one, x_two, y_two) c ->
          match c, index % 2 with
          | '>', 0 ->
            Hash_set.add visited (x_one + 1, y_one);
            x_one + 1, y_one, x_two, y_two
          | '<', 0 ->
            Hash_set.add visited (x_one - 1, y_one);
            x_one - 1, y_one, x_two, y_two
          | 'v', 0 ->
            Hash_set.add visited (x_one, y_one - 1);
            x_one, y_one - 1, x_two, y_two
          | '^', 0 ->
            Hash_set.add visited (x_one, y_one + 1);
            x_one, y_one + 1, x_two, y_two
          | '>', 1 ->
            Hash_set.add visited (x_two + 1, y_two);
            x_one, y_one, x_two + 1, y_two
          | '<', 1 ->
            Hash_set.add visited (x_two - 1, y_two);
            x_one, y_one, x_two - 1, y_two
          | 'v', 1 ->
            Hash_set.add visited (x_two, y_two - 1);
            x_one, y_one, x_two, y_two - 1
          | '^', 1 ->
            Hash_set.add visited (x_two, y_two + 1);
            x_one, y_one, x_two, y_two + 1
          | _ -> x_one, y_one, x_two, y_two)
    in
    Hash_set.length visited
  in
  let process = if part = 1 then part_one else part_two in
  print_result part (process data)
;;

let day_two file part =
  let part_one acc line =
    let tokens = String.split ~on:'x' line in
    let numbers = List.map ~f:Int.of_string tokens in
    match numbers with
    | [ length; width; height ] ->
      let lw, wh, hl = length * width, width * height, height * length in
      let slack = min (min lw wh) hl in
      (2 * (lw + wh + hl)) + slack + acc
    | _ -> failwith "Invalid input"
  in
  let part_two acc line =
    let tokens = String.split ~on:'x' line in
    let numbers = List.map ~f:Int.of_string tokens in
    match numbers with
    | [ length; width; height ] ->
      let lw_perimeter, wh_perimeter, hl_perimeter =
        length + width, width + height, height + length
      in
      let min_area = 2 * min (min lw_perimeter wh_perimeter) hl_perimeter in
      min_area + (length * width * height) + acc
    | _ -> failwith "Invalid input"
  in
  let process = if part = 1 then part_one else part_two in
  let result =
    In_channel.with_file file ~f:(fun file ->
      In_channel.fold_lines file ~init:0 ~f:process)
  in
  print_result part result
;;

let day_one file part =
  let data = In_channel.read_all file in
  let part_one =
    String.fold ~init:0 ~f:(fun acc c ->
      match c with
      | '(' -> acc + 1
      | ')' -> acc - 1
      | _ -> acc)
  in
  let part_two string =
    let chars = String.to_list string in
    let rec check_floor list acc idx =
      if acc = -1
      then idx
      else (
        match list with
        | '(' :: tail -> check_floor tail (acc + 1) (idx + 1)
        | ')' :: tail -> check_floor tail (acc - 1) (idx + 1)
        | _ -> idx)
    in
    check_floor chars 0 0
  in
  let process = if part = 1 then part_one else part_two in
  print_result part (process data)
;;

let () =
  let args = Sys.get_argv () in
  if Array.length args < 2
  then eprintf "Usage: main <day> <test>\n"
  else (
    let day = Array.get args 1 in
    let file = if Array.length args > 2 then "test" else "input" in
    match Int.of_string day with
    | 1 -> day_one file 2
    | 2 -> day_two file 2
    | 3 -> day_three file 2
    | 4 -> day_four file 1
    | 5 -> day_five file 2
    | 6 -> day_six file 2
    | 7 -> day_seven file 2
    | 8 -> day_eight file 2
    | _ -> Stdio.eprintf "Please give a valid day")
;;
