open Base

type coord = int * int

module Pair = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) = if x1 <> x2 then x1 - x2 else y1 - y2
    let sexp_of_t (x, y) = Sexp.List [ sexp_of_int x; sexp_of_int y ]
  end

  include T
  include Comparator.Make (T)
end

type mark =
  | At
  | Dollar
  | Equal
  | Plus
  | Pound
  | Percent
  | Slash
  | Star

type number =
  { value : int
  ; pos : coord
  }

let process_file file =
  let process_line file row_num =
    match In_channel.input_line file with
    | Some str ->
      let map, ls, _, _ =
        String.fold
          str
          ~f:(fun (map, ls, idx, value) char ->
            match char with
            | '0' .. '9' -> map, ls, idx, (10 * value) + Char.get_digit_exn char
            | '@' | '$' | '=' | '+' | '#' | '%' | '/' | '*' ->
              map, (row_num, idx) :: ls, idx + 1, value
            | _ ->
              if value > 0
              then Map.set map ~key:(row_num, idx) ~data:value, ls, idx + 1, 0
              else map, ls, idx + 1, 0)
          ~init:(Map.empty (module Pair), [], 0, 0)
      in
      List.iter ~f:(fun (x, y) -> Stdio.printf "%d, %d\n" x y) ls;
      Map.iter ~f:(fun x -> Stdio.printf "%d\n" x) map;
      0
    | None -> failwith "Invalid input."
  in
  process_line file 0
;;
