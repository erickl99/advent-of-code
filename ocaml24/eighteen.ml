open Base
open Stdio

type point =
  { x : int
  ; y : int
  }

module Point = struct
  module T = struct
    type t = point

    let compare left right =
      if left.x <> right.x then left.x - right.x else left.y - right.y
    ;;

    let sexp_of_t p = Sexp.List [ sexp_of_int p.x; sexp_of_int p.y ]
    let hash p = Int.hash p.x + Int.hash p.y
  end

  include T
  include Comparator.Make (T)
end

let deltas = [| 0, -1; 1, 0; 0, 1; -1, 0 |]

let bfs obstacles rows cols =
  let queue = Queue.create () in
  let visited = Hash_set.create (module Point) in
  let steps, found = ref (-1), ref false in
  Hash_set.add visited { x = 0; y = 0 };
  Queue.enqueue queue { x = 0; y = 0 };
  while (not @@ Queue.is_empty queue) && not !found do
    let length = Queue.length queue in
    for _ = 0 to length - 1 do
      let cell = Queue.dequeue_exn queue in
      if cell.x = rows && cell.y = cols
      then found := true
      else
        Array.iter deltas ~f:(fun (delta_x, delta_y) ->
          let next = { x = cell.x + delta_x; y = cell.y + delta_y } in
          if next.x >= 0
             && next.x <= cols
             && next.y >= 0
             && next.y <= rows
             && (not @@ Set.mem obstacles next)
             && (not @@ Hash_set.mem visited next)
          then (
            Hash_set.add visited next;
            Queue.enqueue queue next))
    done;
    Int.incr steps
  done;
  if !found then Some !steps else None
;;

let part_one file =
  let amount, grid_size =
    if String.is_substring ~substring:"test" file then 12, 6 else 1024, 70
  in
  let all_lines = In_channel.read_lines file in
  let lines = List.take all_lines amount in
  let init = Set.empty (module Point) in
  let obstacles =
    List.fold lines ~init ~f:(fun acc line ->
      match String.split ~on:',' line with
      | [ x; y ] -> Set.add acc { x = Int.of_string x; y = Int.of_string y }
      | _ -> failwith "Invalid input")
  in
  Option.value_exn @@ bfs obstacles grid_size grid_size
;;

let part_two file =
  let grid_size, start =
    if String.is_substring ~substring:"test" file then 6, 12 else 70, 1024
  in
  let bytes = In_channel.read_lines file in
  let init = Set.empty (module Point) in
  let rec check_byte step obstacles bytes =
    match bytes with
    | byte :: tail ->
      let updated =
        match String.split ~on:',' byte with
        | [ x; y ] ->
          Set.add obstacles { x = Int.of_string x; y = Int.of_string y }
        | _ -> failwith "Invalid input"
      in
      if step < start
      then check_byte (step + 1) updated tail
      else (
        match bfs updated grid_size grid_size with
        | Some _ -> check_byte (step + 1) updated tail
        | None -> step)
    | [] -> failwith "Reached end of list"
  in
  printf "%s\n" (List.nth_exn bytes @@ (check_byte 1 init bytes - 1));
  69
;;

let run file part = if part = 1 then part_one file else part_two file
