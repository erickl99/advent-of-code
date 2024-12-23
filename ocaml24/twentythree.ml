open Core
open Stdio

let update_graph map left right =
  match Hashtbl.find map left with
  | Some prev -> Hash_set.add prev right
  | None ->
    Hashtbl.set map ~key:left ~data:(Hash_set.of_list (module String) [ right ])
;;

let rec create_network graph nodes = function
  | head :: tail ->
    (match String.split ~on:'-' head with
     | [ left; right ] ->
       update_graph graph left right;
       update_graph graph right left;
       Hash_set.add nodes left;
       Hash_set.add nodes right;
       create_network graph nodes tail
     | _ -> failwith "Invalid pattern")
  | [] -> ()
;;

let valid_triplet graph one two three =
  let one_neighbors, two_neighbors, three_neighbors =
    ( Hashtbl.find_exn graph one
    , Hashtbl.find_exn graph two
    , Hashtbl.find_exn graph three )
  in
  (Hash_set.mem one_neighbors two && Hash_set.mem one_neighbors three)
  && (Hash_set.mem two_neighbors one && Hash_set.mem two_neighbors three)
  && Hash_set.mem three_neighbors one
  && Hash_set.mem three_neighbors two
  && (Char.equal 't' (String.get one 0)
      || Char.equal 't' (String.get two 0)
      || Char.equal 't' (String.get three 0))
;;

let get_pivot left right =
  let union = Hash_set.union left right in
  let pivot = Hash_set.find union ~f:(fun _ -> true) in
  Option.value_exn pivot
;;

let rec bron_kerbosch r p x graph cliques =
  if Hash_set.length p = 0 && Hash_set.length x = 0
  then (
    let clique =
      Hash_set.to_list r
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:","
    in
    Hash_set.add cliques clique)
  else (
    let pivot = get_pivot p x in
    let pivot_neighbors = Hashtbl.find_exn graph pivot in
    let diff = Hash_set.diff p pivot_neighbors in
    Hash_set.iter diff ~f:(fun node ->
      let node_neigbors = Hashtbl.find_exn graph node in
      Hash_set.add r node;
      bron_kerbosch
        r
        (Hash_set.inter p node_neigbors)
        (Hash_set.inter x node_neigbors)
        graph
        cliques;
      Hash_set.remove r node;
      Hash_set.remove p node;
      Hash_set.add x node))
;;

let find_max_component graph nodes =
  let p = nodes in
  let x = Hash_set.create (module String) in
  let cliques = Hash_set.create (module String) in
  Hashtbl.iteri graph ~f:(fun ~key ~data ->
    let init = Hash_set.create (module String) in
    Hash_set.add init key;
    bron_kerbosch
      init
      (Hash_set.inter p data)
      (Hash_set.inter x data)
      graph
      cliques;
    Hash_set.remove p key;
    Hash_set.add x key);
  let password =
    Option.value_exn
    @@ Hash_set.max_elt cliques ~compare:(fun a b ->
      String.length a - String.length b)
  in
  print_endline password
;;

let check_triplets graph nodes =
  let len = Array.length nodes - 1 in
  let three_size = ref 0 in
  for i = 0 to len do
    for j = i + 1 to len do
      for k = j + 1 to len do
        if valid_triplet graph nodes.(i) nodes.(j) nodes.(k)
        then Int.incr three_size
      done
    done
  done;
  !three_size
;;

let run file part =
  let lines = In_channel.read_lines file in
  let graph = Hashtbl.create (module String) in
  let nodes = Hash_set.create (module String) in
  create_network graph nodes lines;
  if part = 1
  then check_triplets graph (Hash_set.to_array nodes)
  else (
    find_max_component graph nodes;
    69)
;;
