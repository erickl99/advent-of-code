open Base
open Stdio

let mask = Int.shift_left 2 23 - 1

let hash_secret seed =
  let one = Int.shift_left seed 6 in
  let mixed = Int.bit_xor seed one in
  let pruned = Int.bit_and mixed mask in
  let two = Int.shift_right_logical pruned 5 in
  let mixed_two = Int.bit_xor pruned two in
  let pruned_two = Int.bit_and mixed_two mask in
  let three = Int.shift_left pruned_two 11 in
  let mixed_three = Int.bit_xor pruned_two three in
  Int.bit_and mixed_three mask
;;

let part_one seeds =
  List.fold seeds ~init:0 ~f:(fun acc seed ->
    acc + Fn.apply_n_times ~n:2000 (fun x -> hash_secret x) seed)
;;

let rec collect_prices acc n seed =
  if n = 0
  then List.rev acc
  else (
    let next_seed = hash_secret seed in
    let updated = (next_seed % 10) :: acc in
    collect_prices updated (n - 1) next_seed)
;;

let stringify one two three four =
  Int.to_string one
  ^ " "
  ^ Int.to_string two
  ^ " "
  ^ Int.to_string three
  ^ " "
  ^ Int.to_string four
;;

let get_price_changes seeds_delta_lists =
  let unique_changes = Set.empty (module String) in
  let all_seeds_to_changes = [] in
  List.fold
    seeds_delta_lists
    ~init:(unique_changes, all_seeds_to_changes)
    ~f:(fun (unique_changes, all_seeds_to_changes) delta_array ->
      let seed_to_changes = Map.empty (module String) in
      let updated_unique_changes, updated_seed_to_changes =
        Array.foldi
          delta_array
          ~init:(unique_changes, seed_to_changes)
          ~f:(fun idx (unique_changes, seed_to_changes) delta ->
            if idx + 3 >= Array.length delta_array
            then unique_changes, seed_to_changes
            else (
              let one, two, three, four =
                ( delta
                , delta_array.(idx + 1)
                , delta_array.(idx + 2)
                , delta_array.(idx + 3) )
              in
              let change_key = stringify one two three four in
              let seed_to_changes =
                match Map.find seed_to_changes change_key with
                | Some _ -> seed_to_changes
                | None -> Map.set seed_to_changes ~key:change_key ~data:(idx + 4)
              in
              Set.add unique_changes change_key, seed_to_changes))
      in
      updated_unique_changes, updated_seed_to_changes :: all_seeds_to_changes)
;;

let part_two seeds =
  let seed_price_lists =
    List.fold seeds ~init:[] ~f:(fun all_prices seed ->
      collect_prices [ seed % 10 ] 2000 seed :: all_prices)
    |> List.rev |> List.map ~f:List.to_array
  in
  let seed_delta_lists =
    List.map seed_price_lists ~f:(fun price_list ->
      Array.init 2000 ~f:(fun idx -> price_list.(idx + 1) - price_list.(idx)))
  in
  let unique_changes, seed_to_price_changes =
    get_price_changes seed_delta_lists
  in
  let fixed = List.rev seed_to_price_changes in
  Set.fold unique_changes ~init:0 ~f:(fun best_price change ->
    let computed =
      List.fold2_exn fixed seed_price_lists ~init:0 ~f:(fun acc map other ->
        match Map.find map change with
        | Some idx -> other.(idx) + acc
        | None -> acc)
    in
    Int.max computed best_price)
;;

let run file _ =
  let seeds = In_channel.read_lines file |> List.map ~f:Int.of_string in
  part_two seeds
;;
