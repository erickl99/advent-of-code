open Base
open Stdio

type hand =
  { cards : string
  ; rank : int
  ; bid : int
  }

let add_to_map map x =
  match Map.find map x with
  | Some data -> Map.set map ~key:x ~data:(data + 1)
  | None -> Map.set map ~key:x ~data:1
;;

let card_to_int = function
  | 'T' -> 10
  | 'J' -> 0
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | c -> Char.get_digit_exn c
;;

let compare_hand hand1 hand2 =
  if hand1.rank <> hand2.rank
  then hand1.rank - hand2.rank
  else (
    let rec find_diff cards1 cards2 idx =
      let char_one, char_two = String.get cards1 idx, String.get cards2 idx in
      if Char.equal char_one char_two
      then find_diff cards1 cards2 (idx + 1)
      else card_to_int char_one - card_to_int char_two
    in
    find_diff hand1.cards hand2.cards 0)
;;

let score_func ~key:_ ~data:v acc =
  match v with
  | 2 -> 1 + acc
  | 3 -> 3 + acc
  | 4 -> 5 + acc
  | 5 -> 6 + acc
  | _ -> acc
;;

let get_rank cards =
  let count = String.fold ~init:(Map.empty (module Char)) ~f:add_to_map cards in
  Map.fold ~f:score_func ~init:0 count
;;

let get_joker_hand hand =
  let count = String.count ~f:(fun c -> Char.equal c 'J') hand.cards in
  match hand.rank, count with
  | 0, 1 -> { cards = hand.cards; rank = 1; bid = hand.bid }
  | 1, 1 -> { cards = hand.cards; rank = 3; bid = hand.bid }
  | 2, 1 -> { cards = hand.cards; rank = 4; bid = hand.bid }
  | 3, 1 -> { cards = hand.cards; rank = 5; bid = hand.bid }
  | 5, 1 -> { cards = hand.cards; rank = 6; bid = hand.bid }
  | 1, 2 -> { cards = hand.cards; rank = 3; bid = hand.bid }
  | 2, 2 -> { cards = hand.cards; rank = 5; bid = hand.bid }
  | 4, 2 -> { cards = hand.cards; rank = 6; bid = hand.bid }
  | 3, 3 -> { cards = hand.cards; rank = 5; bid = hand.bid }
  | 4, 3 -> { cards = hand.cards; rank = 6; bid = hand.bid }
  | 5, 4 -> { cards = hand.cards; rank = 6; bid = hand.bid }
  | _ -> hand
;;

let process_file file =
  let process_line hands line =
    match String.split ~on:' ' line with
    | [ cards; bid ] ->
      let h = { cards; rank = get_rank cards; bid = Int.of_string bid } in
      get_joker_hand h :: hands
    | _ -> []
  in
  let hands = In_channel.fold_lines ~init:[] ~f:process_line file in
  let sorted = List.sort ~compare:compare_hand hands in
  List.foldi ~f:(fun idx acc h -> acc + ((idx + 1) * h.bid)) ~init:0 sorted
;;
