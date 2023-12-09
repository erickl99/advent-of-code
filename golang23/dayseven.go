package main

import (
	"bufio"
	"cmp"
	"fmt"
	"slices"
	"strconv"
)

const HAND_SIZE = 5

type Card int

const (
	CARD_TWO Card = iota
	CARD_THREE
	CARD_FOUR
	CARD_FIVE
	CARD_SIX
	CARD_SEVEN
	CARD_EIGHT
	CARD_NINE
	CARD_TEN
	CARD_JACK
	CARD_QUEEN
	CARD_KING
	CARD_ACE
)

type HandType int

const (
	HIGH_CARD HandType = iota
	ONE_PAIR
	TWO_PAIR
	THREE_KIND
	FULL_HOUSE
	FOUR_KIND
	FIVE_KIND
)

type Hand struct {
	cards [HAND_SIZE]Card
	htype HandType
	bid   int
}

func compare_hand(h_one, h_two Hand) int {
	cmp_type := cmp.Compare(h_one.htype, h_two.htype)
	if cmp_type != 0 {
		return cmp_type
	}
	ruling := 0
	idx := 0
	for ruling == 0 {
		ruling = cmp.Compare(h_one.cards[idx], h_two.cards[idx])
		idx++
	}
	return ruling
}

func compute_hand(line string) Hand {
	raw_hand := line[:HAND_SIZE]
	raw_bid := line[HAND_SIZE+1:]
	var cards [5]Card
	var count [13]int
	for idx, byte_card := range raw_hand {
		switch byte_card {
		case '2':
			cards[idx] = CARD_TWO
			count[0]++
		case '3':
			cards[idx] = CARD_THREE
			count[1]++
		case '4':
			cards[idx] = CARD_FOUR
			count[2]++
		case '5':
			cards[idx] = CARD_FIVE
			count[3]++
		case '6':
			cards[idx] = CARD_SIX
			count[4]++
		case '7':
			cards[idx] = CARD_SEVEN
			count[5]++
		case '8':
			cards[idx] = CARD_EIGHT
			count[6]++
		case '9':
			cards[idx] = CARD_NINE
			count[7]++
		case 'T':
			cards[idx] = CARD_TEN
			count[8]++
		case 'J':
			cards[idx] = CARD_JACK
			count[9]++
		case 'Q':
			cards[idx] = CARD_QUEEN
			count[10]++
		case 'K':
			cards[idx] = CARD_KING
			count[11]++
		case 'A':
			cards[idx] = CARD_ACE
			count[12]++
		}
	}
	bid, _ := strconv.Atoi(raw_bid)
	score := 0
	for _, c := range count {
		switch c {
		case 2:
			score += 2
		case 3:
			score += 3
		case 4:
			score += 6
		case 5:
			score += 7
		}
	}
	var rank HandType
	switch score {
	case 2:
		rank = ONE_PAIR
	case 4:
		rank = TWO_PAIR
	case 3:
		rank = THREE_KIND
	case 5:
		rank = FULL_HOUSE
	case 6:
		rank = FOUR_KIND
	case 7:
		rank = FIVE_KIND
	default:
		rank = HIGH_CARD
	}
	return Hand{cards, rank, bid}
}

func get_winnings(scanner *bufio.Scanner) int {
	var hands []Hand
	for scanner.Scan() {
		line := scanner.Text()
		hand := compute_hand(line)
		hands = append(hands, hand)
	}
	slices.SortFunc(hands, compare_hand)
	winnings := 0
	for idx, hand := range hands {
		winnings += (idx + 1) * hand.bid
	}
	return winnings
}

func compute_joker_hand(hand Hand) Hand {
	joker_count := 0
	for _, card := range hand.cards {
		if card == CARD_JACK {
			joker_count++
		}
	}
	if joker_count == 1 {
		switch hand.htype {
		case HIGH_CARD:
			hand.htype = ONE_PAIR
		case ONE_PAIR:
			hand.htype = THREE_KIND
		case TWO_PAIR:
			hand.htype = FULL_HOUSE
		case THREE_KIND:
			hand.htype = FOUR_KIND
		case FOUR_KIND:
			hand.htype = FIVE_KIND
		}
	} else if joker_count == 2 {
		switch hand.htype {
		case ONE_PAIR:
			hand.htype = THREE_KIND
		case TWO_PAIR:
			hand.htype = FOUR_KIND
		case FULL_HOUSE:
			hand.htype = FIVE_KIND
		}
	} else if joker_count == 3 {
		switch hand.htype {
		case THREE_KIND:
			hand.htype = FOUR_KIND
		case FULL_HOUSE:
			hand.htype = FIVE_KIND
		}
	} else if joker_count == 4 {
		hand.htype = FIVE_KIND
	}
	return hand
}

func compare_joker_hand(h_one, h_two Hand) int {
	cmp_type := cmp.Compare(h_one.htype, h_two.htype)
	if cmp_type != 0 {
		return cmp_type
	}
	ruling := 0
	idx := 0
	for ruling == 0 {
		left_card := h_one.cards[idx]
		right_card := h_two.cards[idx]
		if left_card == CARD_JACK {
			left_card = CARD_TWO - 1
		}
		if right_card == CARD_JACK {
			right_card = CARD_TWO - 1
		}
		ruling = cmp.Compare(left_card, right_card)
		idx++
	}
	return ruling
}

func get_joker_winnings(scanner *bufio.Scanner) int {
	winnings := 0
	var hands []Hand
	for scanner.Scan() {
		line := scanner.Text()
		hand := compute_hand(line)
		hand = compute_joker_hand(hand)
		hands = append(hands, hand)
	}
	slices.SortFunc(hands, compare_joker_hand)
	for idx, hand := range hands {
		winnings += (idx + 1) * hand.bid
	}
	for _, h := range hands {
		fmt.Println(h)
	}
	return winnings
}
