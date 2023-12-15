package main

import (
	"bufio"
)

type lens struct {
	label        string
	focal_length int
}

func set_lens(boxes map[int][]lens, box int, new_lens lens) {
	if _, ok := boxes[box]; !ok {
		var list []lens
		boxes[box] = list
	}
	idx := 0
	lenses := boxes[box]
	label := new_lens.label
	for idx < len(lenses) {
		if lenses[idx].label == label {
			lenses[idx].focal_length = new_lens.focal_length
			break
		}
		idx++
	}
	if idx == len(lenses) {
		new_ls := append(lenses, new_lens)
		boxes[box] = new_ls
	}
}

func remove_lens(boxes map[int][]lens, box int, label string) {
	lenses := boxes[box]
	for i := 0; i < len(lenses); i++ {
		if lenses[i].label == label {
			boxes[box] = append(lenses[:i], lenses[i+1:]...)
		}
	}
}

func reindeer_hashing(scanner *bufio.Scanner) int {
	boxes := make(map[int][]lens)
	for scanner.Scan() {
		line := scanner.Text()
		box := 0
		left := 0
		right := left
		for i := 0; i < len(line); i++ {
			char := line[i]
			switch char {
			case '=':
				focal_length := int(line[i+1] - '0')
				new_lens := lens{line[left:right], focal_length}
				set_lens(boxes, box, new_lens)
				i++
			case '-':
				label := line[left:right]
				remove_lens(boxes, box, label)
			case ',':
				box = 0
				left = i + 1
				right = left
			default:
				box = ((box + int(char)) * 17) % 256
				right++
			}
		}
	}
	f_power := 0
	for box, lenses := range boxes {
		for idx, l := range lenses {
			f_power += (box + 1) * (idx + 1) * l.focal_length
		}
	}
	return f_power
}
