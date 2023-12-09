package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
)

func extract_number(j int, line []byte, erase bool) (int, int) {
	left := j
	right := j + 1
	length := len(line)
	for left > 0 && is_digit(line[left-1]) {
		left--
	}
	for right < length && is_digit(line[right]) {
		right++
	}
	num := 0
	for k := left; k < right; k++ {
		digit := int(line[k] - '0')
		num = num*10 + digit
		if erase {
			line[k] = '\000'
		}
	}
	return num, right
}

func is_digit(b byte) bool {
	return b >= '0' && b <= '9'
}

func check_neighbors(i int, j int, schematic [][]byte) int {
	idx := j - 1

	sum := 0
	for idx < j+2 {
		if is_digit(schematic[i-1][idx]) {
			num, new_idx := extract_number(idx, schematic[i-1], true)
			sum += num
			idx = new_idx

		} else {
			idx++
		}
	}
	if is_digit(schematic[i][j-1]) {
		num, _ := extract_number(j-1, schematic[i], true)
		sum += num

	}
	if is_digit(schematic[i][j+1]) {
		num, _ := extract_number(j+1, schematic[i], true)
		sum += num

	}
	idx = j - 1
	for idx < j+2 {
		if is_digit(schematic[i+1][idx]) {
			num, new_idx := extract_number(idx, schematic[i+1], true)
			sum += num
			idx = new_idx
		} else {
			idx++
		}
	}
	return sum
}

func engine_part(scanner *bufio.Scanner) int {
	var schematic [][]byte
	re, err := regexp.Compile("\\+|-|\\*|/|=|%|&|@|\\$|#")
	if err != nil {
		fmt.Println("Failed to make regular expression")
		os.Exit(1)
	}
	var indices [][][]int
	for scanner.Scan() {
		line := scanner.Bytes()
		line_copy := make([]byte, len(line))
		copy(line_copy, line)
		schematic = append(schematic, line_copy)
		matches := re.FindAllIndex(line, -1)
		indices = append(indices, matches)
	}
	sum := 0
	for idx, row := range indices {
		for _, pair := range row {
			sum += check_neighbors(idx, pair[0], schematic)
		}
	}
	return sum
}

func verify_gear(i int, j int, schematic [][]byte) int {
	idx := j - 1
	nums_found := 0
	ratio := 1
	for idx < j+2 {
		if is_digit(schematic[i-1][idx]) {
			num, new_idx := extract_number(idx, schematic[i-1], false)
			ratio *= num
			idx = new_idx
			nums_found++
		} else {
			idx++
		}
	}
	if is_digit(schematic[i][j-1]) {
		num, _ := extract_number(j-1, schematic[i], false)
		ratio *= num
		nums_found++
	}
	if is_digit(schematic[i][j+1]) {
		num, _ := extract_number(j+1, schematic[i], false)
		ratio *= num
		nums_found++
	}
	idx = j - 1
	for idx < j+2 {
		if is_digit(schematic[i+1][idx]) {
			num, new_idx := extract_number(idx, schematic[i+1], false)
			ratio *= num
			idx = new_idx
			nums_found++
		} else {
			idx++
		}
	}
	if nums_found != 2 {
		return 0
	}
	return ratio
}

func gear_ratio(scanner *bufio.Scanner) int {
	var schematic [][]byte
	re, err := regexp.Compile("\\*")
	if err != nil {
		fmt.Println("Failed to make regular expression")
		os.Exit(1)
	}
	var indices [][][]int
	for scanner.Scan() {
		line := scanner.Bytes()
		line_copy := make([]byte, len(line))
		copy(line_copy, line)
		schematic = append(schematic, line_copy)
		matches := re.FindAllIndex(line, -1)
		indices = append(indices, matches)
	}
	sum := 0
	for idx, row := range indices {
		for _, pair := range row {
			sum += verify_gear(idx, pair[0], schematic)
		}
	}
	return sum
}
