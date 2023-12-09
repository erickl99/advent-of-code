package main

import (
	"bufio"
	"regexp"
	"strconv"
)

func is_numeric(b byte) bool {
	return b >= '0' && b <= '9'
}

func first_last(scanner *bufio.Scanner) int {
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		idx := 0
		for !is_numeric(line[idx]) {
			idx++
		}
		jdx := len(line) - 1
		for !is_numeric(line[jdx]) {
			jdx--
		}
		number := 10*int(line[idx]-'0') + int(line[jdx]-'0')
		sum += number
	}
	return sum
}

func to_number(num_str string) int {
	if num_str == "" {
		return 0
	}
	switch num_str {
	case "one":
		return 1
	case "two":
		return 2
	case "three":
		return 3
	case "four":
		return 4
	case "five":
		return 5
	case "six":
		return 6
	case "seven":
		return 7
	case "eight":
		return 8
	case "nine":
		return 9
	}
	num, _ := strconv.Atoi(num_str)
	return num
}

func rev_string(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < len(runes)/2; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func reg_num(scanner *bufio.Scanner) int {
	sum := 0
	re, _ := regexp.Compile("[1-9]|one|two|three|four|five|six|seven|eight|nine")
	rev_re, _ := regexp.Compile("[1-9]|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin")
	for scanner.Scan() {
		line := scanner.Text()
		rev_line := rev_string(line)
		match := re.FindString(line)
		rev_match := rev_re.FindString(rev_line)
		num := 10*to_number(match) + to_number(rev_string(rev_match))
		sum += num
	}
	return sum
}
