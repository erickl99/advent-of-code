package main

import (
	"bufio"
	"strconv"
)

func parser(line string) int {
	idx := 0
	for line[idx] != ':' {
		idx++
	}
	idx += 2
	win_nums := make(map[int]bool)
	for line[idx] != '|' {
		if line[idx] == ' ' {
			idx++
			num, _ := strconv.Atoi(line[idx:idx+1])
			win_nums[num] = true
		} else {
			num, _ := strconv.Atoi(line[idx:idx+2])
			win_nums[num] = true
			idx++
		}
		idx += 2
	}
	idx += 2
	length := len(line)
	points := 0.5
	for idx < length {
		if line[idx] == ' ' {
			idx++
			num, _ := strconv.Atoi(line[idx:idx+1])
			if win_nums[num] {
				points *= 2
			}
		} else {
			num, _ := strconv.Atoi(line[idx:idx+2])
			idx++
			if win_nums[num] {
				points *= 2
			}
		}
		idx += 2
	}
	return int(points)
}

func winning_games(scanner *bufio.Scanner) int {
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		points := parser(line)
		sum += points
	}
	return sum
}

func parser_two(line string) int {
	idx := 0
	for line[idx] != ':' {
		idx++
	}
	idx += 2
	win_nums := make(map[int]bool)
	for line[idx] != '|' {
		if line[idx] == ' ' {
			idx++
			num, _ := strconv.Atoi(line[idx:idx+1])
			win_nums[num] = true
		} else {
			num, _ := strconv.Atoi(line[idx:idx+2])
			win_nums[num] = true
			idx++
		}
		idx += 2
	}
	idx += 2
	length := len(line)
	matches := 0
	for idx < length {
		if line[idx] == ' ' {
			idx++
			num, _ := strconv.Atoi(line[idx:idx+1])
			if win_nums[num] {
				matches++
			}
		} else {
			num, _ := strconv.Atoi(line[idx:idx+2])
			idx++
			if win_nums[num] {
				matches++
			}
		}
		idx += 2
	}
	return matches
}

func winning_cards(scanner *bufio.Scanner) int {
	sum := 0
	game_num := 1
	var stack []int
	game_matches := make(map[int]int)
	for scanner.Scan() {
		line := scanner.Text()
		matches := parser_two(line)
		game_matches[game_num] = matches
		for i := 1; i <= matches; i++ {
			stack = append(stack, game_num + i)
		}
		sum++
		game_num++
	}
	for len(stack) > 0 {
		idx := len(stack) - 1
		board_num := stack[idx]
		stack = stack[:idx]
		matches := game_matches[board_num]
		for i := 1; i <= matches; i++ {
			stack = append(stack, board_num + i)
		}
		sum++
	}
	return sum
}
