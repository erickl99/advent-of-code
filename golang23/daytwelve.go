package main

import (
	"bufio"
	"strconv"
	"strings"
)

const FOLD = 5

func valid_springs(spring []byte, damaged []int) bool {
	count := 0
	idx := 0
	for _, b := range spring {
		if b == '#' {
			count++
		} else {
			if count > 0 {
				if idx >= len(damaged) || count != damaged[idx] {
					return false
				}
				count = 0
				idx++
			}
		}
	}
	if count > 0 && idx == len(damaged)-1 {
		return count == damaged[idx]
	}
	if count > 0 {
		return false
	}
	return idx == len(damaged)
}

func find_springs(row string, idx int, damaged []int, space []byte) int {
	if idx >= len(row) {
		if valid_springs(space, damaged) {
			return 1
		} else {
			return 0
		}
	}
	if row[idx] == '?' {
		space[idx] = '.'
		sum_one := find_springs(row, idx+1, damaged, space)
		space[idx] = '#'
		sum_two := find_springs(row, idx+1, damaged, space)
		return sum_one + sum_two
	} else {
		space[idx] = row[idx]
		return find_springs(row, idx+1, damaged, space)
	}
}

func count_springs(scanner *bufio.Scanner) int {
	count := 0
	for scanner.Scan() {
		tokens := strings.Split(scanner.Text(), " ")
		row := tokens[0]
		raw_damaged := strings.Split(tokens[1], ",")
		damaged := make([]int, len(raw_damaged))
		for idx, rd := range raw_damaged {
			num, _ := strconv.Atoi(rd)
			damaged[idx] = num
		}
		count += efficient_springs(row, damaged, 0, 0, 0, make(map[[3]int]int))
	}
	return count
}

func efficient_springs(row string, damaged []int, r_idx int, d_idx int, d_length int, dp map[[3]int]int) int {
	state := [3]int{len(row) - r_idx, len(damaged) - d_idx, d_length}
	if res, ok := dp[state]; ok {
		return res
	}
	if r_idx == len(row) {
		if d_idx == len(damaged) && d_length == 0 {
			dp[state] = 1
			return 1
		} else if d_idx == len(damaged)-1 && damaged[d_idx] == d_length {
			dp[state] = 1
			return 1
		} else {
			dp[state] = 0
			return 0
		}
	}
	valid := 0
	if row[r_idx] == '.' || row[r_idx] == '?' {
		if d_length == 0 {
			valid += efficient_springs(row, damaged, r_idx+1, d_idx, 0, dp)
		} else if d_idx < len(damaged) && damaged[d_idx] == d_length {
			valid += efficient_springs(row, damaged, r_idx+1, d_idx+1, 0, dp)
		}
	}
	if row[r_idx] == '#' || row[r_idx] == '?' {
		valid += efficient_springs(row, damaged, r_idx+1, d_idx, d_length+1, dp)
	}
	dp[state] = valid
	return valid
}

func count_springs_long(scanner *bufio.Scanner) int {
	count := 0
	for scanner.Scan() {
		tokens := strings.Split(scanner.Text(), " ")
		row := tokens[0]
		raw_damaged := strings.Split(tokens[1], ",")
		length := len(raw_damaged)
		damaged := make([]int, length*FOLD)
		for idx, rd := range raw_damaged {
			num, _ := strconv.Atoi(rd)
			for i := 0; i < FOLD; i++ {
				damaged[i*length+idx] = num
			}
		}
		var sb strings.Builder
		for i := 0; i < FOLD-1; i++ {
			sb.WriteString(row)
			sb.WriteString("?")
		}
		sb.WriteString(row)
		count += efficient_springs(sb.String(), damaged, 0, 0, 0, make(map[[3]int]int))
	}
	return count
}
