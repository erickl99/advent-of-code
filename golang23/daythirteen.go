package main

import (
	"bufio"
)

func find_palindrome(nums []int, left int) (int, int, int) {
	length := 0
	idx := left
	right := left + 2
	for left > -1 && right <= len(nums) && nums[left] == nums[right-1] {
		length++
		left--
		right++
	}
	if length > 0 {
		return idx - length + 1, idx + length + 1, idx
	}
	return -1, -1, -1
}

func differing_bit(x, y, rng int) bool {
	bit := 1
	power := 0
	xor := x ^ y
	found := false
	for i := 0; i < rng; i++ {
		if (xor ^ bit) == 0 {
			found = true
			break
		}
		bit = bit << 1
		power++
	}
	return found
}

func find_soft_palindrome(nums []int, left int, rng int) (int, int, int) {
	length := 0
	idx := left
	right := left + 2
	diffs := 0
	for left > -1 && right <= len(nums) && diffs < 2 {
		if nums[left] != nums[right-1] {
			if differing_bit(nums[left], nums[right-1], rng) {
				diffs++
			} else {
				return -1, -1, -1
			}
		}
		length++
		left--
		right++
	}
	if length > 0 && diffs == 1 {
		return idx - length + 1, idx + length + 1, idx
	}
	return -1, -1, -1
}

func find_reflection(nums []int, rng int) int {
	center := -1
	for idx := range nums {
		//p_left, p_right, p_center := find_soft_palindrome(nums, idx, rng)
		p_left, p_right, p_center := find_palindrome(nums, idx)
		if p_left == 0 || p_right == len(nums) {
			center = p_center
		}
	}
	return center + 1
}


func process_pattern(scanner *bufio.Scanner) int {
	var lines []string
	var bin_rows []int
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 {
			break
		}
		num := 0
		for _, spot := range line {
			if spot == '#' {
				num = 2*num + 1
			} else {
				num = 2 * num
			}
		}
		bin_rows = append(bin_rows, num)
		lines = append(lines, line)
	}
	if len(lines) == 0 {
		return -1
	}
	bin_cols := make([]int, len(lines[0]))
	for _, row := range lines {
		for j, spot := range row {
			if spot == '#' {
				bin_cols[j] = bin_cols[j]*2 + 1
			} else {
				bin_cols[j] = bin_cols[j] * 2
			}
		}
	}
	center := find_reflection(bin_rows, len(bin_cols))
	if center > 0 {
		result := 100 * (center)
		return result
	} else {
		center = find_reflection(bin_cols, len(bin_rows))
		return center
	}
}
func reflections(scanner *bufio.Scanner) int {
	sum := 0
	for val := process_pattern(scanner); val > -1; val = process_pattern(scanner) {
		sum += val
	}
	return sum
}
