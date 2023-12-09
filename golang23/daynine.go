package main

import (
	"bufio"
	"fmt"
	"regexp"
	"strconv"
)

const NUM_VALUES = 21

func calculate_new_value(data []int) (int,int) {
	counter := 1
	current_data := data
	differences := [][]int{data}
	for counter != 0 {
		counter = 0
		length := len(current_data)
		diffs := make([]int, length-1)
		for i := 0; i < length-1; i++ {
			delta := current_data[i+1] - current_data[i]
			if delta < 0 {
				counter -= delta
			} else {
				counter += delta
			}
			diffs[i] = delta
		}
		differences = append(differences, diffs)
		current_data = diffs
	}
	iterations := len(differences) - 1
	right_value := 0
	left_value := 0
	for iterations > -1 {
		right_value += differences[iterations][NUM_VALUES - iterations - 1]
		left_value = differences[iterations][0] - left_value
		iterations--
	}
	return left_value, right_value
}

func extrapolate(scanner *bufio.Scanner) int {
	re, _ := regexp.Compile("-?\\d+")
	data := make([]int, NUM_VALUES)
	right_sum := 0
	left_sum := 0
	for scanner.Scan() {
		tokens := re.FindAllString(scanner.Text(), -1)
		for idx, t := range tokens {
			num, _ := strconv.Atoi(t)
			data[idx] = num
		}
		left, right := calculate_new_value(data)
		right_sum += right
		left_sum += left
	}
	fmt.Printf("Right sum: %d\n", right_sum)
	fmt.Printf("Left sum: %d\n", left_sum)
	return left_sum
}
