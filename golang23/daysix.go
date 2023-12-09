package main

import (
	"bufio"
	"fmt"
	"math"
	"regexp"
	"strconv"
)

func compute_quadratic(total_time int, distance int) {
	f_time := float64(total_time)
	discriminant := float64(total_time * total_time - 4 * distance)
	right := f_time + math.Sqrt(discriminant)
	left := f_time - math.Sqrt(discriminant)
	fmt.Println(math.Ceil(right/2) - math.Ceil(left/2))
}

func compute_inequality(total_time int, distance int) int {
	ways := 0
	for i := 1; i < total_time; i++ {
		if (total_time - i) * i > distance {
			ways++
		}
	}
	return ways
}

func record_smash(scanner *bufio.Scanner) int {
	scanner.Scan()
	re, _ := regexp.Compile("\\d+")
	time_strs := re.FindAllString(scanner.Text(), -1)
	scanner.Scan()
	dis_strs := re.FindAllString(scanner.Text(), -1)
	length := len(time_strs)
	result := 1
	for i := 0; i < length; i++ {
		total_time, _ := strconv.Atoi(time_strs[i])
		distance, _ := strconv.Atoi(dis_strs[i])
		result *= compute_inequality(total_time, distance)
		compute_quadratic(total_time, distance)
	}
	return result
}

func recover_number(line []byte) int {
	num := 0
	idx := 0
	for !is_digit(line[idx]) {
		idx++
	}
	length := len(line)
	for idx < length {
		dig := line[idx]
		if is_digit(dig) {
			num = 10 * num + int(dig - '0')
		}
		idx++
	}
	return num
}

func single_record(scanner *bufio.Scanner) int {
	scanner.Scan()
	line := scanner.Bytes()
	total_time := recover_number(line)
	scanner.Scan()
	line = scanner.Bytes()
	distance := recover_number(line)
	compute_quadratic(total_time, distance)
	return compute_inequality(total_time, distance)
}
