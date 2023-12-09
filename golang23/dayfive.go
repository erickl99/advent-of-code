package main

import (
	"bufio"
	"fmt"
	"math"
	"regexp"
	"strconv"
	"strings"
	"sync"
)

type mapper = func(int) int

var single_re, _ = regexp.Compile("\\d+")
var pair_re, _ = regexp.Compile("\\d+ \\d+")

func skip(scanner *bufio.Scanner, n int) {
	for i := 0; i < n; i++ {
		scanner.Scan()
		scanner.Text()
	}
}

func create_mapper(dest int, src int, rangee int) mapper {
	new_func := func(n int) int {
		if n >= src && n < src+rangee {
			return dest + (n - src)
		}
		return -1
	}
	return new_func
}

func parse_mappers(scanner *bufio.Scanner, mapper_ls []mapper) []mapper {
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			skip(scanner, 1)
			break
		}
		param_strs := single_re.FindAllString(line, 3)
		dest, _ := strconv.Atoi(param_strs[0])
		src, _ := strconv.Atoi(param_strs[1])
		rangee, _ := strconv.Atoi(param_strs[2])
		mapper_ls = append(mapper_ls, create_mapper(dest, src, rangee))
	}
	return mapper_ls
}

func process_seed(num int, mapper_pipeline [][]mapper) int {
	total_value := num
	for _, stage := range mapper_pipeline {
		idx := 0
		length := len(stage)
		stage_value := -1
		for stage_value == -1 && idx < length {
			map_func := stage[idx]
			stage_value = map_func(total_value)
			idx++
		}
		if stage_value != -1 {
			total_value = stage_value
		}
	}
	return total_value
}

func create_pipeline(scanner *bufio.Scanner) [][]mapper {
	var mapper_pipeline [][]mapper
	mapper_ls := []mapper{}
	mapper_ls = parse_mappers(scanner, mapper_ls)
	for len(mapper_ls) > 0 {
		mapper_pipeline = append(mapper_pipeline, mapper_ls)
		mapper_ls = []mapper{}
		mapper_ls = parse_mappers(scanner, mapper_ls)
	}
	return mapper_pipeline
}

func min_location(scanner *bufio.Scanner) int {
	scanner.Scan()
	seeds := single_re.FindAllString(scanner.Text(), -1)
	seed_nums := make([]int, len(seeds))
	for idx, sd := range seeds {
		num, _ := strconv.Atoi(sd)
		seed_nums[idx] = num
	}
	skip(scanner, 2)
	mapper_pipeline := create_pipeline(scanner)
	min_loc := math.MaxInt
	for _, num := range seed_nums {
		total_value := process_seed(num, mapper_pipeline)
		if total_value < min_loc {
			min_loc = total_value
		}
	}
	return min_loc
}

func min_range_location(scanner *bufio.Scanner) int {
	scanner.Scan()
	pairs := pair_re.FindAllString(scanner.Text(), -1)
	seed_ranges := make([][2]int, 10)
	for idx, sr := range pairs {
		nums := strings.Split(sr, " ")
		start, _ := strconv.Atoi(nums[0])
		end, _ := strconv.Atoi(nums[1])
		interval := [2]int{start, start + end}
		seed_ranges[idx] = interval
	}
	skip(scanner, 2)
	mapper_pipeline := create_pipeline(scanner)
	var wg sync.WaitGroup
	for _, interval := range seed_ranges {
		wg.Add(1)
		go func(itvl [2]int) {
			defer wg.Done()
			min_loc := math.MaxInt
			for i := itvl[0]; i < itvl[1]; i++ {
				total_value := process_seed(i, mapper_pipeline)
				if total_value < min_loc {
					min_loc = total_value
				}
			}
			fmt.Println(min_loc)
		}(interval)
	}
	wg.Wait()
	return 0
}
