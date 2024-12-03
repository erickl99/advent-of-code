package main

import (
	"bufio"
	"math"
)

func traverse_garden(garden_plot [][]int, source int, steps int) int {
	distances := make([]int, len(garden_plot))
	for idx := range distances {
		distances[idx] = math.MaxInt
	}
	distances[source] = 0
	var queue [][2]int
	queue = append(queue, [2]int{source, 0})
	for len(queue) > 0 {
		node := -1
		min_dis := math.MaxInt
		min_idx := -1
		for idx, val := range queue {
			if val[1] < min_dis {
				min_dis = val[1]
				min_idx = idx
				node = val[0]
			}
		}
		queue = append(queue[:min_idx], queue[min_idx+1:]...)
		for _, neighbor := range garden_plot[node] {
			if distances[node]+1 < distances[neighbor] {
				distances[neighbor] = distances[node] + 1
				queue = append(queue, [2]int{neighbor, distances[neighbor]})
			}
		}
	}
	total := 0
	for _, dis := range distances {
		if dis == steps || dis < steps && (steps-dis)%2 == 0 {
			total++
		}
	}
	return total
}

var steps = make([]int, 65+131*2)
var seen = make(map[[3]int]bool)

func count_steps(garden_plot []string, x, y, depth, max_depth int) {
	if depth != max_depth {
		length := len(garden_plot)
		dirs := [][2]int{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
		for _, d := range dirs {
			new_x := x + d[0]
			new_y := y + d[1]
			i := ((new_y % length) + length) % length
			j := ((new_x % length) + length) % length
			if garden_plot[i][j] == '#' {
				continue
			}

			state := [3]int{new_x, new_y, depth}
			if _, ok := seen[state]; ok {
				continue
			}
			seen[state] = true
			steps[depth] += 1
			count_steps(garden_plot, new_x, new_y, depth+1, max_depth)
		}
	}
}

func infinite_garden(scanner *bufio.Scanner) int {
	var garden_plot []string
	start_x := 0
	start_y := 0
	line_idx := 0
	for scanner.Scan() {
		line := scanner.Text()
		garden_plot = append(garden_plot, line)
		for idx, char := range line {
			if char == 'S' {
				start_x = idx
				start_y = line_idx
			}
		}
		line_idx++
	}
	count_steps(garden_plot, start_x, start_y, 0, 65+131*2)
	val := 26501365 / 131
	linear := steps[64+131] - steps[64]
	quadratic := steps[64+131*2] - steps[64+131]
	result := val + linear*val + (val*(val-1)*2)*(quadratic-linear)
	return result
}

func garden_plot(scanner *bufio.Scanner) int {
	var garden_plot [][]int
	row_idx := 0
	source := 0
	var string_garden []string
	for scanner.Scan() {
		line := scanner.Text()
		string_garden = append(string_garden, line)
		for idx, char := range line {
			neighbors := []int{}
			garden_plot = append(garden_plot, neighbors)
			if char == 'S' {
				source = len(line)*row_idx + idx
			}
		}
		row_idx++
	}
	row_len := len(string_garden[0])
	for i, row := range string_garden {
		for j, char := range row {
			if char != '#' {
				if i-1 > -1 && string_garden[i-1][j] != '#' {
					garden_plot[i*row_len+j] = append(garden_plot[i*row_len+j], (i-1)*row_len+j)
				}
				if i+1 < len(string_garden) && string_garden[i+1][j] != '#' {
					garden_plot[i*row_len+j] = append(garden_plot[i*row_len+j], (i+1)*row_len+j)
				}
				if j-1 > -1 && string_garden[i][j-1] != '#' {
					garden_plot[i*row_len+j] = append(garden_plot[i*row_len+j], i*row_len+j-1)
				}
				if j+1 < len(string_garden) && string_garden[i][j+1] != '#' {
					garden_plot[i*row_len+j] = append(garden_plot[i*row_len+j], i*row_len+j+1)
				}
			}
		}
	}
	steps := 64
	traverse_garden(garden_plot, source, steps)
	for i := 0; i < row_len; i++ {
		traverse_garden(garden_plot, i, steps)
		traverse_garden(garden_plot, (len(string_garden)-1)*row_len+i, steps)
	}
	return 0
}
