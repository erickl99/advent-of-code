package main

import (
	"bufio"
	"fmt"
	"math"
	"strings"
)

func visited(path [][2]int, coord [2]int) bool {
	for _, c := range path {
		if c == coord {
			return true
		}
	}
	return false
}

var max_path = 0

func traverse_trail(trail []string, direction int, i int, j int, path [][2]int) {
	if trail[i][j] == 'E' {
		if len(path) > max_path {
			max_path = len(path)
		}
		return
	}
	if i-1 > -1 && direction != SOUTH && trail[i-1][j] != '#' && !visited(path, [2]int{i - 1, j}) {
		new_path := make([][2]int, len(path)+1)
		copy(new_path, path)
		new_path[len(path)] = [2]int{i - 1, j}
		traverse_trail(trail, NORTH, i-1, j, new_path)
	}
	if i+1 < len(trail) && direction != NORTH && trail[i+1][j] != '#' && !visited(path, [2]int{i + 1, j}) {
		new_path := make([][2]int, len(path)+1)
		copy(new_path, path)
		new_path[len(path)] = [2]int{i + 1, j}
		traverse_trail(trail, SOUTH, i+1, j, new_path)
	}
	if j-1 > -1 && direction != EAST && trail[i][j-1] != '#' && !visited(path, [2]int{i, j - 1}) {
		new_path := make([][2]int, len(path)+1)
		copy(new_path, path)
		new_path[len(path)] = [2]int{i, j - 1}
		traverse_trail(trail, WEST, i, j-1, new_path)
	}
	if j+1 < len(trail[i]) && direction != WEST && trail[i][j+1] != '#' && !visited(path, [2]int{i, j + 1}) {
		new_path := make([][2]int, len(path)+1)
		copy(new_path, path)
		new_path[len(path)] = [2]int{i, j + 1}
		traverse_trail(trail, EAST, i, j+1, new_path)
	}
}

func possible_tiles(trail []string, direction int, i int, j int, path [][2]int) [][3]int {
	var tiles [][3]int
	if i-1 > -1 && direction != SOUTH && trail[i-1][j] != '#' && !visited(path, [2]int{i - 1, j}) {
		tiles = append(tiles, [3]int{i - 1, j, NORTH})
	}
	if i+1 < len(trail) && direction != NORTH && trail[i+1][j] != '#' && !visited(path, [2]int{i + 1, j}) {
		tiles = append(tiles, [3]int{i + 1, j, SOUTH})
	}
	if j-1 > -1 && direction != EAST && trail[i][j-1] != '#' && !visited(path, [2]int{i, j - 1}) {
		tiles = append(tiles, [3]int{i, j - 1, WEST})
	}
	if j+1 < len(trail[i]) && direction != WEST && trail[i][j+1] != '#' && !visited(path, [2]int{i, j + 1}) {
		tiles = append(tiles, [3]int{i, j + 1, EAST})
	}
	return tiles
}

var current_path [][2]int
var visits = make(map[[2]int]bool)

func longer_trail(trail []string, i int, j int) {
	coord := [2]int{i, j}
	if visits[coord] {
		return
	}
	visits[coord] = true
	current_path = append(current_path, coord)
	if trail[i][j] == 'E' {
		if len(current_path) > max_path {
			max_path = len(current_path)
		}
		visits[coord] = false
		current_path = current_path[:len(current_path)-1]
		return
	}
	if i-1 > -1 && trail[i-1][j] != '#' {
		longer_trail(trail, i-1, j)
	}
	if i+1 < len(trail) && trail[i+1][j] != '#' {
		longer_trail(trail, i+1, j)
	}
	if j-1 > -1 && trail[i][j-1] != '#' {
		longer_trail(trail, i, j-1)
	}
	if i+1 < len(trail[0]) && trail[i][j+1] != '#' {
		longer_trail(trail, i, j+1)
	}
	current_path = current_path[:len(current_path)-1]
	visits[coord] = false
}

var stack_thing [][2]int

func topological(trail []string, i, j int) {
	coord := [2]int{i, j}
	visits[coord] = true
	if i-1 > -1 && trail[i-1][j] != '#' && !visits[[2]int{i - 1, j}] {
		topological(trail, i-1, j)
	}
	if i+1 < len(trail) && trail[i+1][j] != '#' && !visits[[2]int{i + 1, j}] {
		topological(trail, i+1, j)
	}
	if j-1 > -1 && trail[i][j-1] != '#' && !visits[[2]int{i, j - 1}] {
		topological(trail, i, j-1)
	}
	if i+1 < len(trail[0]) && trail[i][j+1] != '#' && !visits[[2]int{i, j + 1}] {
		topological(trail, i, j+1)
	}
	stack_thing = append(stack_thing, coord)
}

func longest_hike(scanner *bufio.Scanner) int {
	var trail []string
	num_tiles := 0
	for scanner.Scan() {
		line := scanner.Text()
		trail = append(trail, line)
		num_tiles += len(line)
	}
	last_line := trail[len(trail)-1]
	last_line = strings.Replace(last_line, ".", "E", -1)
	trail[len(trail)-1] = last_line
	distances := make([]int, num_tiles)
	for i, row := range trail {
		for j, tile := range row {
			if tile != '#' && !visits[[2]int{i, j}] {
				topological(trail, i, j)
			}
		}
	}
	for idx := range distances {
		distances[idx] = math.MinInt
	}
	distances[1] = 0
	for len(stack_thing) > 0 {
		coord := stack_thing[len(stack_thing)-1]
		stack_thing = stack_thing[:len(stack_thing)-1]
		idx := len(trail)*coord[0] + coord[1]
		if distances[idx] != math.MinInt {
			fmt.Println("in here", idx)
			i := coord[0]
			j := coord[1]
			if i-1 > -1 && trail[i-1][j] != '#' && !visits[[2]int{i - 1, j}] {
				n_idx := len(trail)*(coord[0]-1) + coord[1]
				if distances[idx]+1 < distances[n_idx] {
					distances[n_idx] = distances[idx] + 1
				}
			}
			if i+1 < len(trail) && trail[i+1][j] != '#' && !visits[[2]int{i + 1, j}] {
				n_idx := len(trail)*(coord[0]+1) + coord[1]
				if distances[idx]+1 < distances[n_idx] {
					distances[n_idx] = distances[idx] + 1
				}
			}
			if j-1 > -1 && trail[i][j-1] != '#' && !visits[[2]int{i, j - 1}] {
				n_idx := len(trail)*(coord[0]) + coord[1] - 1
				if distances[idx]+1 < distances[n_idx] {
					distances[n_idx] = distances[idx] + 1
				}
			}
			if j+1 < len(trail[0]) && trail[i][j+1] != '#' && !visits[[2]int{i, j + 1}] {
				n_idx := len(trail)*(coord[0]) + coord[1] + 1
				if distances[idx]+1 < distances[n_idx] {
					distances[n_idx] = distances[idx] + 1
				}
			}
		}
	}
	fmt.Println(distances)
	return max_path
}
