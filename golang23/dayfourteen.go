package main

import (
	"bufio"
	"fmt"
)

type rock = int

const (
	EMPTY rock = iota
	ROUND
	SQUARE
)

type dir = int

const (
	NORTH dir = iota
	SOUTH
	EAST
	WEST
)

const ROW_LENGTH = 100
const COL_LENGTH = ROW_LENGTH

func move_rock(platform_map [][]int, i int, j int, d dir) {
	switch d {
	case NORTH:
		platform_map[i][j] = EMPTY
		for i > 0 && platform_map[i-1][j] == EMPTY {
			i--
		}
		platform_map[i][j] = ROUND
	case SOUTH:
		platform_map[i][j] = EMPTY
		for i < len(platform_map)-1 && platform_map[i+1][j] == EMPTY {
			i++
		}
		platform_map[i][j] = ROUND
	case EAST:
		platform_map[i][j] = EMPTY
		for j < len(platform_map[i])-1 && platform_map[i][j+1] == EMPTY {
			j++
		}
		platform_map[i][j] = ROUND
	case WEST:
		platform_map[i][j] = EMPTY
		for j > 0 && platform_map[i][j-1] == EMPTY {
			j--
		}
		platform_map[i][j] = ROUND
	}
}

func encode_platform(platform_map [][]int) [ROW_LENGTH]string {
	encoded := [COL_LENGTH]string{}
	space := [ROW_LENGTH]byte{}
	for i, row := range platform_map {
		for j, rock := range row {
			switch rock {
			case EMPTY:
				space[j] = '.'
			case ROUND:
				space[j] = 'O'
			case SQUARE:
				space[j] = '#'
			}
		}
		encoded[i] = string(space[:])
	}
	return encoded
}

func perform_tilt(platform_map [][]int, d dir) {
	height := len(platform_map)
	width := len(platform_map[0])
	switch d {
	case NORTH:
		for i := 0; i < height; i++ {
			for j := 0; j < width; j++ {
				if platform_map[i][j] == ROUND {
					move_rock(platform_map, i, j, d)
				}
			}
		}
	case SOUTH:
		for i := height - 1; i > -1; i-- {
			for j := 0; j < width; j++ {
				if platform_map[i][j] == ROUND {
					move_rock(platform_map, i, j, d)
				}
			}
		}
	case EAST:
		for i := 0; i < height; i++ {
			for j := width - 1; j > -1; j-- {
				if platform_map[i][j] == ROUND {
					move_rock(platform_map, i, j, d)
				}
			}
		}
	case WEST:
		for i := 0; i < height; i++ {
			for j := 0; j < width; j++ {
				if platform_map[i][j] == ROUND {
					move_rock(platform_map, i, j, d)
				}
			}
		}
	}
}

func print_platform(platform_map [][]int) {
	for _, v := range platform_map {
		fmt.Println(v)
	}
}

func rock_load(scanner *bufio.Scanner) int {
	var platform_map [][]int
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for idx, val := range line {
			switch val {
			case '.':
				row[idx] = EMPTY
			case 'O':
				row[idx] = ROUND
			case '#':
				row[idx] = SQUARE
			}
		}
		platform_map = append(platform_map, row)
	}
	encode_cache := make(map[[ROW_LENGTH]string]int)
	init_enc := encode_platform(platform_map)
	encode_cache[init_enc] = 0
	// fmt.Println("Adding initial state:")
	// for _, e := range init_enc {
	// 	fmt.Println(e)
	// }
	// fmt.Println()
	cycle_length := 0
	cycle_start := 0
	for i := 1; i <= 1000; i++ {
		perform_tilt(platform_map, NORTH)
		perform_tilt(platform_map, WEST)
		perform_tilt(platform_map, SOUTH)
		perform_tilt(platform_map, EAST)
		new_enc := encode_platform(platform_map)
		if iter, ok := encode_cache[new_enc]; ok {
			// for _, e := range new_enc {
			// 	fmt.Println(e)
			// }
			fmt.Printf("Found cycle, after %d iterations, starts at %d\n", i, iter)
			cycle_length = i - iter
			cycle_start = iter
			break
		} else {
			// fmt.Println("Adding encoding:")
			// for _, e := range new_enc {
			// 	fmt.Println(e)
			// }
			// fmt.Println()
			encode_cache[new_enc] = i
		}
	}
	leftover := (1000000000 - cycle_start) % cycle_length
	for i := 0; i < leftover; i++ {
		perform_tilt(platform_map, NORTH)
		perform_tilt(platform_map, WEST)
		perform_tilt(platform_map, SOUTH)
		perform_tilt(platform_map, EAST)
	}
	total_load := 0
	multiplier := len(platform_map)
	for _, row := range platform_map {
		count := 0
		for _, rock := range row {
			if rock == ROUND {
				count++
			}
		}
		total_load += count * multiplier
		multiplier--
	}
	return total_load
}
