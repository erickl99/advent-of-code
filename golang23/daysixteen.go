package main

import (
	"bufio"
)

const (
	RIGHT_MIRROR = iota + 1
	LEFT_MIRROR
	HOR_SPLITTER
	VER_SPLITTER
)

type splitter struct {
	side_one_checked bool
	side_two_checked bool
}

func step_light(i int, j int, d dir) (int, int) {
	switch d {
	case NORTH:
		i--
	case SOUTH:
		i++
	case EAST:
		j++
	case WEST:
		j--
	}
	return i, j
}

func dir_to_string(d dir) string {
	switch d {
	case NORTH:
		return "NORTH"
	case SOUTH:
		return "SOUTH"
	case EAST:
		return "EAST"
	case WEST:
		return "WEST"
	}
	return ""
}

func traverse_grid(tile_grid [][]int, i int, j int, d dir, splitter_tiles map[[2]int]splitter, energized_tiles map[[2]int]bool) {
	height := len(tile_grid)
	width := len(tile_grid[0])
	i, j = step_light(i, j, d)
	for i > -1 && i < height && j > -1 && j < width {
		tile := tile_grid[i][j]
		coord := [2]int{i, j}
		switch tile {
		case LEFT_MIRROR:
			switch d {
			case NORTH:
				d = WEST
			case SOUTH:
				d = EAST
			case EAST:
				d = SOUTH
			case WEST:
				d = NORTH
			}
		case RIGHT_MIRROR:
			switch d {
			case NORTH:
				d = EAST
			case SOUTH:
				d = WEST
			case EAST:
				d = NORTH
			case WEST:
				d = SOUTH
			}
		case HOR_SPLITTER:
			sp_tile := splitter_tiles[coord]
			if d == NORTH || d == SOUTH {
				if !sp_tile.side_one_checked {
					sp_tile.side_one_checked = true
					splitter_tiles[coord] = sp_tile
					traverse_grid(tile_grid, i, j, WEST, splitter_tiles, energized_tiles)
				}
				if !sp_tile.side_two_checked {
					sp_tile.side_two_checked = true
					splitter_tiles[coord] = sp_tile
					d = EAST
				} else {
					return
				}
			}
		case VER_SPLITTER:
			sp_tile := splitter_tiles[coord]
			if d == WEST || d == EAST {
				if !sp_tile.side_one_checked {
					sp_tile.side_one_checked = true
					splitter_tiles[coord] = sp_tile
					traverse_grid(tile_grid, i, j, NORTH, splitter_tiles, energized_tiles)
				}
				if !sp_tile.side_two_checked {
					sp_tile.side_two_checked = true
					splitter_tiles[coord] = sp_tile
					d = SOUTH
				} else {
					return
				}
			}
		}
		energized_tiles[coord] = true
		i, j = step_light(i, j, d)
	}
}

func clean_splitter_tiles(splitter_tiles map[[2]int]splitter) {
	clean_splitter := splitter{false, false}
	for key := range splitter_tiles {
		splitter_tiles[key] = clean_splitter
	}
}

func light_tiles(scanner *bufio.Scanner) int {
	var tile_grid [][]int
	splitter_tiles := make(map[[2]int]splitter)
	line_num := 0
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for idx, char := range line {
			switch char {
			case '/':
				row[idx] = RIGHT_MIRROR
			case '\\':
				row[idx] = LEFT_MIRROR
			case '|':
				row[idx] = VER_SPLITTER
				sp := splitter{false, false}
				coord := [2]int{line_num, idx}
				splitter_tiles[coord] = sp
			case '-':
				row[idx] = HOR_SPLITTER
				sp := splitter{false, false}
				coord := [2]int{line_num, idx}
				splitter_tiles[coord] = sp
			}
		}
		tile_grid = append(tile_grid, row)
		line_num++
	}
	max_tiles := 0
	for idx, row := range tile_grid {
		e_tiles_one := make(map[[2]int]bool)
		traverse_grid(tile_grid, idx, -1, EAST, splitter_tiles, e_tiles_one)
		if len(e_tiles_one) > max_tiles {
			max_tiles = len(e_tiles_one)
		}
		clean_splitter_tiles(splitter_tiles)
		e_tiles_two := make(map[[2]int]bool)
		traverse_grid(tile_grid, idx, len(row), WEST, splitter_tiles, e_tiles_two)
		if len(e_tiles_two) > max_tiles {
			max_tiles = len(e_tiles_two)
		}
		clean_splitter_tiles(splitter_tiles)
	}
	first_row := tile_grid[0]
	for idx := range first_row {
		e_tiles_one := make(map[[2]int]bool)
		traverse_grid(tile_grid, -1, idx, SOUTH, splitter_tiles, e_tiles_one)
		if len(e_tiles_one) > max_tiles {
			max_tiles = len(e_tiles_one)
		}
		clean_splitter_tiles(splitter_tiles)
		e_tiles_two := make(map[[2]int]bool)
		traverse_grid(tile_grid, len(first_row), idx, NORTH, splitter_tiles, e_tiles_two)
		if len(e_tiles_two) > max_tiles {
			max_tiles = len(e_tiles_two)
		}
		clean_splitter_tiles(splitter_tiles)
	}
	return max_tiles
}
