package main

import (
	"bufio"
	"fmt"
	"strings"
)

type pipe = int

const (
	LEFT_RIGHT pipe = iota + 1
	DOWN_UP
	LEFT_UP
	UP_RIGHT
	DOWN_RIGHT
	LEFT_DOWN
	GROUND
)

type direction = string

const (
	LEFT  direction = "L"
	RIGHT           = "R"
	UP              = "U"
	DOWN            = "D"
)

func pipes_to_string(pipe_line []pipe) string {
	var sb strings.Builder
	for _, p := range pipe_line {
		switch p {
		case LEFT_RIGHT:
			sb.WriteString("═")
		case DOWN_UP:
			sb.WriteString("║")
		case LEFT_UP:
			sb.WriteString("╝")
		case UP_RIGHT:
			sb.WriteString("╚")
		case DOWN_RIGHT:
			sb.WriteString("╔")
		case LEFT_DOWN:
			sb.WriteString("╗")
		default:
			sb.WriteString("X")
		}
	}
	return sb.String()
}

func step(p pipe, d direction) direction {
	switch p {
	case LEFT_RIGHT:
		return d
	case DOWN_UP:
		return d
	case LEFT_UP:
		if d == RIGHT {
			return UP
		} else {
			return LEFT
		}
	case UP_RIGHT:
		if d == DOWN {
			return RIGHT
		} else {
			return UP
		}
	case DOWN_RIGHT:
		if d == UP {
			return RIGHT
		} else {
			return DOWN
		}
	case LEFT_DOWN:
		if d == RIGHT {
			return DOWN
		} else {
			return LEFT
		}
	}
	return d
}

func get_outline(pipe_line []pipe) string {
	var sb strings.Builder
	for _, p := range pipe_line {
		switch p {
		case -LEFT_RIGHT:
			sb.WriteString("═")
		case -DOWN_UP:
			sb.WriteString("║")
		case -LEFT_UP:
			sb.WriteString("╝")
		case -UP_RIGHT:
			sb.WriteString("╚")
		case -DOWN_RIGHT:
			sb.WriteString("╔")
		case -LEFT_DOWN:
			sb.WriteString("╗")
		default:
			sb.WriteString("O")
		}
	}
	return sb.String()
}

func pipe_steps(scanner *bufio.Scanner) int {
	var maze [][]pipe
	line_num := 0
	start_x := -1
	start_y := -1
	for scanner.Scan() {
		line := scanner.Text()
		pipe_line := make([]pipe, len(line))
		for idx, loc := range line {
			switch loc {
			case '|':
				pipe_line[idx] = DOWN_UP
			case '-':
				pipe_line[idx] = LEFT_RIGHT
			case 'F':
				pipe_line[idx] = DOWN_RIGHT
			case '7':
				pipe_line[idx] = LEFT_DOWN
				idx++
			case 'L':
				pipe_line[idx] = UP_RIGHT
				idx++
			case 'J':
				pipe_line[idx] = LEFT_UP
				idx++
			case 'S':
				pipe_line[idx] = -DOWN_RIGHT
				start_y = line_num
				start_x = idx
			default:
				pipe_line[idx] = GROUND
			}
		}
		maze = append(maze, pipe_line)
		line_num++
	}
	current_x := start_x
	current_y := start_y + 1
	current_dir := DOWN
	steps := -1
	for current_x != start_x || current_y != start_y {
		p := maze[current_y][current_x]
		maze[current_y][current_x] = -p
		current_dir = step(p, current_dir)
		switch current_dir {
		case UP:
			current_y--
		case DOWN:
			current_y++
		case LEFT:
			current_x--
		case RIGHT:
			current_x++
		}
		steps++
	}
	tiles := 0
	for _, row := range maze {
		inside := false
		for _, loc := range row {
			if loc == -DOWN_UP || loc == -LEFT_UP || loc == -UP_RIGHT {
				inside = !inside
			} else if inside && loc > 0{
				tiles++
			}
		}
	}
	fmt.Println(tiles)
	return steps / 2
}
