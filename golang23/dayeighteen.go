package main

import (
	"bufio"
	"strconv"
	"strings"
)

type coord struct {
	x int
	y int
}

func get_instructions(line string) (string, int) {
	tokens := strings.Split(line, " ")
	direction := tokens[0]
	distance, _ := strconv.Atoi(tokens[1])
	return direction, distance
}

func hex_instructions(line string) (string, int) {
	tokens := strings.Split(line, " ")
	hex_num := tokens[2]
	distance, _ := strconv.ParseInt(hex_num[2:7], 16, 0)
	direction := hex_num[7:8]
	switch direction {
	case "0":
		return "R", int(distance)
	case "1":
		return "D", int(distance)
	case "2":
		return "L", int(distance)
	case "3":
		return "U", int(distance)
	}
	return "", int(distance)
}

func giant_lagoon(scanner *bufio.Scanner) int {
	edge_points := 0
	var vertices []coord
	x := 0
	y := 0
	for scanner.Scan() {
		direction, distance := hex_instructions(scanner.Text())
		switch direction {
		case "U":
			edge_points += distance
			y -= distance
			coord := coord{x, y}
			vertices = append(vertices, coord)
		case "D":
			edge_points += distance
			y += distance
			coord := coord{x, y}
			vertices = append(vertices, coord)
		case "L":
			edge_points += distance
			x -= distance
			coord := coord{x, y}
			vertices = append(vertices, coord)
		case "R":
			edge_points += distance
			x += distance
			coord := coord{x, y}
			vertices = append(vertices, coord)
		}
	}
	area := 0
	num_vertices := len(vertices)
	for i := 1; i < num_vertices - 1; i++ {
		area += vertices[i].y * (vertices[i-1].x - vertices[i+1].x)
	}
	area = area/2
	inner_points := area + 1 - edge_points/2
	return edge_points + inner_points
}

func lava_lagoon(scanner *bufio.Scanner) int {
	x := 0
	y := 0
	perimeter := make(map[coord]bool)
	min_height := 0
	min_width := 0
	max_height := 0
	max_width := 0
	for scanner.Scan() {
		direction, distance := get_instructions(scanner.Text())
		switch direction {
		case "U":
			for i := 0; i < distance; i++ {
				cd := coord{x, y - i}
				perimeter[cd] = true
			}
			y -= distance
		case "D":
			for i := 0; i < distance; i++ {
				cd := coord{x, y + i}
				perimeter[cd] = true
			}
			y += distance
		case "L":
			for j := 0; j < distance; j++ {
				cd := coord{x - j, y}
				perimeter[cd] = true
			}
			x -= distance
		case "R":
			for j := 0; j < distance; j++ {
				cd := coord{x + j, y}
				perimeter[cd] = true
			}
			x += distance
		}
		if x > max_width {
			max_width = x
		}
		if y > max_height {
			max_height = y
		}
		if x < min_width {
			min_width = x
		}
		if y < min_height {
			min_height = y
		}
	}
	outside_area := 0
	min_height--
	max_height++
	min_width--
	max_width++
	start := coord{min_width, min_height}
	stack := []coord{start}
	visited := make(map[coord]bool)
	for len(stack) > 0 {
		cd := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if !visited[cd] {
			outside_area++
			visited[cd] = true
			if cd.y > min_height {
				below := coord{cd.x, cd.y - 1}
				if !perimeter[below] {
					stack = append(stack, below)
				}
			}
			if cd.y < max_height {
				above := coord{cd.x, cd.y + 1}
				if !perimeter[above] {
					stack = append(stack, above)
				}
			}
			if cd.x > min_width {
				left := coord{cd.x - 1, cd.y}
				if !perimeter[left] {
					stack = append(stack, left)
				}
			}
			if cd.x < max_width {
				right := coord{cd.x + 1, cd.y}
				if !perimeter[right] {
					stack = append(stack, right)
				}
			}
		}
	}
	max_area := (max_height + 1 - min_height) * (max_width + 1 - min_width)
	return max_area - outside_area
}
