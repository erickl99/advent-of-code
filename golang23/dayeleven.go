package main

import (
	"bufio"
	"fmt"
)

func find_galaxies(row string) []int {
	var indices []int
	for idx, loc := range row {
		if loc == '#' {
			indices = append(indices, idx)
		}
	}
	return indices
}

func add_galaxy(gal int, cols []int) []int {
	found := false
	idx := 0
	for !found && idx < len(cols) {
		if cols[idx] == gal {
			found = true
		}
		idx++
	}
	if !found {
		cols = append(cols, gal)
	}
	return cols
}

func compute_distance(i int, j int, galaxies [][2]int) int {
	gal_one := galaxies[i]
	gal_two := galaxies[j]
	hor_dir := 1
	ver_dir := 1
	if gal_one[1] > gal_two[1] {
		hor_dir = -1
	}
	if gal_one[0] > gal_two[0] {
		ver_dir = -1
	}
	distance := 0
	for x := gal_one[1]; x != gal_two[1]; x = x + hor_dir {
		found := false
		idx := 0
		for idx < len(galaxies) && !found {
			if galaxies[idx][1] == x {
				found = true
			}
			idx++
		}
		if !found {
			distance += 999999
		}
		distance++
	}
	for y := gal_one[0]; y != gal_two[0]; y = y + ver_dir {
		found := false
		idx := 0
		for idx < len(galaxies) && !found {
			if galaxies[idx][0] == y {
				found = true
			}
			idx++
		}
		if !found {
			distance += 999999
		}
		distance++
	}
	return distance
}

func shortest_path(scanner *bufio.Scanner) int {
	sum_distance := 0
	var space_map []string
	var galaxies [][2]int
	line_num := 0
	for scanner.Scan() {
		line := scanner.Text()
		for idx, loc := range line{
			if loc == '#' {
				galaxies = append(galaxies, [2]int{line_num, idx})
			}
		}
		space_map = append(space_map, line)
		line_num++
	}
	for _, v := range space_map {
		fmt.Println(v)
	}
	length := len(galaxies)
	for i := 0; i < length; i++ {
		for j := i + 1; j < length; j++ {
			sum_distance += compute_distance(i, j, galaxies)
		}
	}
	return sum_distance
}
