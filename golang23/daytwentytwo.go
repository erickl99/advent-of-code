package main

import (
	"bufio"
	"fmt"
	"sort"
	"strconv"
	"strings"
)

const (
	X_AXIS = iota
	Y_AXIS
	Z_AXIS
)

type brick struct {
	vertex [3]int
	length int
	axis   int
	name   string
}

func (b brick) intersects(other brick, axis int) bool {
	b_range := [2]int{b.vertex[axis], b.vertex[axis]}
	o_range := [2]int{other.vertex[axis], other.vertex[axis]}
	if b.axis == axis {
		b_range[1] += b.length
	}
	if other.axis == axis {
		o_range[1] += other.length
	}
	case_one := b_range[0] <= o_range[0] && o_range[0] <= b_range[1]
	case_two := o_range[0] <= b_range[0] && b_range[0] <= o_range[1]
	return case_one || case_two
}

func (b brick) String() string {
	axis := ""
	switch b.axis {
	case X_AXIS:
		axis = "x"
	case Y_AXIS:
		axis = "y"
	case Z_AXIS:
		axis = "z"
	}
	str := fmt.Sprintf("Brick{Name:%s, Vertex:%v, Length: %d, Axis: %s}", b.name, b.vertex, b.length, axis)
	return str
}

func check_layer(bricks []brick, moving brick) int {
	placement := -1
	for _, br := range bricks {
		touches := moving.intersects(br, X_AXIS) && moving.intersects(br, Y_AXIS)
		if touches {
			new_placement := 1
			if br.axis == Z_AXIS {
				new_placement += br.length
			}
			if new_placement > placement {
				placement = new_placement
			}
		}
	}
	return placement
}

func simulate_fall(bricks []brick) [][]brick {
	var layers [][]brick
	var first_layer []brick
	num_bricks := len(bricks)
	idx := 0
	for idx < num_bricks {
		br := bricks[idx]
		if br.vertex[Z_AXIS] == 1 {
			first_layer = append(first_layer, br)
			fmt.Printf("Setting brick %v to layer 1\n", br)
		} else {
			break
		}
		idx++
	}
	layers = append(layers, first_layer)
	for idx < num_bricks {
		br := bricks[idx]
		lyr := br.vertex[Z_AXIS] - 2
		num_layers := len(layers)
		fmt.Println("Checking brick", br, lyr)
		for num_layers < lyr+1 {
			layers = append(layers, []brick{})
			num_layers++
		}
		finished := false
		for !finished {
			for lyr > 0 && len(layers[lyr]) == 0 {
				lyr--
			}
			change := check_layer(layers[lyr], br)
			if change > -1 {
				finished = true
			}
			if change == -1 && lyr == 0 {
				finished = true
				lyr = 0
			} else {
				lyr += change
			}
		}
		num_layers = len(layers)
		for num_layers <= lyr {
			layers = append(layers, []brick{})
			num_layers++
		}
		br.vertex[Z_AXIS] = lyr + 1
		layers[lyr] = append(layers[lyr], br)
		fmt.Printf("Moved brick %s to layer %d\n", br.name, lyr+1)
		idx++
	}
	return layers
}

func create_brick(coord_one, coord_two []string, name string) brick {
	vertex := [3]int{}
	length := 0
	axis := -1
	for idx, str_num := range coord_one {
		val, _ := strconv.Atoi(str_num)
		vertex[idx] = val
		if str_num != coord_two[idx] {
			val_two, _ := strconv.Atoi(coord_two[idx])
			if val > val_two {
				length = val - val_two
			} else {
				length = val_two - val
			}
			axis = idx
		}
	}
	new_brick := brick{name: name, vertex: vertex, length: length, axis: axis}
	return new_brick
}

func sand_slabs(scanner *bufio.Scanner) int {
	var bricks []brick
	name := 'A'
	num := 0
	for scanner.Scan() {
		line := scanner.Text()
		coords := strings.Split(line, "~")
		c_one := strings.Split(coords[0], ",")
		c_two := strings.Split(coords[1], ",")
		new_brick := create_brick(c_one, c_two, string(name)+strconv.Itoa(num))
		bricks = append(bricks, new_brick)
		name++
		if name > 'Z' {
			name = 'A'
		}
		num++
	}
	sort.Slice(bricks, func(i, j int) bool {
		return bricks[i].vertex[2] < bricks[j].vertex[2]
	})
	layers := simulate_fall(bricks)
	final_layer := 0
	total := 0
	idx := 0
	for idx < len(layers)-1 {
		current := layers[idx]
		above := layers[idx+1]
		neighbors := make(map[string][]string)
		for _, br := range current {
			for _, other := range above {
				supported := other.intersects(br, X_AXIS) && other.intersects(br, Y_AXIS)
				if supported {
					if _, ok := neighbors[other.name]; !ok {
						neighbors[other.name] = []string{}
					}
					neighbors[other.name] = append(neighbors[other.name], br.name)
				}
			}
		}
		valid := make(map[string]bool)
		for _, ls := range neighbors {
			if len(ls) > 1 {
				for _, name := range ls {
					valid[name] = true
				}
			}
		}
		total += len(valid)
		idx++
	}
	total += len(layers[final_layer])
	return total
}
