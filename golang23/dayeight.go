package main

import (
	"bufio"
	"fmt"
	"regexp"
)

type node struct {
	left  string
	right string
}

const INIT_NODE = "AAA"
const FINAL_NODE = "ZZZ"

func create_node_map(scanner *bufio.Scanner) (map[string]node, []string) {
	node_map := make(map[string]node)
	re, _ := regexp.Compile("([A-Z]|[1-2])+")
	var start_nodes []string
	for scanner.Scan() {
		line := scanner.Text()
		tokens := re.FindAllString(line, -1)
		new_node := node{tokens[1], tokens[2]}
		node_map[tokens[0]] = new_node
		if tokens[0][2] == 'A' {
			start_nodes = append(start_nodes, tokens[0])
		}
	}
	return node_map, start_nodes
}

func calculate_steps(scanner *bufio.Scanner) int {
	scanner.Scan()
	directions := scanner.Text()
	scanner.Scan()
	node_map, _ := create_node_map(scanner)
	current_node := INIT_NODE
	idx := 0
	length := len(directions)
	steps := 0
	for current_node != FINAL_NODE {
		move := directions[idx]
		if move == 'L' {
			current_node = node_map[current_node].left
		} else {
			current_node = node_map[current_node].right
		}
		idx = (idx + 1) % length
		steps++
	}
	return steps
}

func finished(node_name string) bool {
	return node_name[2] == 'Z'
}

func ghost_steps(scanner *bufio.Scanner) int {
	scanner.Scan()
	directions := scanner.Text()
	scanner.Scan()

	node_map, start_nodes := create_node_map(scanner)
	idx := 0
	length := len(directions)
	steps := make([]int, len(start_nodes))
	for i, s_node := range start_nodes {
		current_node := s_node
		node_steps := 0
		for !finished(current_node) {
			if directions[idx] == 'L' {
				current_node = node_map[current_node].left
			} else {
				current_node = node_map[current_node].right
			}
			idx = (idx + 1) % length
			node_steps++
		}
		steps[i] = node_steps
		idx = 0
	}
	total_steps := 1
	for _, s := range steps {
		total_steps *= s
	}
	fmt.Println(steps)
	return 0
}
