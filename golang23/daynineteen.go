package main

import (
	"bufio"
	"fmt"
	"strconv"
	"strings"
)

const START = "in"

type rule struct {
	category    byte
	less_than   bool
	value       int
	destination string
}

type workflow struct {
	rules []rule
	final string
}

type interval struct {
	start int
	end   int
}

type state = [4]interval

func (w workflow) String() string {
	var sb strings.Builder
	sb.WriteString("{")
	for _, rl := range w.rules {
		if rl.less_than {
			str := fmt.Sprintf("%c < %d -> %s", rl.category, rl.value, rl.destination)
			sb.WriteString(str)
		} else {
			str := fmt.Sprintf("%c > %d -> %s", rl.category, rl.value, rl.destination)
			sb.WriteString(str)
		}
		sb.WriteString(", ")
	}
	sb.WriteString(w.final + "}")
	return sb.String()
}

type machine_part struct {
	x int
	m int
	a int
	s int
}

func (mp machine_part) sum() int {
	return mp.x + mp.m + mp.a + mp.s
}

func extract_workflow(str_flow string) workflow {
	tokens := strings.Split(str_flow, ",")
	length := len(tokens)
	rules := make([]rule, length-1)
	for i := 0; i < length-1; i++ {
		predicate := tokens[i]
		cat := predicate[0]
		idx := strings.IndexByte(predicate, ':')
		value, _ := strconv.Atoi(predicate[2:idx])
		destination := predicate[idx+1:]
		if predicate[1] == '<' {
			new_rule := rule{cat, true, value, destination}
			rules[i] = new_rule
		} else {
			new_rule := rule{cat, false, value, destination}
			rules[i] = new_rule
		}
	}
	return workflow{rules, tokens[length-1]}
}

func extract_part(str_part string) machine_part {
	tokens := strings.Split(str_part, ",")
	part := machine_part{}
	for _, def := range tokens {
		num, _ := strconv.Atoi(def[2:])
		switch def[0] {
		case 'x':
			part.x = num
		case 'm':
			part.m = num
		case 'a':
			part.a = num
		case 's':
			part.s = num
		}
	}
	return part
}

func valid_judgment(value int, extrema int, less_than bool) bool {
	if less_than {
		return value < extrema
	} else {
		return value > extrema
	}
}

func process_part(part machine_part, flows map[string]workflow, start string) bool {
	current := start
	for current != "A" && current != "R" {
		w_flow := flows[current]
		judged := false
		for _, rule := range w_flow.rules {
			num := 0
			switch rule.category {
			case 'x':
				num = part.x
			case 'm':
				num = part.m
			case 'a':
				num = part.a
			case 's':
				num = part.s
			}
			if valid_judgment(num, rule.value, rule.less_than) {
				current = rule.destination
				judged = true
				break
			}
		}
		if !judged {
			current = w_flow.final
		}
	}
	return current == "A"
}

const (
	LESS = iota
	GREATER
	GREATER_EQUAL
	LESS_EQUAL
)

func update_interval(rng interval, extrema int, inequality int) interval {
	switch inequality {
	case LESS:
		start := min(rng.start, extrema)
		end := min(rng.end, extrema-1)
		return interval{start, end}
	case GREATER:
		start := max(rng.start, extrema+1)
		end := max(rng.end, extrema)
		return interval{start, end}
	case LESS_EQUAL:
		start := min(rng.start, extrema)
		end := min(rng.end, extrema)
		return interval{start, end}
	case GREATER_EQUAL:
		start := max(rng.start, extrema)
		end := max(rng.end, extrema)
		return interval{start, end}
	}
	return rng
}

func product(st state) int {
	x_range := st[0].end - st[0].start + 1
	m_range := st[1].end - st[1].start + 1
	a_range := st[2].end - st[2].start + 1
	s_range := st[3].end - st[3].start + 1
	return x_range * m_range * a_range * s_range
}

func workflow_ranges(current state, flows map[string]workflow, name string) int {
	if name == "A" {
		return product(current)
	}
	if name == "R" {
		return 0
	}
	w_flow := flows[name]
	total := 0
	for _, rl := range w_flow.rules {
		ineq := LESS
		if !rl.less_than {
			ineq = GREATER
		}
		switch rl.category {
		case 'x':
			new_range := update_interval(current[0], rl.value, ineq)
			new_state := current
			new_state[0] = new_range
			total += workflow_ranges(new_state, flows, rl.destination)
			current[0] = update_interval(current[0], rl.value, ineq+2)
		case 'm':
			new_range := update_interval(current[1], rl.value, ineq)
			new_state := current
			new_state[1] = new_range
			total += workflow_ranges(new_state, flows, rl.destination)
			current[1] = update_interval(current[1], rl.value, ineq+2)
		case 'a':
			new_range := update_interval(current[2], rl.value, ineq)
			new_state := current
			new_state[2] = new_range
			total += workflow_ranges(new_state, flows, rl.destination)
			current[2] = update_interval(current[2], rl.value, ineq+2)
		case 's':
			new_range := update_interval(current[3], rl.value, ineq)
			new_state := current
			new_state[3] = new_range
			total += workflow_ranges(new_state, flows, rl.destination)
			current[3] = update_interval(current[3], rl.value, ineq+2)
		}
	}
	return total + workflow_ranges(current, flows, w_flow.final)
}

func rating_parts(scanner *bufio.Scanner) int {
	flows := make(map[string]workflow)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			break
		}
		idx := strings.IndexByte(line, '{')
		name := line[:idx]
		str_flow := line[idx+1 : len(line)-1]
		w_flow := extract_workflow(str_flow)
		flows[name] = w_flow
	}
	var m_parts []machine_part
	for scanner.Scan() {
		line := scanner.Text()
		trimmed := line[1 : len(line)-1]
		part := extract_part(trimmed)
		m_parts = append(m_parts, part)
	}
	total := 0
	for _, part := range m_parts {
		valid := process_part(part, flows, START)
		if valid {
			total += part.sum()
		}
	}
	base := interval{1, 4000}
	start_state := state{base, base, base, base}
	fmt.Println("Possible permutations is", workflow_ranges(start_state, flows, START))
	return total
}
