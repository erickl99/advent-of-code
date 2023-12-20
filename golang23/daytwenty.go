package main

import (
	"bufio"
	"fmt"
	"strings"
)

type signal = bool

type module interface {
	create_pulse() signal
	get_drains() []string
}

type signal_address struct {
	sig  signal
	src  string
	dest string
}

type flip_flop struct {
	drains []string
	active bool
}

func (f *flip_flop) create_pulse() signal {
	f.active = !f.active
	if f.active {
		return HIGH
	} else {
		return LOW
	}
}

func (f flip_flop) get_drains() []string {
	return f.drains
}

func (f flip_flop) String() string {
	return fmt.Sprintf("FlipFlop{Drains: %v, Active: %t}\n", f.drains, f.active)
}

type conjunction struct {
	drains  []string
	sources []string
	memory  []signal
}

func (c *conjunction) create_pulse() signal {
	pulse := HIGH
	for _, src := range c.memory {
		pulse = pulse && src
	}
	return !pulse
}

func (c conjunction) get_drains() []string {
	return c.drains
}

func (c conjunction) String() string {
	return fmt.Sprintf("Conjuction{Drains: %v, Sources: %v, Memory: %v}\n", c.drains, c.sources, c.memory)
}

const BROADCASTER = "broadcaster"
const LOW signal = false
const HIGH signal = true

func press_button(queue []signal_address, module_map map[string]module) (int, int, bool) {
	low := 1
	high := 0
	activated := false
	for len(queue) > 0 {
		item := queue[0]
		dest := item.dest
		src := item.src
		sig := item.sig
		queue = queue[1:]
		mod := module_map[dest]
		if sig == LOW {
			low++
		} else {
			high++
		}
		if dest == "vt" && sig == LOW {
			activated = true
		}
		switch flip_conj := mod.(type) {
		case *flip_flop:
			if sig == LOW {
				sig = flip_conj.create_pulse()
				for _, mod_name := range flip_conj.drains {
					new_address := signal_address{sig, dest, mod_name}
					queue = append(queue, new_address)
				}
			}
		case *conjunction:
			idx := 0
			for i, s := range flip_conj.sources {
				if src == s {
					idx = i
					break
				}
			}
			flip_conj.memory[idx] = sig
			sig = flip_conj.create_pulse()
			for _, mod_name := range flip_conj.drains {
				new_address := signal_address{sig, dest, mod_name}
				queue = append(queue, new_address)
			}
		}
	}
	return low, high, activated
}

func total_pulses(scanner *bufio.Scanner) int {
	module_map := make(map[string]module)
	var queue []signal_address
	conj_set := make(map[string]bool)
	for scanner.Scan() {
		line := scanner.Text()
		tokens := strings.Split(line, " -> ")
		mod := tokens[0]
		name := mod[1:]
		drains := strings.Split(tokens[1], ", ")
		switch mod[0] {
		case '%':
			fl_fl := new(flip_flop)
			fl_fl.active = false
			fl_fl.drains = drains
			module_map[name] = fl_fl
		case '&':
			conj := new(conjunction)
			conj.drains = drains
			conj.sources = []string{}
			conj.memory = []bool{}
			module_map[name] = conj
			conj_set[name] = true
		default:
			init_queue := make([]signal_address, len(drains))
			for idx, d := range drains {
				init_queue[idx] = signal_address{LOW, BROADCASTER, d}
			}
			queue = init_queue
		}
	}
	for name, mod := range module_map {
		for _, dest := range mod.get_drains() {
			if conj_set[dest] {
				conj_mod := module_map[dest].(*conjunction)
				conj_mod.sources = append(conj_mod.sources, name)
				conj_mod.memory = append(conj_mod.memory, false)
			}
		}
	}
	low := 0
	high := 0
	iterations := 0
	rx_active := false
	for !rx_active && iterations < 10000000 {
		low_produced, high_produced, status := press_button(queue, module_map)
		low += low_produced
		high += high_produced
		rx_active = status
		iterations++
	}
	fmt.Println(iterations)
	return low * high
}
