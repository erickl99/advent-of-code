package main

import (
	"bufio"
	"fmt"
)

func min_heat_loss(scanner *bufio.Scanner) int {

	for scanner.Scan() {
		line := scanner.Text()
		fmt.Println(line)
	}
	return 0
}
