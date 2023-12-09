package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func extract_count(str string) int {
	count := 0
	if str[1] == ' ' {
		count, _ = strconv.Atoi(str[0:1])
	} else {
		count, _ = strconv.Atoi(str[0:2])
	}
	return count
}

func validate(str string, count int) bool {
	return strings.Contains(str, "g") && count <= 13 || strings.Contains(str, "red") && count <= 12 || strings.Contains(str, "b") && count <= 14
}

func valid_games(scanner *bufio.Scanner) int {
	sum := 0
	re, err := regexp.Compile("\\d+ (red|green|blue)")
	if err != nil {
		fmt.Println("Failed to compile regex!")
		os.Exit(1)
	}
	game_num := 1
	for scanner.Scan() {
		line := scanner.Text()
		matches := re.FindAllString(line, -1)
		valid := true
		for _, match := range matches {
			num := extract_count(match)
			if !validate(match, num) {
				valid = false
				break
			}
		}
		if valid {
			sum += game_num
			fmt.Println(game_num)
		}
		game_num++
	}
	return sum
}

func power_sum(scanner *bufio.Scanner) int{
	power_sum := 0
	re, err := regexp.Compile("(\\d+ (red|green|blue), )*\\d+ (red|green|blue);?")
	if err != nil {
		fmt.Println("Failed to compile regex!")
		os.Exit(1)
	}
	for scanner.Scan() {
		line := scanner.Text()
		matches := re.FindAllString(line, -1)
		red := -1
		blue := -1
		green := -1
		for _, match := range matches {
			tokens := strings.Split(match, ", ")
			for _, tkn := range tokens {
				count := extract_count(tkn)
				if strings.Contains(tkn, "g") {
					green = max(green, count)
				} else if strings.Contains(tkn, "r") {
					red = max(red, count)
				} else {
					blue = max(blue, count)
				}
			}
		}
		power_sum += red * green * blue
	}
	return power_sum
}
