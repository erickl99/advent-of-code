package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
)

func perform_request(day int) []byte {
	url := fmt.Sprintf("https://adventofcode.com/2023/day/%d/input", day)
	req, err := http.NewRequest(http.MethodGet, url, nil)
	if err != nil {
		log.Fatal(err)
	}

	cookie := os.Getenv("COOKIE")
	if cookie == "" {
		log.Fatal("Please provide a cookie!")
	}
	req.Header.Add("Cookie", cookie)

	client := http.Client{}
	rep, err := client.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer rep.Body.Close()

	body, err := io.ReadAll(rep.Body)
	if err != nil {
		log.Fatal(err)
	}
	return body
}

func get_input(day int) string {
	path := fmt.Sprintf("input-%d", day)
	bytes, err := os.ReadFile(path)
	if err != nil {
		log.Println("Performing request...")
		bytes = perform_request(day)
		os.WriteFile(path, bytes, 0644)
	}
	return string(bytes)
}

func main() {
	args := os.Args
	if len(args) < 2 {
		fmt.Println("Please provide a day.")
		return
	}
	day, err := strconv.Atoi(args[1])
	if err != nil || day < 1 || day > 25 {
		fmt.Println("Please provide a valid day!")
		os.Exit(1)
	}
	input := get_input(day)
	scanner := bufio.NewScanner(strings.NewReader(input))
	fmt.Println(pipe_steps(scanner))
}
