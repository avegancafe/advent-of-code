package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	fmt.Println("Reading file...")
	dat, err := os.ReadFile("./input.txt")
	check(err)
	fmt.Println("Processing...")
	xs := strings.Split(string(dat), "\n")
	largerThanPrev := 0

	fmt.Println("Counting increases...")
  for i, x := range xs {
		if i < 2 {
			continue
		}

		parsedX, _ := strconv.ParseInt(x, 0, 64)
		parsedOld, _ := strconv.ParseInt(xs[i-1], 0, 64)
		if parsedX > parsedOld {
			largerThanPrev += 1
		}
	}

	fmt.Println("Done!")
	fmt.Printf("Number of times increased: %d", largerThanPrev)
}
