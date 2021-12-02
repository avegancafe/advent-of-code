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
		if i < 3 {
			continue
		}

		parsedX, _ := strconv.ParseInt(x, 0, 64)
    parsedY, _ := strconv.ParseInt(xs[i - 1], 0, 64)
    parsedZ, _ := strconv.ParseInt(xs[i - 2], 0, 64)
    parsedA, _ := strconv.ParseInt(xs[i - 3], 0, 64)

    sumA := parsedX + parsedY + parsedZ
    sumB := parsedY + parsedZ + parsedA
		if sumA > sumB {
			largerThanPrev += 1
		}
	}

	fmt.Println("Done!")
	fmt.Printf("Number of times increased: %d", largerThanPrev)
}
