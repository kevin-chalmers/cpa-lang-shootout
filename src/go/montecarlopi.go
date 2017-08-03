package main

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"time"
)

func montecarlopi(iterations int, master chan float64) {
	s := rand.NewSource(time.Now().UnixNano())
	r := rand.New(s)
	in_circle := 0
	for count := 0; count < iterations; count++ {
		x := r.Float64()
		y := r.Float64()
		if x*x+y*y <= 1.0 {
			in_circle++
		}
	}
	master <- (4.0 * float64(in_circle)) / float64(iterations)
}

func experiment(iterations int, threads int) {
	f, err := os.Create(fmt.Sprintf("mcp-go-%d.csv", threads))
	if err != nil {
		fmt.Println("error creating file: ", err)
		return
	}
	c := make(chan float64)
	defer f.Close()
	for index := 0; index < 100; index++ {
		start := time.Now()
		for t := 0; t < threads; t++ {
			go montecarlopi(iterations/threads, c)
		}
		pi := 0.0
		for t := 0; t < threads; t++ {
			pi = pi + <-c
		}
		pi = pi / float64(threads)
		total := time.Now().Sub(start).Nanoseconds()
		fmt.Println(index, " ", pi, " ", total)
		_, err := f.WriteString(fmt.Sprintf("%d\n", total))
		if err != nil {
			fmt.Println("error writing string: ", err)
		}
	}
}

func main() {
	experiment(int(math.Pow(2, 24)), 1)
	experiment(int(math.Pow(2, 24)), 2)
	experiment(int(math.Pow(2, 24)), 4)
	experiment(int(math.Pow(2, 24)), 8)
	experiment(int(math.Pow(2, 24)), 16)
}
