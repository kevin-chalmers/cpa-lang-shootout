// Author: Kevin Chalmers
// Date: 17/08/2017

package main

import "fmt"
import "time"
import "os"

// id sends on out whatever is received on in.
func id(in chan int, out chan int, count int) {
	for index := 0; index < count; index++ {
		x := <-in
		out <- x
	}
}

// prefix outputs N, then behaves as id.
func prefix(N int, in chan int, out chan int, count int) {
	out <- N
	id(in, out, count)
}

// printer reads in 10000 values and calculates the time taken.  Results are
// stored in ct-go.csv.
func printer(in chan int, count int) {
	var results [100]int64
	start := time.Now()
	for index := 0; index < count; index++ {
		if index%10000 == 0 {
			total := time.Now().Sub(start)
			results[index/10000] = total.Nanoseconds() / 10000
			fmt.Println(results[index/10000])
			start = time.Now()
		}
		<-in
	}
	f, err := os.Create("ct-go.csv")
	if err != nil {
		fmt.Println("error creating file: ", err)
		return
	}
	defer f.Close()
	for index := 0; index < 100; index++ {
		_, err = f.WriteString(fmt.Sprintf("%d\n", results[index]))
		if err != nil {
			fmt.Println("error writing string: ", err)
		}
	}
}

// succ reads a value from in, increments it, and outputs on out.
func succ(in chan int, out chan int, count int) {
	for index := 0; index < count; index++ {
		x := <-in
		out <- x + 1
	}
}

// delta reads from in, then outputs on out0 then out1.
func delta(in chan int, out0 chan int, out1 chan int, count int) {
	for index := 0; index < count; index++ {
		x := <-in
		out0 <- x
		out1 <- x
	}
}

func main() {
	count := 10000 * 100
	a := make(chan int)
	b := make(chan int)
	c := make(chan int)
	d := make(chan int)
	go prefix(0, a, b, count)
	go delta(b, c, d, count)
	go succ(c, a, count)
	printer(d, count)
}
