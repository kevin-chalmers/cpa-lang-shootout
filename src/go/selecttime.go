// Author: Kevin Chalmers
// Date: 17/08/2017

package main

import "fmt"
import "time"
import "os"
import "math"

// stressedpacket contains the writer id and a packet index.
type stressedpacket struct {
	writer int
	n      int
}

// writer outputs packets constantly on out.
func writer(out chan<- stressedpacket, writer int, writes int) {
	for n := 0; n < writes; n++ {
		out <- stressedpacket{writer: writer, n: n}
	}
}

// save_results saves the select time benchmark results.  Files are
func save_results(N int, results [256]int64) {
	f, err := os.Create(fmt.Sprintf("st-go-%d.csv", N))
	if err != nil {
		fmt.Println("error creating file: ", err)
		return
	}
	defer f.Close()
	for index := 0; index < 256; index++ {
		_, err = f.WriteString(fmt.Sprintf("%d\n", results[index]))
		if err != nil {
			fmt.Println("error writing string: ", err)
		}
	}
}

// do_select performs the select operation.  Hard coded branches based on number
// of inputs.  Not ideal, but works.
func do_select(N int, in []chan stressedpacket) {
	switch N {
	case 1:
		select {
		case <-in[0]:
		}
	case 2:
		select {
		case <-in[0]:
		case <-in[1]:
		}
	case 4:
		select {
		case <-in[0]:
		case <-in[1]:
		case <-in[2]:
		case <-in[3]:
		}
	case 8:
		select {
		case <-in[0]:
		case <-in[1]:
		case <-in[2]:
		case <-in[3]:
		case <-in[4]:
		case <-in[5]:
		case <-in[6]:
		case <-in[7]:
		}
	}
}

// reader selects from its list of incoming channels and times how long it takes
// to perform 65536 selections.
func reader(N int, in []chan stressedpacket, total int) {
	var results [256]int64
	start := time.Now()
	for count := 0; count < total; count++ {
		if count%65536 == 0 {
			total := time.Now().Sub(start)
			results[count/65536] = total.Nanoseconds() / 65536
			fmt.Println(results[count/65536])
			start = time.Now()
		}
		do_select(N, in)
	}
	save_results(len(in), results)
}

// experiment runs a given select time experiment on number of iterations and
// threads.
func experiment(iterations int, threads int) {
	var chans = make([]chan stressedpacket, threads)
	for i := 0; i < threads; i++ {
		chans[i] = make(chan stressedpacket)
		go writer(chans[i], i, iterations/threads)
	}
	reader(threads, chans, iterations)
}

func main() {
	experiment(int(math.Pow(2, 24)), 1)
	experiment(int(math.Pow(2, 24)), 2)
	experiment(int(math.Pow(2, 24)), 4)
	experiment(int(math.Pow(2, 24)), 8)
}
