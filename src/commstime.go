package main

import (
  "fmt"
  "time"
  "os"
)

const TOTAL = 1000000

func id(in chan int, out chan int) {
  for i := 0; i < TOTAL; i++ {
    x := <- in
    out <- x
  }
}

func prefix(prefix int, in chan int, out chan int) {
  out <- prefix
  id(in, out)
}

func delta(in chan int, out0 chan int, out1 chan int) {
  for i := 0; i < TOTAL; i++ {
    x := <- in
    out0 <- x
    out1 <- x
  }
}

func succ(in chan int, out chan int) {
  for i := 0; i < TOTAL; i++ {
    x := <- in
    x++
    out <- x
  }
}

func consume(in chan int) {
  var results [TOTAL / 10000]time.Duration
  for j := 0; j < TOTAL; j += 10000 {
    start := time.Now()
    for i := 0; i < 10000; i++ {
      <- in
    }
    total := time.Since(start)
    fmt.Println(total / 10000)
    results[j / 10000] = total / 10000
  }
  f, _ := os.Create("ct-go.csv")
  for i := 0; i < TOTAL / 10000; i++ {
    fmt.Fprintf(f, "%d,", results[i].Nanoseconds())
  }
}

func main() {
  var a chan int = make(chan int)
  var b chan int = make(chan int)
  var c chan int = make(chan int)
  var d chan int = make(chan int)
  go prefix(0, a, b)
  go delta(b, c, d)
  go succ(c, a)
  consume(d)
}
