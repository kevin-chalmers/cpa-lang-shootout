package main

import (
  "fmt"
  "time"
  "os"
  "strconv"
  "math"
)

var WRITES int

type stressedpacket struct {
  writer int
  n int
}

func stressedwriter(out chan stressedpacket, writer int) {
  for n := 0; n < WRITES; n++ {
    out <- stressedpacket{writer : writer, n : n}
  }
}

func stressedreader(in []chan stressedpacket, WRITERS_PER_CHANNEL int) {
  var counter int
  counter = int(math.Pow(2, 16))
  start := time.Now()
  total := len(in) * WRITERS_PER_CHANNEL * WRITES
  results := make([]int64, total / int(math.Pow(2, 16)))
  for j := 0; j < total; j++ {
    if counter == 0 {
      total := time.Since(start)
      results[j / int(math.Pow(2, 16))] = total.Nanoseconds() / int64(math.Pow(2, 16))
      start = time.Now()
      counter = int(math.Pow(2, 16))
    }
    for i := range in {
      select {
      case <- in[i]:
      default:
      }
    }
    counter--
  }
  filename := "sr-go-" + strconv.Itoa(len(in)) + "-" + strconv.Itoa(WRITERS_PER_CHANNEL) + ".csv"
  f, _ := os.Create(filename)
  for i := range results {
    fmt.Fprintf(f, "%d,", results[i])
  }
  f.Sync()
}

func main() {
  for c := 2; c <= 1024; c *= 2 {
    for w := 1; w <= 1024; w *= 2 {
      WRITES = (int(math.Pow(2, 26)) / c) / w
      fmt.Println(c, w)
      writer := 0
      var chans = make([]chan stressedpacket, c)
      for i := 0; i < c; i++ {
        chans[i] = make(chan stressedpacket)
        for j := 0; j < w; j++ {
          go stressedwriter(chans[i], writer)
          writer++
        }
      }
      stressedreader(chans, w)
    }
  }
}
