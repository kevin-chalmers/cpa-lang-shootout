package main

import (
  "fmt"
)

func mid(input chan int, output chan int) {
  var x int;
  for {
    select {
    case x = <- input :
      fmt.Println("Input")
    case output <- x:
      fmt.Println("Output")
    }
  }
}

func left(output chan int) {
  var counter int = 0
  for {
    output <- counter
    counter++
  }
}

func right(input chan int) {
  for {
    x := <- input
    fmt.Println(x)
  }
}

func main() {
  var l chan int = make(chan int)
  var r chan int = make(chan int)
  go left(l)
  go mid(l, r)
  right(r)
}
