package main

import (
  "fmt"
)

const N = 5

func phil(name int, sits chan bool, getsup chan bool, left_up chan bool, left_down chan bool, right_up chan bool, right_down chan bool) {
  fmt.Println(p, " thinking")
  sits <- true
  fmt.Println(p, " sitting")
  left_up <- true
  right_up <- true
  fmt.Println(p, " eating")
  left_down <- true
  right_down <- true
  getsup <- true
  phil(name, sits, getsup, left_up, left_down, right_up, right_down)
}

func fork(left_up chan bool, left_down chan bool, right_up chan bool, right_down chan bool) {
  select {
  case <- left_up:
    <- left_down
    fork(left_up, left_down, right_up, right_down)
  case <- right_up:
    <- right_down
    fork(left_up, left_down, right_up, right_down)
  }
}

func butler(j int, sits chan bool, getsup chan bool) {
  if j < N {
    select {
    case <- sits:
      butler(j + 1, sits, getsup)
    case <- getsup:
      butler(j - 1, sits, getsup)
    }
  }
  else {
    <- getsup
    butler(j - 1, sits, getsup)
  }
}

func main() {
  var picks := make([]chan bool, N)
  var putsdown := make([]chan bool, N)
  var sits := make(chan bool)
  var getsup := make(chan bool)
  for i := 0; i < N; i++ {
    go phil(i, sits, getsup, picks[i], putsdown[i], picks[(i + 1) % N], putsdown[(i + 1) % N])
  }
  for i := 0; i < N; i++ {
    go fork()
  }
}
