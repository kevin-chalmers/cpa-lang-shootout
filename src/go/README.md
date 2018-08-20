# Go

[Wikipedia Entry](https://en.wikipedia.org/wiki/Go_(programming_language))

[Main Website](https://golang.org/)

The information presented was taken from Go 1.8.3.

## Running the Benchmarks

Information was gathered by building the closest equivalent to a release build in Go.  The command was:

```go build -ldflags "-s -w" <filename>```

The benchmarks provided are:

* [Communication Time](commstime.go)
* [Selection Time](selecttime.go)
* [Monte Carlo &pi;](montecarlopi.go)

## Language Features

| Feature                                   | Support |
| ----------------------------------------- | ------- |
| Synchronous communication                 | yes     |
| First-order channels                      | yes     |
| Higher-order channels                     | yes     |
| First-order processes                     | no      |
| Higher-order processes                    | no      |
| Parallel execution statement              | no      |
| Indexed parallel execution statement      | no      |
| State ownership                           | no      |
| Process ownership                         | no      |
| Selection on incoming messages            | yes     |
| Indexed selection                         | partial |
| Selection based on incoming value         | no      |
| Guarded selection                         | no      |
| Fair selection                            | yes     |
| Selection with timer                      | yes     |
| Other selection types                     | yes     |
| Selection of outgoing messages            | yes     |
| Multi-party synchronisation               | yes     |
| Selection on multi-party synchronisation  | no      |

### Synchronous Communication

Go provides synchronous buffered channels of size 0 as default.  To create a channel with a buffer requires a size parameter to be passed to the channel creation operation. 

```
// Normal channel creation
var a := make(chan int)
// Buffered creation
var c := make(chan int, n)
```

### First-order Channels

A channel type is provided in Go, and the channel can be typed.  Channels can be assigned to variables.  The ends of the channel (writing and reading) can be captured if required.

### Higher-order Channels

Channels in Go can be typed to communicate other channels.

```
// Create a channel to send channels.
var a := make(chan chan int);
// Create a normal channel and send it via the other channel.
var b := make(chan int);
a <- b;
```

### Selection on Incoming Messages

Selection between available channels is simple in Go.  Selection is achieved using the ```select``` statement, which acts much like a ```switch``` statement in C-like languages.

```
func do_work(in0 <-chan int, in1 <-chan int) {
    select {
        case msg := <-in0 : // do some work
        case msg := <-in1 : // do some work
    }
}
```

### Indexed Selection

Go does allow a form of indexed selection behaviour via a for loop.

```
func do_work(in []chan int) {
    for i := range in {
        select {
            case x := <- in[i] : // do some work
            default:
        }
    }
}
```

The syntax is clunky, but it does provide indexed selection.  The use of ```default``` causes a fall-through which is not ideal.  What we have is a busy select, and a surrounding loop would ensure an option is picked.  Go can also support indexed selection via reflection, but would require some construction of the select object and also have an overhead due to reflection.  The indexed select is therefore partially supported.

### Fair Selection

The ```select``` is fair in that when multiple possible channels are ready the chosen branch is selected randomly.  This is not quite a priority fairness where the last selected branch is given lower priority, but is a fair solution.

### Selection with Timer

A timeout is available via the ```time``` package in Go.  Multiple timeouts are possible in a ```select``` block.

```
func do_work(in chan int, timeout_ms int) {
    select {
        case n := <-in : do_more_work()
        case <-time.After(time.Millisecond * timeout_ms) : do_less_work()
    }
}
```

### Other Selection Types

As Go's ```select``` is similar to a ```switch``` statement in other languages, the ```default``` keyword has been used as another selection type.

```
func do_work(in chan int) {
    select {
        case n := <-in : do_some_work()
        default : do_nothing()
    }
}
```

### Selection of Outgoing Messages

An output operation can be used within a ```select```.

```
func do_work(in <-chan int, out ->chan int, n int) {
    select {
        case n <- in : do_input_work()
        case out <- n : do_output_work()
    }
}
```

### Multi-party Synchronisation

The ```WaitGroup``` provides multi-party synchronisation.

```
func worker(out chan int, wg WaitGroup) {
    // Do some work
    out <- result
    wg.Done()
}
```

## Compilation Process and Runtime Environment

Go code is converted into binary code and can run as a standalone executable.  The Go code can also be run directly from the command line using the ```go``` command.  There is no virtual machine, and everything runs directly on the host machine.