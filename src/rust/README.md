# Rust

[Wikipedia Entry](https://en.wikipedia.org/wiki/Rust_(programming_language))

[Main Website](https://www.rust-lang.org/en-US/)

The information presented was taken from Rust 1.21 Nightly.  Nightly is required for ```select!```.

## Running the Benchmarks

Information was gathered by building release builds using Cargo.

```cargo build --release```

```cargo run --release```

The benchmarks provided are:
* [Communication Time](commstime/src/commstime.rs)
* [Selection Time](selecttime/src/main.rs)
* [Monte Carlo &pi;](montecarlopi/src/main.rs)

## Language Features

| Feature                                   | Support      |
| ----------------------------------------- | ------------ |
| Synchronous communication                 | yes          |
| First-order channels                      | yes          |
| Higher-order channels                     | yes          |
| First-order processes                     | yes          |
| Higher-order processes                    | yes          |
| Parallel execution statement              | no           |
| Indexed parallel execution statement      | no           |
| State ownership                           | yes          |
| Process ownership                         | no           |
| Selection on incoming messages            | experimental |
| Indexed selection                         | no           |
| Selection based on incoming value         | no           |
| Guarded selection                         | no           |
| Fair selection                            | no           |
| Selection with timer                      | no           |
| Other selection types                     | no           |
| Selection of outgoing messages            | no           |
| Multi-party synchronisation               | yes          |
| Selection on multi-party synchronisation  | no           |

### Synchronous Communication

Rust's default channel type is asynchronous, but a synchronous channel is provided which can have a buffer size set during creation: a buffer size of 0 is a rendezvous channel.

```
// Create "infinitely buffered" channel.
let (tx, rx) = channel();
// Create a synchronous channel of buffer size n
let (tx, rx) = sync_channel(n);
```

### First-order Channels

The channel type in Rust is explicitly assignable to writing end and reading end variables: ```tx``` and ```rx```.  ```tx``` has type ```Sender``` or ```SyncSender```, ```rx``` has type ```Receiver```.  Both of these end types can be typed.

### Higher-order Channels

Rust can also declare channels of types to send channel ends - either ```Sender```, ```SyncSender```, ```Receiver```.

```
// Create two channels.
let (a_tx, a_rx) = channel();
let (b_tx, b_rx) = channel();
// Send one end of b down a.
a_tx.send(b_tx).unwrap();
```

### First-order Processes

Although threads are created in Rust using the ```thread::spawn``` command, a ```thread``` object is not accessible.  Rather, ```thread::spawn``` returns a ```JoinHandle``` object, which allows the parent thread to join (wait) for the created child thread.  It is argued that Rust's thread creation mechanism does allow assignment of a value representative of the created thread, and therefore Rust provides first-class processes.

```
let child = thread::spawn(move || { println!("Hello World!"); });
```

### Higher-order Processes

As Rust can communicate any type via its channel it can send ```JoinHandle``` types.  Therefore, Rust has higher-order process support.

### State Ownership

Ownership of resources is an important factor in Rust, and although references are allowed, only one variable owns a given resource.  References in Rust use borrow semantics, and this is enforced at compile time.

### Selection on Incoming Messages

Selection in Rust is currently in an experimental state, although it used to be in the standard library.  The functionality is provided, but it is considered not necessarily stable.  As such, a nightly build of Rust is required to perform selection.

```
fn do_work(in0 : Receiver<i32>, in1 : Receiver<i32>) {
    select! {
        msg = in0.recv() => // do some work
        msg = in1.recv() => // do some work
    }
}
```

### Multi-party Synchronisation

A ```Barrier``` type is provided by Rust.

```
fn worker(out : Sender<i32>, bar : Barrier) {
    // Do some work
    out.send(result).unwrap();
    bar.wait()
}
```