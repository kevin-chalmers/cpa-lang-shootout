# D

[Wikipedia Entry](https://en.wikipedia.org/wiki/D_(programming_language))

[Main Website](https://dlang.org/)

The information presented was taken from D 2.075.1.

## Running the Benchmarks

The D reference compiler has been used.  To build the benchmarks with optimisation turned on use the following:

```shell
dmd -O <filename>
```

This will build a binary executable that can be launched as normal.

The benchmarks provided are:

* [Communication Time](commstime.d)
* [Selection Time](selecttime.d)
* [Monte Carlo &pi;](montecarlo.d)

## Language Features

| Feature                                   | Support |
| ----------------------------------------- | ------- |
| Synchronous communication                 | no      |
| First-order channels                      | no      |
| Higher-order channels                     | no      |
| First-order processes                     | yes     |
| Higher-order processes                    | yes     |
| Parallel execution statement              | no      |
| Indexed parallel execution statement      | no      |
| State ownership                           | no      |
| Process ownership                         | yes     |
| Selection on incoming messages            | no      |
| Indexed selection                         | no      |
| Selection based on incoming value         | partial |
| Guarded selection                         | no      |
| Fair selection                            | no      |
| Selection with timer                      | yes     |
| Other selection types                     | partial |
| Selection of outgoing messages            | no      |
| Multi-party synchronisation               | yes     |
| Selection on multi-party synchronisation  | no      |

### Synchronous Communication

Although D's concurrency model is actor-based with a mailbox, the mailbox can be configured to block with one message.

```d
setMaxMailboxSize(proc, 1, block);
```

The mailbox will only accept one message before blocking any further senders.  The synchronisation is therefore not full as the sender and receiver never truly synchronise.  The sender will deposit its message and continue before the receiver acts on it.  It is the following sender that will block.  Therefore synchronous communication is not supported.

### First-order Processes

The lack of first-order channels in D is replaced by first-class processes.  When a thread is created, using the `spawn` function, the ID (`Tid`) of the created thread is returned.  This ID allows messages to be sent to the thread.

### Higher-order Processes

D supports the communication of any type to a thread's mailbox, including thread IDs.  Similar to Erlang, sending thread IDs allows communication networks to be set up.

```d
auto A = spawn(...);
auto B = spawn(...);
B.send(A);
```

### Process Ownership

When a thread creates another thread via the `spawn` command, the creating thread must wait for the child thread to complete before it completes.  It is not as explicit to notice when spawning threads, but small experiments allow indicate that this property is true.

### Selection Based on Incoming Value

When a message is received by a D thread, behaviour can be defined based on type:

```d
receive(
    (int i) { ... },
    (float f) { ... },
    (Variant v) { ... }
);
```

The reason for a partial implementation is that only types can be used, not explicit values.  A `receive` must be commited on the type before the value can be tested.

### Selection with Timer

The `receiveTimeout` operation takes a `Duration` object that can act as a timeout:

```d
bool received = receiveDuration(
    dur!"nsecs"(100),
    (int i) { ... },
    (float f) { ... }
);
```

If the duration passes before a message is received, `false` is returned from the `receiveDuration` operation.

### Other Selection Types

A default-style select type is provided by `Variant`:

```d
receive(
    (int i) { ... },
    (Variant v) { ... }
)
```

If there are no values of type `int` in the mailbox, the `Variant` case will be choosen, and the value(s) provided in `v`.  If multiple values are in the mailbox, `v` will be a `Tuple`.

## Compilation Process and Runtime Environment

D code compiles to a binary executable.  D also provides different schedulers, so an application may use kernel-level or user-level threads based.