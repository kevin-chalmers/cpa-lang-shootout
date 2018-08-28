# Ada

[Wikipedia Entry](https://en.wikipedia.org/wiki/ChucK)

[Main Website](http://chuck.cs.princeton.edu/)

ChucK is an audio production language which happens to support message passing concurrency in a limited manner.  Message passing is handled via an `Event` which can be overloaded to include parameters and hence becomes a message.  ChucK is limited insofar that a process network containing a loop is not possible, and that there is no selection operator.  Parallelism is possible by running multiple files, but it is complicated to communicate between them.

## Running the Benchmarks

Communication and selection benchmarks are not possible in ChucK.  The parallel performance benchmark is, but a single ChucK VM is not multi-threaded.

## Language Features

| Feature                                   | Support   |
| ----------------------------------------- | --------- |
| Synchronous communication                 | partial   |
| First-order channels                      | yes       |
| Higher-order channels                     | yes       |
| First-order processes                     | yes       |
| Higher-order processes                    | yes       |
| Parallel execution statement              | no        |
| Indexed parallel execution statement      | no        |
| State ownership                           | no        |
| Process ownership                         | yes       |
| Selection on incoming messages            | no        |
| Indexed selection                         | no        |
| Selection based on incoming value         | no        |
| Guarded selection                         | no        |
| Fair selection                            | no        |
| Selection with timer                      | no        |
| Other selection types                     | no        |
| Selection on outgoing messages            | no        |
| Multi-party synchronisation               | no        |
| Selection on multi-party synchronisation  | no        |

### Synchronous Communication

ChucK communication is performed via an `Event`.  The communication is synchronous but can require the programmer to explicitly `yield` to ensure a message is processed by the receiving `spork`.

```chuck
class Int_Chan extends Event
{
    int value;
}

Int_Chan a;
// Send message
5 => a.value;
a.signal();
// Explicitly yield to allow receiver to act on message
me.yield();
```

### First-order Channels

## Compilation Process and Runtime Environment

ChucK programs are executed within a virtual machine via the command `chuck`.  For example:

```shell
chuck test.ck
```