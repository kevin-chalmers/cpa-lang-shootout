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
| Process ownership                         | partial   |
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

ChucK uses events which act like channels insofar as an `Event` can be extended to include data:

```chuck
class Int_Chan extends Event
{
    int value;
}

Int_Chan a;

// Send a message
5 => a.value();
a.signal();
...
// Receive a message
a => now;
a.value => local;
```

### Higher-order Channels

ChucK events can contain other events.  References have to be used as we are sending objects rather than primitive data.

```chuck
class Int_Chan extends Event
{
    int value;
}

class Chan_Chan extends Event
{
    Int_Chan value;
}

Chan_Chan a;
// Set the value of a
Int_Chan b @=> a.value;
a.signal();
...
// Get the value from a
a => now;
a.value @=> Int_Chan @ c;
```

### First-order Processes

A `spork` operation returns a type `Shred` which can be stored in a variable.

```chuck
spork ~proc() @=> Shred s;
```

### Higher-order Processes

As a `Shred` is a class type it can be contained in an `Event` just as an `Event` can.

### Process Ownership

There is an implicit parent-child relationship between Shreds.  However, a child `Shred` will terminate when its parent `Shred` does.  A parent does not wait for its children to complete.  As the definition of process ownership is that the parent waits for its children to complete this is only a partial meeting of the requirements.

## Compilation Process and Runtime Environment

ChucK programs are executed within a virtual machine via the command `chuck`.  For example:

```shell
chuck test.ck
```