# Erlang

[Wikipedia Entry](https://en.wikipedia.org/wiki/Erlang_(programming_language))

[Main Website](https://www.erlang.org/)

The information presented was taken from Erlang 18.

## Running the Benchmarks

Information was gathered by running the applications within the Erlang VM.  Start Erlang and then:

```shell
c(<name>).
<name>:start().
```

The benchmarks provided are:

* [Communication Time](commstime.erl)
* [Selection Time](selecttime.erl)
* [Monte Carlo &pi;](montecarlopi.erl)

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
| State ownership                           | yes     |
| Process ownership                         | no      |
| Selection on incoming messages            | no      |
| Indexed selection                         | no      |
| Selection based on incoming value         | partial |
| Guarded selection                         | yes     |
| Fair selection                            | no      |
| Selection with timer                      | yes     |
| Other selection types                     | no      |
| Selection of outgoing messages            | no      |
| Multi-party synchronisation               | no      |
| Selection on multi-party synchronisation  | no      |

### First-order Processes

The lack of first-order channels in Erlang is replaced by first-class processes.  When a process is created, using the `spawn` function, the ID of the created process is returned.  This allows messages to be sent to the process.

### Higher-order Processes

Erlang supports the communication of any type to a process's mailbox, including process IDs.  In fact, this is a common mechanism to allow communication networks to be set up.

```erlang
A = spawn(...),
B = spawn(...),
A ! B.
```

### State Ownership

As Erlang is a functional programming language values are not referenced between contexts.  As such, Erlang has state ownership because everything is copied.

### Selection Based on Incoming Value

The lack of selection on messages is replaced by selection on incoming values in Erlang.  The incoming values that are selectable on are limited to user defined atoms, and the selection is more a form of pattern matching than a choice.

```erlang
do_work() ->
  receive
    a -> %% do some work,
    {b, X} -> %% do some work
  end.
```

### Guarded Selection

Erlang does allow guarded behaviour on selected incoming values.

```erlang
guarded() ->
    receive
        N when N > 42 -> do_work_a();
        N when N < 12 -> do_work_b();
        N -> do_work_c()
    end.
```

### Selection with Timer

The ```after``` option in a ```receive``` statement to have a timeout.

```erlang
do_work(Timeout_ms) ->
    receive
        N -> do_more_work()
    after
        Timeout_ms -> do_less_work()
    end.
```

## Compilation Process and Runtime Environment

Erlang code runs within the Beam Virtual Machine.  The code is normally compiled and loaded from within a Read-Evaluated-Print Loop environment, although code can be compiled from the command line.