# occam-&pi;

[Wikipedia Entry](https://en.wikipedia.org/wiki/Occam-%CF%80)

[Main Website](http://pop-users.org/occam-pi/)

The information presented was taken from the Kent Retargetable occam Compiler (KRoC) 1.6.

## Running the Benchmarks

Information was gathered by building using KRoC 1.6 using ```occbuild```

```occbuild --program <filename>```

The benchmarks provided are:
* [Communication Time](commstime.occ)
* [Selection Time](selecttime.occ)
* [Monte Carlo &pi;](mcp.occ)
* [Better Monte Carlo &pi;](phw-monte-carlo.occ) - thanks to Peter Welch.

The second Monte Carlo &pi; benchmark enforces the multi-core scheduler to work, giving actual speedup results.

## Language Features

| Feature                                   | Support |
| ----------------------------------------- | ------- |
| Synchronous communication                 | yes     |
| First-order channels                      | yes     |
| Higher-order channels                     | yes     |
| First-order processes                     | partial |
| Higher-order processes                    | partial |
| Parallel execution statement              | yes     |
| Indexed parallel execution statement      | yes     |
| State ownership                           | yes     |
| Process ownership                         | yes     |
| Selection on incoming messages            | yes     |
| Indexed selection                         | yes     |
| Selection based on incoming value         | no      |
| Guarded selection                         | yes     |
| Fair selection                            | yes     |
| Selection with timer                      | yes     |
| Other selection types                     | yes     |
| Selection of outgoing messages            | no      |
| Multi-party synchronisation               | yes     |
| Selection on multi-party synchronisation  | no      |

### Synchronous Communication

occam-&pi; operates on a purely synchronous channel model.  No buffering is possible without a process in-between.

### First-order Channels

occam-&pi; has the ability to create typed channels, and the ability to capture the writing and reading ends of the channel.  All of these channels can be assignable to a variable.

### Higher-order Channels

Mobile records enable communication of channel end types in occam-&pi;.

```
CHAN TYPE INT.IO
  MOBILE RECORD
    CHAN INT in?:
:

PROC node(CHAN INT.IO! sent.int)
  INT.IO! int.c:
  INT.IO? int.s:
  SEQ
    int.c, int.s := MOBILE INT.IO
    send.int ! int.c
    ...
:
```

### First-order Processes

Although occam-&pi; has mobile process types as an extension feature, which allows assignment of processes to variables, this feature does not support standard process types.  Mobile processes have some restrictions on their definition, and, as such, occam-&pi; only provides partial support for first-order processes.  More discussion is provided in higher-order processes.

### Higher-order Processes

As occam-&pi; does have a ```MOBILE PROC``` type it can be argued that higher-order processes are supported.  However, these processes are really continuations.

```
PROC TYPE mobile.proc(CHAN INT in?):

MOBILE PROC my.proc(CHAN INT in?)
  IMPLEMENTS mobile.proc
  SEQ
    -- Do some work
    SUSPEND
    -- Process is checkpointed here.
    -- Restarting the process allows continuation
    
    -- Do some other work
:
```

A higher-order process in occam-&pi; can be sent via a channel when it is suspended (with the ```SUSPEND```) keyword.  When re-invoked, the process will continue at the suspension point.  Process parameters are limited to channels or barriers, limiting the type of mobile processes that can be designed.

### Parallel Execution Statement

A parallel execution statement is provided.

```
PROC system()
  PAR
    proc.1()
    proc.2()
    ...
:
```

### Indexed Parallel Execution Statement

occam-&pi; supports indexed parallel execution via the replicated ```PAR``` construct.

```
PAR i = 0 FOR N
  my_func(i)
```

### State Ownership

Originally occam only supported copy semantics.  The introduction of occam-&pi; allowed data to be _moved_ if it is declared as ```MOBILE```.  The compiler checks that data is not referenced to avoid shared ownership and the possible data race that can occur.

### Process Ownership

occam-&pi; does not allow processes to be assigned to a variable, but any processes created in a ```PAR``` block is _"owned"_ by that block insofar as the ```PAR``` does not complete until all child processes have.  However, occam-&pi; does allow processes to be spawned without a surrounding ```PAR``` block using the ```FORK``` keyword.  An ownership statement can be made using a ```FORKING``` statement, so process ownership is maintained within occam-&pi;.

### Selection on Incoming Messages

```ALT``` allows selection from a set of incoming channels.

```
PROC do.work(CHAN INT in.0?, in.1)
  INT x:
  ALT
    in.0 ? x
      -- do some work
    in.1 ? x
      -- do some work
:
```

### Indexed Selection

The ```ALT``` process can have ```FOR``` applied to it as ```SEQ``` and ```PAR``` can.

```
PROC do.work([N]CHAN INT in?) 
    INT x:
    ALT i = 0 FOR N
        in[i] ? x
            -- do work
:
```

### Guarded Selection

Conditions can be placed directly into an ```ALT``` case. 

```
PROC guarded(INT n, CHAN INT in?)
  INT x:
  ALT
    n > 42 && in ? x
      do_work_a()
    n < 12 && in ? x
      do_work_b()
    in ? x
      do_work_c()
:
```

occam-&pi;'s conditions are separate to the channels.  The logical condition can be tested when the ```ALT``` is executed, thus leading certain branches to be inactive.

### Fair Selection

By default an ```ALT``` in occam-&pi; is fair.  occam-&pi; also provides an unfair ```PRI ALT``` where branches are evaluated in order.

```
PROC do.work(CHAN INT in.0?, in.1)
  INT x:
  PRI ALT
    in.0 ? x
    -- do some work
    in.1 ? x
    -- do some work
:
```

### Selection with Timer

The ```TIMER``` enables timeouts in an ```ALT```.  Multiple ```TIMER```s can be created, but the granularity of the timer is only milliseconds.

```
PROC do.work(CHAN INT in?, INT timeout_ms)
  INT n, t:
  TIMER tim:
  SEQ
    tim ? t
    t := t + timeout_ms
    ALT
      in ? n
        do.more.work()
      tim ? AFTER t
        do.less.work()
:
```

### Other Selection Types

The ```SKIP``` (empty process) guard is used for a default branch in an ```ALT```.

```
PROC do.work(CHAN INT in?)
  INT n:
  ALT
    in ? n
      do.some.work()
    SKIP
      do.nothing()
:
```

### Multi-party Synchronisation

A ```BARRIER``` is also provided by occam-$\pi$.

```
PROC worker(CHAN INT out!, BARRIER bar)
  INT result:
  SEQ
    -- Do some work
    out ! result
    SYNC bar
:
```

## Compilation Process and Runtime Environment

occam-&pi; code can be compiled to a number of different targets.  The Transtepreter will execute code in a virtual machine, whereas building for the local system will produce an executable that is built using the Kent CCSP library and runtime.  It therefore executes on the local machine.