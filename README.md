# Message-passing Concurrent Language Evaluation

This repository contains a collection of programs that evaluate languages with message-passing concurrency.  The programs are designed to evaluate properties of message-passing concurrency rather than general language performance.  Furthermore, a selection of features from process calculi are examined for each language.  These are:

* Synchronous communication.
* First-order channels.
* Higher-order channels.
* First-order processes.
* Higher-order processes.
* Parallel execution statement.
* Indexed parallel execution statement.
* State ownership.
* Process ownership.
* Selection on incoming messages.
* Indexed selection.
* Selection based on incoming value.
* Guarded selection.
* Fair selection.
* Selection with timer.
* Other selection types.
* Selection of outgoing messages.
* Multi-party synchronisation.
* Selection on multi-party synchronisation.

A description of these features is provided in the appendix below.

## Classification

One of the aims of this work is the building of a classification for message-passing concurrent languages.  The hope is that an understanding of common features and different approaches can be made by investigating the approaches to message-passing concurrency.  At present, the classification is limited as only a handful of languages have been examined.  The current classification is below:

`TODO`

## Current Languages

The languages currently evaluated are:

* [Ada](src/ada/)
* [ChucK](src/chuck/)
* [D](src/d/)
* [Erlang](src/erlang/)
* [Go](src/go/)
* [occam-_&pi;_](src/occam/)
* [Rust](src/rust/)

Languages that have so far been discounted are:

* Aha!
* ALGOL 68
* AmbientTalk
* Ateji PX
* Axum
* BCPL
* C=
* C&omega;
* Clojure
* Concurrent Pascal, although SuperPascal may allow a similar analysis.

Details on why these languages are discounted is given in the [Discounted Languages](#CurrentlyDiscountedLanguages) section.

## Benchmark Results

There are currently three benchmark applications:

1. Communication Time.
2. Selection Time.
3. Monte Carlo _&pi;_ (for multicore support analysis).

The benchmarks are described in the appendix below.  

## Contributing to the Evaluation

If you want to help, feel free to pull the repository, implement the benchmarks, undertake the evaluation, and make a pull request.  At present, the following languages have been identified as potentially having message-passing concurrency support in the language or via the language's standard libraries but have not been completed.

* [Aikido](http://aikido.sourceforge.net/.)
* [C#](https://en.wikipedia.org/wiki/C_Sharp_(programming_language)) - via [`BlockingCollection`](https://docs.microsoft.com/en-us/dotnet/standard/collections/thread-safe/blockingcollection-overview).
* [CAL Actor Language](https://en.wikipedia.org/wiki/CAL_Actor_Language) - tricky to find an implementation.  Try [here](http://orcc.sourceforge.net/).
* [Dodo](http://dodo.sourceforge.net/)
* [E](https://en.wikipedia.org/wiki/E_(programming_language)) - instructions available [here](http://erights.org/).
* [Eiffel](https://en.wikipedia.org/wiki/Eiffel_(programming_language)) - a good starting point is probably [Eiffel Software](https://www.eiffel.com/).  Concurrency seems to be provided via [SCOOP](https://www.eiffel.org/doc/solutions/Concurrent%20programming%20with%20SCOOP) but unclear if message passing is available.
* [Elixir](https://en.wikipedia.org/wiki/Elixir_(programming_language)) (although this is really just Erlang) - instructions available [here](https://elixir-lang.org/).
* [Esterel](https://en.wikipedia.org/wiki/Esterel) - instructions available [here](http://www.esterel.org/).
* [Euphoria](https://en.wikipedia.org/wiki/Euphoria_(programming_language)) - instructions available [here](https://openeuphoria.org/).
* [F#](https://en.wikipedia.org/wiki/F_Sharp_(programming_language))
* [Falcon](https://en.wikipedia.org/wiki/Falcon_(programming_language)) - instructions available [here](http://falconpl.org/).
* [Fantom](https://en.wikipedia.org/wiki/Fantom_(programming_language)) (although this does run on-top of other runtimes) - instructions available [here](http://www.fantom.org/).
* [Fancy](http://www.fancy-lang.org/)
* [FortranM](https://en.wikipedia.org/wiki/FortranM) - seems out-of-date, but try [here](http://www.netlib.org/fortran-m/).
* [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)) - via the [Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent-Chan.html) type.  Unclear if this is part of standard Haskell.
* [Hume](https://en.wikipedia.org/wiki/Hume_(programming_language)) - instructions available [here](http://www.hume-lang.org/).
* [Go!](https://en.wikipedia.org/wiki/Go!_(programming_language)) (not to be confused with Go) - instructions available [here](https://github.com/frankmccabe/go).
* [Goaldi](https://github.com/proebsting/goaldi)
* [Icon](https://en.wikipedia.org/wiki/Icon_(programming_language)) - although not sure a compiler exists anymore.
* [Io](https://en.wikipedia.org/wiki/Io_(programming_language)) - instructions available [here](http://iolanguage.org/).
* [J](https://en.wikipedia.org/wiki/J_(programming_language)) - instructions available [here](http://www.jsoftware.com/).
* [Java](https://en.wikipedia.org/wiki/Java_(programming_language)) - the [`SynchronousQueue`](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/SynchronousQueue.html) is part of the standard Java API and provides a synchronous channel like construct.
* [JoCaml](https://en.wikipedia.org/wiki/JoCaml) - instructions available [here](http://jocaml.inria.fr/).
* [Join Java](https://en.wikipedia.org/wiki/Join_Java) - appears to have been a PhD project.  Try [here](http://joinjava.unisa.edu.au/).
* [Joule](https://en.wikipedia.org/wiki/Joule_(programming_language)) - not sure if still relevant.
* [Julia](https://en.wikipedia.org/wiki/Julia_(programming_language)) - instructions available [here](https://julialang.org/).
* [Kotlin](https://en.wikipedia.org/wiki/Kotlin_(programming_language)) - instructions available [here](https://kotlinlang.org/).
* [LabVIEW](https://en.wikipedia.org/wiki/LabVIEW) - might be stretching the definition though.  Instructions available [here](http://www.ni.com/en-gb/shop/labview.html).
* [Limbo](https://en.wikipedia.org/wiki/Limbo_(programming_language)) - instructions available [here](http://www.vitanuova.com/inferno/limbo.html).
* [Logtalk](https://en.wikipedia.org/wiki/Logtalk) - instructions available [here](https://logtalk.org/).
* [Lua](https://en.wikipedia.org/wiki/Lua_(programming_language)) - instructions available [here](https://www.lua.org/).
* [Manticore](http://manticore.cs.uchicago.edu/).
* [Mercury](https://en.wikipedia.org/wiki/Mercury_(programming_language)) - instructions available [here](http://www.mercurylang.org/).
* [MPD](https://en.wikipedia.org/wiki/MPD_(programming_language)) - instructions available [here](https://www2.cs.arizona.edu/mpd/).
* [Mythryl](https://mythryl.org/index3.html)
* [Neko](https://en.wikipedia.org/wiki/Neko_(programming_language)) - instructions available [here](http://nekovm.org/).
* [Newsqueak](https://en.wikipedia.org/wiki/Newsqueak) - may not be suitable, but an interpreter is available [here](https://github.com/rwos/newsqueak).
* [Nim](https://en.wikipedia.org/wiki/Nim_(programming_language)) - instructions available [here](https://nim-lang.org/).
* [OCaml](https://en.wikipedia.org/wiki/OCaml) - instructions available [here](http://ocaml.org/).
* [Oforth](http://www.oforth.com/)
* [Open Object Rexx](https://en.wikipedia.org/wiki/Object_REXX) - instructions available [here](http://www.oorexx.org/).
* [Orc](https://en.wikipedia.org/wiki/Orc_(programming_language)) - looks interesting.
* [Oz](https://en.wikipedia.org/wiki/Oz_(programming_language)) - the best approach seems to be via [Mozart](http://mozart.github.io/).
* [P](https://github.com/p-org/P) - Microsoft research language.  Seems interesting.
* [Panda](https://panda-lang.org/)
* [Perl](https://en.wikipedia.org/wiki/Perl) - although I think this may be via OS process communication.  Instructions available [here](https://www.perl.org/).
* [Perl 6](https://en.wikipedia.org/wiki/Perl_6) - although see comment on Perl.  Instructions available [here](https://perl6.org/).
* [Phix](http://phix.x10.mx/) - but seems just to be Euphoria.
* [PicoLisp](https://en.wikipedia.org/wiki/PicoLisp) - details available [here](https://picolisp.com/wiki/?home).
* [Pony](https://www.ponylang.org/)
* [Preesm](https://en.wikipedia.org/wiki/Preesm) - may be stretching the definition.  Instructions available [here](http://preesm.sourceforge.net/website/).
* [ProcessJ](https://processj.cs.unlv.edu/)
* [Prolog](https://en.wikipedia.org/wiki/Prolog) - there appears to be send message and receive message support.  See [here](http://www.swi-prolog.org/pldoc/man?section=threads).
* [Python](https://en.wikipedia.org/wiki/Python_(programming_language)) via threads and queues.  Details available [here](https://www.python.org/).
* [Racket](https://en.wikipedia.org/wiki/Racket_(programming_language)) - instructions available [here](https://racket-lang.org/).
* [Red](https://en.wikipedia.org/wiki/Red_(programming_language)) - instructions available [here](http://www.red-lang.org/).
* [Ruby](https://en.wikipedia.org/wiki/Ruby_(programming_language)) - instructions available [here](https://www.ruby-lang.org/en/).  Unsure about this.
* [SALSA](https://en.wikipedia.org/wiki/SALSA_(programming_language)) - instructions available [here](http://wcl.cs.rpi.edu/salsa/).
* [Scala](https://en.wikipedia.org/wiki/Scala_(programming_language)) - instructions available [here](http://www.scala-lang.org/).  It looks like actors are no longer part of the core library though.
* [SequenceL](https://en.wikipedia.org/wiki/SequenceL) - instructions available [here](https://texasmulticore.com/).
* [Sequoia++](https://web.stanford.edu/group/sequoia/)
* [SIGNAL](https://en.wikipedia.org/wiki/SIGNAL_(programming_language)) - may be stretching the definition.  Also tricky to find.  Try [The Polychrony Toolset](http://www.irisa.fr/espresso/Polychrony/).
* [SISAL](https://en.wikipedia.org/wiki/SISAL) - think this is unavailable.  Try [here](https://sourceforge.net/projects/sisal/).
* [SpecC](https://en.wikipedia.org/wiki/SpecC) - a reference compiler can be found via [here](http://www.cecs.uci.edu/~specc/).
* [SR](https://en.wikipedia.org/wiki/SR_(programming_language)) - looks like it is no longer maintained.  Try [here](https://www2.cs.arizona.edu/sr/).
* [Standard ML](https://en.wikipedia.org/wiki/Standard_ML) - but most likely [ConcurrentML](https://en.wikipedia.org/wiki/Concurrent_ML).  Try [here](http://cml.cs.uchicago.edu/).
* [SuperPascal](https://en.wikipedia.org/wiki/SuperPascal) - unlikely to be still possible, but try [here](http://brinch-hansen.net/) and [here](https://github.com/octonion/superpascal).
* [Swift](https://en.wikipedia.org/wiki/Swift_(programming_language)) (via Grand Central Dispatch) - Swift is [available for Linux](https://swift.org/download/).
* [SystemC](https://en.wikipedia.org/wiki/SystemC) - but looks like this is no longer available.  [Official website](http://systemc.org/).
* [SystemVerilog](https://en.wikipedia.org/wiki/SystemVerilog)
* [Tcl](https://en.wikipedia.org/wiki/Tcl) - instructions available [here](http://www.tcl.tk/).
* [Unicon](https://en.wikipedia.org/wiki/Unicon_(programming_language)) - instructions available [here](https://unicon.sourceforge.io/).
* [Unified Parallel C](https://en.wikipedia.org/wiki/Unified_Parallel_C) - may fit the criteria.
* [Vorpal](http://vmlanguages.is-research.de/vorpal/) - seems to have become [Clump](http://d.plaindoux.free.fr/clump/).
* [Wren](http://wren.io/)
* [XC](https://en.wikipedia.org/wiki/XC_(programming_language)) - although unsure if this will work on a general PC.  There is an open source [XCore project](https://github.com/xcore).
* [Zkl](http://www.zenkinetic.com/zkl.html)
* [Zonnon](https://en.wikipedia.org/wiki/Zonnon) - instructions available [here](http://www.zonnon.ethz.ch/).
* [&#x3bc;C++](https://en.wikipedia.org/wiki/%CE%9CC%2B%2B) - instructions available [here](https://plg.uwaterloo.ca/usystem/uC++.html).

Not all of these languages may support message-passing concurrency, as only a quick overview has been made.  Languages may also be missing.  If you know of a language missed please get in touch.  Also, to be included a current implementation of the language is required.  There are historic languages with message-passing concurrency, but seem to be unavailable today.  For comparison purposes, the language must run on Linux.

Language selection criteria:

1. the language __must__ provide mechanisms to send a message between components as part of the core language features (e.g., keyword support and/or standard library) and not via an additional library.
2. the message must be sent in a manner so that the receiver can __choose__ when to receive it; therefore, giving the receiver control over its internal state.  A method invocation on an object is therefore not a message.  This criteria implies a receiver has some thread of execution independant to the sender.
3. the receiver __must__ be able to wait __passively__ for a message to be received - that is, a busy spinning on a value to change is not a message.  Having a queue between threads that a receiver tests for readiness is not suitable.
4. a communication __must__ be made using a single command, i.e., language keyword or standard library call.  A slight concesion is made for *yielding* due to the use of coroutine approaches that often require an explicit `yield` statement, and languages that may define a communication in some form of block statement.  This is to keep in the spirit of a message-pass being a single operation.
5. messages __must__ be any structured data type supported in the language.  Conversion to bytes, strings, or another data serialization technique is __not__ considered message-passing.  Nor is any use of I/O mechanisms to simulate communication.

If you have to download a separate library, or write your own functions, to achieve message-passing then the criteria does not allow the language to be included.  Future work will examine library support, but as numerous examples exist this is currently outside the scope of this work.

### Currently Discounted Languages

For various reasons, some languages originally considered for the study have been discounted.  These languages, and some information for there discounting, are listed below:

* [Aha!](http://www.ahafactor.net/language).  Appears to be unavailable now.
* [ALGOL 68](https://en.wikipedia.org/wiki/ALGOL_68) - but may be stretching the definition.  ALGOL 68 requires explicit use of shared values and semaphores to enable message passing.  This violates criteria 4.
* [AmbientTalk](https://en.wikipedia.org/wiki/AmbientTalk) - instructions available [here](http://soft.vub.ac.be/amop/).  On analysis, AmbientTalk does not meet criteria 2 (the message must be sent in a manner so that the receiver can __choose__ when to receive it; therefore, giving the receiver control over its internal state).  This is because messages are essentially asynchronous method calls on actors, not a communication that the actor can decide when to engage in.
* [Ateji PX](https://en.wikipedia.org/wiki/Ateji_PX) - seems to be unavailable now.
* [Axum](https://en.wikipedia.org/wiki/Axum_(programming_language)) - but looks like this is [closed](https://msdn.microsoft.com/en-us/devlabs/dd795202.aspx).
* [BCPL](https://en.wikipedia.org/wiki/BCPL) - details [here](https://www.cl.cam.ac.uk/~mr10/) - but looks like a complicated mechanism to allow communication.  On examination, although BCPL does have coroutine support, message-passing has to be implemented by the programmer.  As such, BCPL is dicounted.
* [C=](http://www.hoopoesnest.com/cstripes/cstripes-sketch.htm) - although unsure how easy it is to communicate between concurrent components.  Appears to be unavailable now.
* [C&omega;](https://en.wikipedia.org/wiki/C%CF%89) - Microsoft Research [page](https://www.microsoft.com/en-us/research/project/comega/?from=http%3A%2F%2Fresearch.microsoft.com%2Fcomega%2F).  Might not be suitable.  After some investigation, C&omega; became the [Joins Concurrency Library](https://en.wikipedia.org/wiki/Joins_(concurrency_library)) for .NET, although this is not a standard library and therefore does not meet the criteria.  A C&omega; compiler can be downloaded, but it requires .NET 1.1, which is no longer supported.  Thus, C&omega; has been discounted from the list.
* [Clojure](https://en.wikipedia.org/wiki/Clojure) - instructions available [here](https://clojure.org/).  At present this does not seem to meet the criteria.  The `agents` package provides simple agents that respond to function calls but have no control over their state.  The `core.async` package does provide the functionality, but it is not core Clojure and therefore does not meet criteria 1.
* [Concurrent Pascal](https://en.wikipedia.org/wiki/Concurrent_Pascal) - although it might be a stretch saying this is live.  A compiler for microcontrollers is available [here](https://github.com/dhawk/concurrent-pascal-compiler).  After some investigation it appears that Concurrent Pascal is not available.  No version exists that can be executed on a Linux desktop.
* [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk).  Smalltalk's concurrency support is limited to forking processes and a semaphore for synchronisation.  As such, it does not meet the criteria for message passing.

## Language Timeline

This section indicates when languages where released.  The aim is to illustrate any clusters of development.

1972. (2) Prolog
1973. (1) Standard ML (as ML)
1974. (0)
1975. (0)
1976. (0)
1977. (1) Icon
1978. (0)
1979. (0)
1980. (0)
1981. (0)
1982. (1) SIGNAL
1983. (4) Ada, Esterel, occam, SISAL
1984. (0)
1985. (0)
1986. (3) Eiffel, Erlang, LabVIEW
1987. (1) Perl
1988. (3) Object REXX, PicoLisp, Tcl
1989. (1) SequenceL
1990. (3) Haskell, J, Python
1991. (1) Oz
1992. (1) μC++
1993. (4) Concurrent ML, Euphoria, Lua, SuperPascal
1994. (2) Newsquek, Racket
1995. (4) Java, Limbo, Mercury, Ruby
1996. (1) Ocaml
1997. (1) E
1998. (1) Logtalk
1999. (1) SystemC
2000. (4) C#, Hume, Join Java, Unicon
2001. (4) CAL Actor Language, D, SALSA, SpecC
2002. (2) Io, SystemVerilog
2003. (4) Aikido, ChucK, Falcon, Go!
2004. (1) Scala
2005. (3) F#, Neko, XC
2006. (1) Sequoia++
2007. (1) Dodo, Manticore
2008. (2) Nim, PREESM
2009. (3) Clump, Go, ProcessJ
2010. (2) Fancy, Rust
2011. (3) Elixir, Kotlin, Red
2012. (1) Julia
2013. (2) Wren, Zonnon
2014. (4) Goaldi, Oforth, Pony, Swift
2015. (2) Panda, Perl 6
2016. (1) Fantom
2017. (0)
2018. (0)

Unknown - FortranM, JoCaml, MPD, Mythryl, Phix, SR, Zkl

## Additional References

There are a few sites which provide useful information on programming languages:

* [Tiobe Index](http://www.tiobe.com/tiobe-index/) uses Internet search engine results to determine language popularity.
* [GitHub popularity](http://githut.info/) provides information on language popularity in GitHub projects.
* [Rosetta Code](http://rosettacode.org/wiki/Rosetta_Code) provides common examples in various programming languages.
* [List of programming languages on Wikipedia](https://en.wikipedia.org/wiki/List_of_programming_languages) if you want to see how many languages are out there.  Not complete though.

# Appendix

## Properties

The properties taken from process calculi are:

### Synchronous Communication

Generally, process calculi work with synchronous communication - that is two or more processes must agree to communicate and do so as a single atomic operation.  For example, in CSP:

```cspm
channel a, b

P = a -> b -> SKIP
Q = b -> SKIP

SYSTEM = P [{b} || {b}] Q
```

`Q` will always wait until `P` is ready to communicate via `b`, and vice-versa.  Not all languages support synchronous communication.

### First-order Channels

Does the language have a *channel-like* construct?  Although process calculi generally have channels for inter-process communication, not all languages do.  Typically, *actor-style* concurrent languages do not have channels.

### Higher-order Channels

The _&pi;_-Calculus permits channels to be sent as messages, thus reconfiguring the communication network.  For example (adapted from [Wikipedia](https://en.wikipedia.org/wiki/%CE%A0-calculus#A_small_example)):

```pi
(new x)(x<z>.0 | x(y).y<x>.x(y).0) | z(v).v<v>.0
```

`z` is communicated over channel `x` in the first step:

```pi
(new x)(0 | z<x>.x(y).0) | z(v).v<v>.0
```

`x` is communicated over channel `z` in the next step:

```pi
(new x)(0 | x(y).0) | x<x>.0
```

In the final step, `x` is communicated via channel `x`:

```pi
(new x)(0 | 0) | 0
```

If the language's channels support sending any type, this will typically include channels.

### First-order Processes

Assigning an *active* process to a variable is not a process calculus feature except in the *higher-order &pi;-Calculus*.  First-order processes are included for two reasons:

1. Languages without channels will use a process variable to send messages to.
2. To discuss higher-order processes below.

If a process launch can be assigned to a variable, then the language supports first-order processes.  *Actor-style* concurrency requires this feature, although other languages do support it.

### Higher-order Processes

The *higher-order &pi;-Calculus* supports sending of process variables across a channel.  For example:

```pi
x<R>.P | x(Y).Q
```

It has been shown that in the _&pi;_-Calculus process mobility can be simulated via channel mobility.  However, the ability to send a process name is used extensively in *actor-style* concurrency to build communication networks.  Typically, if a language supports first-order processes it supports higher-order processes.

### Parallel Execution Statement

In process calculi, an operator defining parallel execution is normally provided.  In CSP:

```cspm
P |[{a}]| Q

P ||| Q
```

In _&pi;_-Calculus:

```pi
P | Q
```

Few languages directly support a parallel execution statement.  occam is one such example:

```occam
PAR
    P()
    Q()
    R()
```

### Indexed Parallel Execution Statement

CSP supports a parallel execution statement:

```cspm
Q = || i : {0..10} @ [A(x)] P(x)
```

Where `A` is the alphabet of the replicated process `P`.  CSP works on *alphabetised* replication, so a set of any type can be used.

Typically, such a statement is provided in a language as a `parallel for` or equivalent.

### State Ownership

Does the language ensure no data is shared between processes?  This is key concept in concurrency safety.  Generally, languages supporting reference passing and pointers do not promote state ownership.

### Process Ownership

When a process is created it exists and is owned within the creating context.  This can be tested by spawning a child process and checking if the owning process waits for child processes to complete when it ends.  In process calculi this is implicit the parallel operator must be completed before a process can continue to the next operation.

### Selection on Incoming Messages

A key feature of process calculi is the ability to emit behaviour based on a choice.  Process calculi may provide different mechanisms for choice.  For example, CSP specifies both internal and external choice.

An example of choice in CSP is:

```cspm
channel a, b, c

P = (a -> P) [] (b -> SKIP)
Q = a -> SKIP
R = b -> SKIP
```

When run in parallel, `P` may communicate on `a` first or `b` first.  Depending on that choice the system can deadlock.

In the properties selection between incoming and outgoing messages has been seperated.  This is because many languages will support input selection, but few support output selection.  This property is met by being able to select from a range of incoming message sources.  To do so will normally require first-order channels.

### Indexed Selection

The ability to select from an array or similar range of incoming message sources via an index.  In CSP this is called a replicated choice (either internal or external).  For example:

```cspm
channel a, b, c

P = a -> SKIP
Q = b -> SKIP
R = c -> SKIP
S = [] x : {a, b, c} @ x -> S
```

`S` will select either `a`, `b`, or `c` and complete the communication with the relevant process.  It will do so three times until it deadlocks.

This feature appears to be uncommon in languages.  It could be seen as an equivalant to a `select for` statement.  occam provides an `ALT FOR` statement for this purpose.

### Selection Based on Incoming Value

The ability to select an incoming message based on the value of the message.  In process calculi this is quite common:

```cspm
channel a : Bool

P = (a.true -> P) [] (a.false -> SKIP)
```

In languages this is less common as it normally requires a read to be commited before the value is checked.  *Actor-style* concurrency does provide a partial implementation based on the message type.

### Guarded Selection

The ability to activate a selection choice based on a boolean condition.  In CSP:

```cspm
channel a, b

P(x) = (x < 5 & a -> P(x + 1)) [] (x >= 5 & b -> SKIP)
Q = b -> SKIP

SYSTEM = P(0) [{a, b} || {b}] Q
```

While `x` is less than 5, `P` will increment `x`.  Once `x` equals 5, `P` and `Q` will sync and terminate.

Some languages appear to support this feature, and it there is no consistent pattern seen yet across language types.

### Fair Selection

Whether the selection can be considered fair as far as possible.  Fair means that the probability of being selected is the same as other events in a selection operation over time.  This can be achieved through random selection of ready guards, or deprioritising previously selected guards.

Fair selection is not a process calculi property.  As long as a choice is made when possible that is enough.  The probability of selection does not feature in the model.  However, in practice languages attempt to support fair selection to allow easier reasoning.

### Selection with Timer

Whether some form of timing can be used during a selection.  Process calculi incorporating time do exist (e.g. Timed CSP).  Because of this, and the prevalant use of timers in selection, timed selection has been added as a property to test for.

### Other Selection Types

This criteria could include anything, but typically we want a default option that allows continuation if nothing is ready to select.  In CSP, this can be modelled with SKIP:

```cspm
channel a, b

P = (a -> P) [] (b -> SKIP) [] SKIP
```

Effectively we want to have a non-blocking selection option, which is a quite common design pattern.

### Selection of Outgoing Messages

As input selection, but the ability to select based on output messages.  At present, only Go seems to support this feature.  In process calculi there is no differentiation between input and output selction.

### Multi-party Synchronisation

The ability for more than two processes to syncrhonise at once.  Typically supported by a barrier, but any object that allows a group of processes to agree to synchronise.  In CSP all parties must agree to synchronise on an event for it to become ready:

```cspm
channel a, b, c, d

P = a -> d -> P
Q = d -> b -> Q
R = c -> d -> R
```

`P`, `Q`, and `R` must agree to communicate on `d` if they are suitably composed in parallel.

### Selection on Multi-party Synchronisation

The ability to select on a multi-party synchronisation point.  CSP does not differentiate between event types, so a multi-party synchronisation point can be in a choice operation.  This has yet to be found in a programming language, although libraries such as JCSP do support it.

## Benchmarks

At present, three benchmarks have been used.

### Communication Time

This benchmark measures the communication / coordination time of a message.  Four processes are involved:

* __Prefix__ outputs 0, then outputs what it inputs.
* __Delta__ outputs its input on two channels sequentially.
* __Successor__ increments its input before output.
* __Consumer__ times how long it takes to receive _N_ inputs.

The processes are connected together to produce the natural numbers.  A rough CSP equivalent is:

```cspm
channel a, b, c, d : Int

ID = c?x -> a!x -> ID

PREFIX(n) = a!n -> ID

DELTA = a?x -> b!x -> d!x -> DELTA

SUCC = b?x -> c!(x + 1) -> SUCC

CONSUMER = d?x -> CONSUMER

COMMSTIME = (((PREFIX(0) [{a,c} || {a,b,d}] DELTA) [{a,b,c,d} || {b,c}] SUCC) [{a,b,c,d} || {d}] CONSUMER)
```

The Communicaton Time benchmark (also referred to as commstime) was originally developed in occam by Peter Welch at the University of Kent.

### Select Time

This benchmark measures how long it takes to select a message from a _N_ inputs.  A single reading process services multiple inputs from _N_ writers.  The reader times how long it takes to perform a single selection for different sizes of _N_.

Some languages (typically actor-based ones with mailbox systems) do not fair well when _N_ is greater than the number of threads as the reader cannot service the writers fast enough and the mailbox expands.

### Monte Carlo &pi;

A simple data-parallel benchmark designed to test if the language has multi-core support.  Speed-up measures are derived based on 2, 4, and 8 processes being run.  It is not the time taken to perform the benchmark but the speedup that is important.