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
* [Erlang](src/erlang/)
* [Go](src/go/)
* [occam-_&pi;_](src/occam/)
* [Rust](src/rust/)

Languages that have so far been discounted are:

* Aha!
* AmbientTalk
* Ateji PX
* Axum
* C=
* C&omega;
* Concurrent Pascal, although SuperPascal may allow a similar analysis.

## Benchmark Results

There are currently three benchmark applications:

1. Communication Time.
2. Selection Time.
3. Monte Carlo _&pi;_ (for multicore support analysis).

The benchmarks are described in the appendix below.  

## Contributing to the Evaluation

If you want to help, feel free to pull the repository, implement the benchmarks, undertake the evaluation, and make a pull request.  At present, the following languages have been identified as potentially having message-passing concurrency support in the language or via the language's standard libraries.

* [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) - the typical install is [GNAT](http://libre.adacore.com/).
* [Aha!](http://www.ahafactor.net/language).  Appears to be unavailable now.
* [AmbientTalk](https://en.wikipedia.org/wiki/AmbientTalk) - instructions available [here](http://soft.vub.ac.be/amop/).  On analysis, AmbientTalk does not meet criteria 2 (the message must be sent in a manner so that the receiver can __choose__ when to receive it; therefore, giving the receiver control over its internal state).  This is because messages are essentially asynchronous method calls on actors, not a communication that the actor can decide when to engage in.
* [Ateji PX](https://en.wikipedia.org/wiki/Ateji_PX) - seems to be unavailable now.
* [Axum](https://en.wikipedia.org/wiki/Axum_(programming_language)) - but looks like this is [closed](https://msdn.microsoft.com/en-us/devlabs/dd795202.aspx).
* [C=](http://www.hoopoesnest.com/cstripes/cstripes-sketch.htm) - although unsure how easy it is to communicate between concurrent components.  Appears to be unavailable now.
* [C&omega;](https://en.wikipedia.org/wiki/C%CF%89) - Microsoft Research [page](https://www.microsoft.com/en-us/research/project/comega/?from=http%3A%2F%2Fresearch.microsoft.com%2Fcomega%2F).  Might not be suitable.  After some investigation, C&omega; became the [Joins Concurrency Library](https://en.wikipedia.org/wiki/Joins_(concurrency_library)) for .NET, although this is not a standard library and therefore does not meet the criteria.  A C&omega; compiler can be downloaded, but it requires .NET 1.1, which is no longer supported.  Thus, C&omega; has been discounted from the list.
* [CAL Actor Language](https://en.wikipedia.org/wiki/CAL_Actor_Language) - tricky to find an implementation.  Try [here](http://orcc.sourceforge.net/).
* [ChucK](https://en.wikipedia.org/wiki/ChucK) - instructions available [here](http://chuck.cs.princeton.edu/).  ChucK looked promising but the scheduler (or shreduler) does not support a communication loop, nor selection, so two key benchmarks are not possible.  An analysis of the language is provided.
* [Clojure](https://en.wikipedia.org/wiki/Clojure) - instructions available [here](https://clojure.org/).  At present this does not seem to meet the criteria.  The `agents` package provides simple agents that respond to function calls but have no control over their state.  The `core.async` package does provide the functionality, but it is not core Clojure and therefore does not meet criteria 1.
* [Concurrent Pascal](https://en.wikipedia.org/wiki/Concurrent_Pascal) - although it might be a stretch saying this is live.  A compiler for microcontrollers is available [here](https://github.com/dhawk/concurrent-pascal-compiler).  After some investigation it appears that Concurrent Pascal is not available.  No version exists that can be executed on a Linux desktop.
* [D](https://en.wikipedia.org/wiki/D_(programming_language)) - instructions available [here](https://dlang.org/).
* [Dodo](http://dodo.sourceforge.net/)
* [E](https://en.wikipedia.org/wiki/E_(programming_language)) - instructions available [here](http://erights.org/).
* [Eiffel](https://en.wikipedia.org/wiki/Eiffel_(programming_language)) - a good starting point is probably [Eiffel Software](https://www.eiffel.com/).  Concurrency seems to be provided via [SCOOP](https://www.eiffel.org/doc/solutions/Concurrent%20programming%20with%20SCOOP) but unclear if message passing is available.
* [Elixir](https://en.wikipedia.org/wiki/Elixir_(programming_language)) (although this is really just Erlang) - instructions available [here](https://elixir-lang.org/).
* [Esterel](https://en.wikipedia.org/wiki/Esterel) - instructions available [here](http://www.esterel.org/).
* [Falcon](https://en.wikipedia.org/wiki/Falcon_(programming_language)) - instructions available [here](http://falconpl.org/).
* [Fantom](https://en.wikipedia.org/wiki/Fantom_(programming_language)) (although this does run on-top of other runtimes) - instructions available [here](http://www.fantom.org/).
* [Fancy](http://www.fancy-lang.org/)
* [FortranM](https://en.wikipedia.org/wiki/FortranM) - seems out-of-date, but try [here](http://www.netlib.org/fortran-m/).
* [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)) - via the [Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent-Chan.html) type.  Unclear if this is part of standard Haskell.
* [Hume](https://en.wikipedia.org/wiki/Hume_(programming_language)) - instructions available [here](http://www.hume-lang.org/).
* [Go!](https://en.wikipedia.org/wiki/Go!_(programming_language)) (not to be confused with Go) - instructions available [here](https://github.com/frankmccabe/go).
* [Io](https://en.wikipedia.org/wiki/Io_(programming_language)) - instructions available [here](http://iolanguage.org/).
* [J](https://en.wikipedia.org/wiki/J_(programming_language)) - instructions available [here](http://www.jsoftware.com/).
* [Java](https://en.wikipedia.org/wiki/Java_(programming_language)) - the [`SynchronousQueue`](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/SynchronousQueue.html) is part of the standard Java API and provides a synchronous channel like construct.
* [JoCaml](https://en.wikipedia.org/wiki/JoCaml) - instructions available [here](http://jocaml.inria.fr/).
* [Join Java](https://en.wikipedia.org/wiki/Join_Java) - appears to have been a PhD project.  Try [here](http://joinjava.unisa.edu.au/).
* [Julia](https://en.wikipedia.org/wiki/Julia_(programming_language)) - instructions available [here](https://julialang.org/).
* [LabVIEW](https://en.wikipedia.org/wiki/LabVIEW) - might be stretching the definition though.  Instructions available [here](http://www.ni.com/en-gb/shop/labview.html).
* [Limbo](https://en.wikipedia.org/wiki/Limbo_(programming_language)) - instructions available [here](http://www.vitanuova.com/inferno/limbo.html).
* [Lua](https://en.wikipedia.org/wiki/Lua_(programming_language)) - instructions available [here](https://www.lua.org/).
* [MPD](https://en.wikipedia.org/wiki/MPD_(programming_language)) - instructions available [here](https://www2.cs.arizona.edu/mpd/).
* [Mythryl](https://mythryl.org/index3.html)
* [Neko](https://en.wikipedia.org/wiki/Neko_(programming_language)) - instructions available [here](http://nekovm.org/).
* [Newsqueak](https://en.wikipedia.org/wiki/Newsqueak) - may not be suitable, but an interpreter is available [here](https://github.com/rwos/newsqueak).
* [Nim](https://en.wikipedia.org/wiki/Nim_(programming_language)) - instructions available [here](https://nim-lang.org/).
* [OCaml](https://en.wikipedia.org/wiki/OCaml) - instructions available [here](http://ocaml.org/).
* [Oforth](http://www.oforth.com/)
* [Open Object Rexx](https://en.wikipedia.org/wiki/Object_REXX) - instructions available [here](http://www.oorexx.org/).
* [Oz](https://en.wikipedia.org/wiki/Oz_(programming_language)) - the best approach seems to be via [Mozart](http://mozart.github.io/).
* [Panda](https://panda-lang.org/)
* [Perl](https://en.wikipedia.org/wiki/Perl) - although I think this may be via OS process communication.  Instructions available [here](https://www.perl.org/).
* [Perl 6](https://en.wikipedia.org/wiki/Perl_6) - although see comment on Perl.  Instructions available [here](https://perl6.org/).
* [Pony](https://www.ponylang.org/)
* [Preesm](https://en.wikipedia.org/wiki/Preesm) - may be stretching the definition.  Instructions available [here](http://preesm.sourceforge.net/website/).
* [ProcessJ](https://processj.cs.unlv.edu/)
* [Prolog](https://en.wikipedia.org/wiki/Prolog) - there appears to be send message and receive message support.  See [here](http://www.swi-prolog.org/pldoc/man?section=threads)
* [Racket](https://en.wikipedia.org/wiki/Racket_(programming_language)) - instructions available [here](https://racket-lang.org/).
* [Red](https://en.wikipedia.org/wiki/Red_(programming_language)) - instructions available [here](http://www.red-lang.org/).
* [Ruby](https://en.wikipedia.org/wiki/Ruby_(programming_language)) - instructions available [here](https://www.ruby-lang.org/en/).  Unsure about this.
* [SALSA](https://en.wikipedia.org/wiki/SALSA_(programming_language)) - instructions available [here](http://wcl.cs.rpi.edu/salsa/).
* [Scala](https://en.wikipedia.org/wiki/Scala_(programming_language)) - instructions available [here](http://www.scala-lang.org/).  It looks like actors are no longer part of the core library though.
* [SequenceL](https://en.wikipedia.org/wiki/SequenceL) - instructions available [here](https://texasmulticore.com/).
* [Sequoia++](https://web.stanford.edu/group/sequoia/)
* [SIGNAL](https://en.wikipedia.org/wiki/SIGNAL_(programming_language)) - may be stretching the definition.  Also tricky to find.  Try [The Polychrony Toolset](http://www.irisa.fr/espresso/Polychrony/).
* [SISAL](https://en.wikipedia.org/wiki/SISAL) - think this is unavailable.  Try [here](https://sourceforge.net/projects/sisal/).
* [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk) - instructions available [here](http://smalltalk.org/).
* [SpecC](https://en.wikipedia.org/wiki/SpecC) - a reference compiler can be found via [here](http://www.cecs.uci.edu/~specc/).
* [SR](https://en.wikipedia.org/wiki/SR_(programming_language)) - looks like it is no longer maintained.  Try [here](https://www2.cs.arizona.edu/sr/).
* [Standard ML](https://en.wikipedia.org/wiki/Standard_ML) - but most likely [ConcurrentML](https://en.wikipedia.org/wiki/Concurrent_ML).  Try [here](http://cml.cs.uchicago.edu/).
* [SuperPascal](https://en.wikipedia.org/wiki/SuperPascal) - unlikely to be still possible, but try [here](http://brinch-hansen.net/) and [here](https://github.com/octonion/superpascal).
* [Swift](https://en.wikipedia.org/wiki/Swift_(programming_language)) (via Grand Central Dispatch) - Swift is [available for Linux](https://swift.org/download/).
* [SystemC](https://en.wikipedia.org/wiki/SystemC) - but looks like this is no longer available.  [Official website](http://systemc.org/).
* [Tcl](https://en.wikipedia.org/wiki/Tcl) - instructions available [here](http://www.tcl.tk/).
* [Unicon](https://en.wikipedia.org/wiki/Unicon_(programming_language)) - instructions available [here](https://unicon.sourceforge.io/).
* [Vorpal](http://vmlanguages.is-research.de/vorpal/) - seems to have become [Clump](http://d.plaindoux.free.fr/clump/).
* [Wren](http://wren.io/)
* [XC](https://en.wikipedia.org/wiki/XC_(programming_language)) - although unsure if this will work on a general PC.  There is an open source [XCore project](https://github.com/xcore).
* [Zkl](http://www.zenkinetic.com/zkl.html)
* [Zonnon](https://en.wikipedia.org/wiki/Zonnon) - instructions available [here](http://www.zonnon.ethz.ch/).
* [&#x3bc;C++](https://en.wikipedia.org/wiki/%CE%9CC%2B%2B) - instructions available [here](https://plg.uwaterloo.ca/usystem/uC++.html).

Not all of these languages may support message-passing concurrency, as only a quick overview has been made.  Languages may also be missing.  If you know of a language missed please get in touch.  Also, to be included a current implementation of the language is required.  There are historic languages with message-passing concurrency, but seem to be unavailable today.  For comparison purposes, the language must run on Linux.

Language selection criteria:

* the language __must__ provide mechanisms to send a message between components as part of the core language features (e.g., keyword support and/or standard library) and not via an additional library.
* the message must be sent in a manner so that the receiver can __choose__ when to receive it; therefore, giving the receiver control over its internal state.  A method invocation on an object is therefore not a message.
* the receiver __must__ be able to wait __passively__ for a message to be received - that is, a busy spinning on a value to change is not a message.  Having a queue between threads that a receiver tests for readiness is not suitable.
* messages __must__ be any structured data type supported in the language.  Conversion to bytes, strings, or another data serialization technique is __not__ considered message-passing.  Nor is any use of I/O mechanisms to simulate communication.

If you have to download a separate library, or write your own functions, to achieve message-passing then the criteria does not allow the language to be included.  Future work will examine library support, but as numerous examples exist this is currently outside the scope of this work.

## Language Timeline

This section indicates when languages where released.  The aim is to illustrate any clusters of development.

1972. (3) Concurrent Pascal, Prolog, Smalltalk
1973. (1) Standard ML (as ML)
1974. (0)
1975. (0)
1976. (0)
1977. (0)
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
1988. (2) Object REXX, Tcl
1989. (1) SequenceL
1990. (2) Haskell, J
1991. (1) Oz
1992. (1) &#x3bc;C++
1993. (2) Lua, SuperPascal
1994. (2) Newsquek, Racket
1995. (2) Limbo, Ruby, Java
1996. (1) Ocaml
1997. (1) E
1998. (0)
1999. (1) SystemC
2000. (2) Hume, Join Java
2001. (4) CAL Actor Language, D, SALSA, SpecC
2002. (1) Io
2003. (4) C&omega;, ChucK, Falcon, Go!
2004. (1) Scala
2005. (2) Neko, XC
2006. (2) AmbientTalk, Sequoia++
2007. (2) Clojure, Dodo
2008. (2) Nim, PREESM
2009. (3) Axum, Go, ProcessJ
2010. (1) Rust
2011. (3) Aha!, Elixir, Red
2012. (1) Julia
2013. (1) Zonnon
2014. (3) Oforth, Pony, Swift
2015. (2) Panda, Perl 6
2016. (1) Fantom
2017. (0)
2018. (0)

Unknown - Ateji PX, C=, Concurrent ML, Fancy, FortranM, JoCaml, MPD, Mythryl, SR, Unicon, Vorpal, Wren, Zkl

## Additional References

There are a few sites which provide useful information on programming languages:

* [Tiobe Index](http://www.tiobe.com/tiobe-index/) uses Internet search engine results to determine language popularity.
* [GitHub popularity](http://githut.info/) provides information on language popularity in GitHub projects.
* [Rosetta Code](http://rosettacode.org/wiki/Rosetta_Code) provides common examples in various programming languages.
* [List of programming languages on Wikipedia](https://en.wikipedia.org/wiki/List_of_programming_languages) if you want to see how many languages are out there.  Not complete though.

# Appendix

## Properties

The properties taken from process calculi are:

* __Synchronous communication__ - do communicating components wait until they are both ready to complete a communication.
* __First-order channels__ - does the language have a channel construct.
* __Higher-order channels__ - can channels send channels.
* __First-order processes__ - are processes (or threads or similar) assignable to variables.
* __Higher-order processes__ - can processes be communicated to other processes.
* __Parallel execution statement__ - can parallel execution be explicitly defined.
* __Indexed parallel execution statement__ - essentially a parallel for statement.
* __State ownership__ - does the language ensure no sharing of data between processes.
* __Process ownership__ - when a process is created it exists and is owned within the creating context.  If the containing context ends, it must wait for any owned processes.
* __Selection on incoming messages__ - the ability to select from a range of incoming message sources.  Requires first-order channels.
* __Indexed selection__ - the ability to select from an array or similar range of incoming message sources via an index.  Requires first-order channels.
* __Selection based on incoming value__ - the ability to select an incoming message based on the value in the message.
* __Guarded selection__ - the ability activate selection choices based on a boolean condition.
* __Fair selection__ - whether the selection can be considered fair (as far as possible).
* __Selection with timer__ - whether some form of timing can be used during a selection.
* __Other selection types__ - could be anything, but most likely is a default option.
* __Selection of outgoing messages__ - as input selection, but the ability to select on output.
* __Multi-party synchronisation__ - typically a barrier, but some object that allows more than two processes to synchronise at once.
* __Selection on multi-party synchronisation__ - whether the multi-party synchronisation can be used in a selection.

## Benchmarks

At present, three benchmarks have been used.

### Communication Time

This benchmark measures the communication / coordination time of a message.  Four processes are involved:

* __Prefix__ outputs 0, then outputs what it inputs.
* __Delta__ outputs its input on two channels sequentially.
* __Successor__ increments its input before output.
* __Consumer__ times how long it takes to receive _N_ inputs.

The processes are connected together to produce the natural numbers.

The Communicaton Time benchmark (also referred to as commstime) was originally developed in occam by Peter Welch at the University of Kent.

### Select Time

This benchmark measures how long it takes to select a message from a _N_ inputs.  A single reading process services multiple inputs from _N_ writers.  The reader times how long it takes to perform a single selection for different sizes of _N_.

Some languages (typically actor-based ones with mailbox systems) do not fair well when _N_ is greater than the number of threads as the reader cannot service the writers fast enough and the mailbox expands.

### Monte Carlo &pi;

A simple data-parallel benchmark designed to test if the language has multi-core support.  Speed-up measures are derived based on 2, 4, and 8 processes being run.  It is not the time taken to perform the benchmark but the speedup that is important.