# Ada

[Wikipedia Entry](https://en.wikipedia.org/wiki/Ada_(programming_language))

[Main Website](http://www.adaic.org/)

[Main Implementation - GNAT](https://www.adacore.com/)

Ada is an interesting language, in that it has a similar model to actors (send messages directly to tasks), yet does not use a mailbox, providing synchronous rendezvous via an interface of acceptable entries.  These entries could be considered messages in a mailbox, but there is selection based on the message type.  It appears to have channel-based language influences, but without the channels.

## Running the Benchmarks

Information was gathered via `gnatmake` with the `-O3` optimisation switch.  The command was:

```gnatmake -O3 <filename>```

The benchmarks provided are:

- [Communication Time](commstime.adb)
- [Selection Time](selecttime.adb)
- [Monte Carlo &pi;](montecarlopi.adb)

## Language Features

| Feature                                   | Support   |
| ----------------------------------------- | --------- |
| Synchronous communication                 | yes       |
| First-order channels                      | no        |
| Higher-order channels                     | no        |
| First-order processes                     | yes       |
| Higher-order processes                    | yes       |
| Parallel execution statement              | no        |
| Indexed parallel execution statement      | partial   |
| State ownership                           | no        |
| Process ownership                         | partial   |
| Selection on incoming messages            | yes       |
| Indexed selection                         | partial   |
| Selection based on incoming value         | yes       |
| Guarded selection                         | yes       |
| Fair selection                            | no?       |
| Selection with timer                      | yes       |
| Other selection types                     | yes       |
| Selection on outgoing messages            | no        |
| Multi-party synchronisation               | no        |
| Selection on multi-party synchronisation  | no        |

### Synchronous Communication

Communication between tasks in Ada are synchronous, involved in what is termed at rendezvous during communication.  `accept` statements and matching calls cause rendezvous to occur.

```ada
task Receiver is
    entry Send(Value : INTEGER);
end Receiver;

task Sender;

task body Receiver is
begin
    accept Send(Value : Integer) do
        -- In a rendezvous
    end; -- end of rendezvous
end Receiver;

task body Sender is
begin
    Receiver.Send(0); -- rendezvous initiated here.
end Sender;
```

### First-order Processes

An Ada task can be defined as a type and instances created, or it can just be defined.  The following illustrates a task being allocated to a variable.

```ada
task type My_Task is
    -- some entry definitions
end My_Task;

task body My_Task is
begin
    -- body definition
end My_Task;

T : My_Task; -- variable of type My_Task.
```

### Higher-order Processes

Ada tasks can be sent during rendezvous if the ```entry``` has defined as such.

```ada
task type Sender is
    entry OK;
end Sender;

task type Receiver is
    entry Send(T : in Sender);
end Receiver;

task body Sender is
begin
    accept OK do
        -- do something
    end;
end Sender;

task body Receiver is
begin
    accept Send(T : in Sender) do
        -- do something with the Sender
    end;
end Receiver;

S : Sender;
R : Receiver;

begin
    R.Send(S);
end;
```

### Indexed Parallel Execution Statement

This is a weird definition, but meets the general principle in part.  As Ada provides first-order processes, also provides arrays, and furthermore that tasks are started when they are instantiated, a form of indexed parallel execution is available, although none blocking on creation.

```ada
task type My_Task is
end My_Task;

-- Rest of definition

tasks : array(1..N) of My_Task;
```

This is a partial meeting of requirements as we cannot pass the index parameter directly to task creation.  A `for` loop and `access` types would solve the problem, but this is not an indexed parallel execution.

### Process Ownership

There is some sense of ownership in so far that created tasks are waited for on creation.  Ownership does depend on how a task is created.  When created in the scope of a procedure, then the procedure will wait for a task to complete.  [Select Time](selecttime.adb) illustrates in the `experiment` procedure:

```ada
procedure experiment(writers : INTEGER) is
    writer_tasks : array(1..writers) of access Writer;
begin
    Reader_Task := new Reader(writers);
    for i in 1..writers loop
        Put_Line("=>" & Integer'Image(i));
        writer_tasks(i) := new Writer(i, Iterations / writers);
    end loop;
end experiment;
```

### Selection on Incoming Messages

Selection on `entry` is performed with the `select` statement:

```ada
task body My_Task is
begin
    select
        accept entry_1;
    or
        accept entry_2;
    or
        accept entry_3;
    end select;
end;
```

`or` distinguishes the different cases.

### Indexed Selection

As [Go](../go) it is possible to partially fulfill a form of indexed selection through manipulation of entry families and a `for` loop.

```ada
type entry_range is range 0..10;

task type My_Task is
    entry My_Entry(entry_range)(value : in Integer);
end My_Task;

task body My_Task is
begin
    for i in entry_range loop
        select
            accept(i)(value : in Integer);
        else
            null;
        end select;
    end loop;
end My_Task;
```

This will be a busy indexed selection and provides the general idea.  As Ada's `select` is not fair the behaviour is roughly approximate.  [Go's](../go) promotes different behviour to their standard select.

### Selection Based on Incoming Value

Ada's `accept` statement can be used with specific values and therefore the requirement is met.  Below is an example.

```ada
type Entry_Range is range 0..10;
   
task My_Task is
    entry My_Entry(Entry_Range);
end My_Task;

task body My_Task is
begin
    select
        accept My_Entry(5);
    or
        accept My_Entry(10);
    end select;
end My_Task;
```

### Guarded Selection

Ada provides a `when` statement to activate an `entry` based on a conditional:

```ada
task My_Task is
    entry Entry_1;
    entry Entry_2;
    entry Entry_3;
end My_Task;

task body My_Task is
    value : Boolean := True;
begin
    select
        when value => accept Entry_1;
    or
        when value => accept Entry_2;
    or
        when not value => accept Entry_3;
    end select;
end My_Task;
```

### Fair Selection 

Finding a definition for the fairness of selection has been difficult so the following [test program](testfairness.adb) has been used:

```ada
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Test_Fairness is

   task Reader is
      entry Read_1;
      entry Read_2;
      entry Read_3;
      entry Read_4;
   end Reader;
   
   task body Reader is
   begin
      for i in 0..100000 loop
         select
            accept Read_1 do
               Put_Line("1");
            end;
         or
            accept Read_2 do
               Put_Line("2");
            end;
         or
            accept Read_3 do
               Put_Line("3");
            end;
         or
            accept Read_4 do
               Put_Line("4");
            end;
         end select;
      end loop;
   end Reader;
   
   task Writer_1;
   task Writer_2;
   task Writer_3;
   task Writer_4;
   
   task body Writer_1 is
   begin
      for i in 0..25000 loop
         Reader.Read_1;
      end loop;
   end Writer_1;
   
   task body Writer_2 is
   begin
      for i in 0..25000 loop
         Reader.Read_2;
      end loop;
   end Writer_2;
   
   task body Writer_3 is
   begin
      for i in 0..25000 loop
         Reader.Read_3;
      end loop;
   end Writer_3;
   
   task body Writer_4 is
   begin
      for i in 0..25000 loop
         Reader.Read_4;
      end loop;
   end Writer_4;
   
begin
   Put_Line("Test");
end Test_Fairness;
```

The results indicate that there is a priority based on declaration order of the `accept` statements.  Therefore, Ada is considered to have a priority (not-fair) `select`.

### Selection with Timer

Ada provides a `delay` statement that can be used in a `select` statement:

```ada
task My_Task is
    entry My_Entry;
end My_Task;

task body My_Task is
begin
    select
        accept My_Entry;
    or
        delay 10.0;
    end select;
end My_Task;
```

### Other Selection Types

Ada's `select` can have a default (`else`) clause allowing a fall-through much like a SKIP in CSP.

```ada
task My_Task is
    entry My_Entry;
end My_Task;

task body My_Task is
begin
    select
        accept My_Entry;
    else
        null;
    end select;
end My_Task;
```

## Compilation Process and Runtime Environment

Ada code is converted into binary code which will run as a standalone executable.  There is no virtual machine, and everything runs directly on the host machine.