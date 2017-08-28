# Ada

Ada is an interesting language, in that it has a similar model to actors (send messages directly to tasks), yet does not use a mailbox, providing synchronous rendezvous via an interface of acceptable entries.  These entries could be considered messages in a mailbox, but there is selection based on the message type.  It appears to have channel-based language influences, but without the channels.

## Language Features

| Feature                                   | Support   |
| ----------------------------------------- | --------- |
| Synchronous communication                 | yes       |
| First-order channels                      | no        |
| Higher-order channels                     | no        |
| First-order processes                     | yes       |
| Higher-order processes                    | yes       |
| Parallel execution statement              | no        |
| Indexed parallel execution statement      | yes       |
| State ownership                           | no        |
| Process ownership                         | no        |
| Selection on incoming messages            | yes       |
| Indexed selection                         | no        |
| Selection based on incoming value         | yes       |
| Guarded selection                         | yes       |
| Fair selection                            | undefined |
| Selection with timer                      | yes       |
| Other selection types                     | yes       |
| Selection of outgoing messages            | no        |
| Multi-party synchronisation               | no        |
| Selection on multi-party synchronisation  | no        |

### Synchronous Communication

Communication between tasks in Ada are synchronous, involved in what is termed at rendezvous during communication.  ```accept``` statements and matching calls cause rendezvous to occur.

```
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

```
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

```
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

This is a weird definition, but meets the general principle.  As Ada provides first-order processes, and Ada also provides arrays, and furthermore that tasks are started when they are instantiated, a form of indexed parallel execution is available, although none blocking on creation.

```
task type My_Task is
end My_Task;

-- Rest of definition

tasks : array(1..N) of My_Task;
```

### Selection on Incoming Messages