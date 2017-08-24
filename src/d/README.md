# D

[Wikipedia Entry](https://en.wikipedia.org/wiki/D_(programming_language))

[Main Website](https://dlang.org/)

The information presented was taken from D 2.075.1.

## Running the Benchmarks


The benchmarks provided are:
* [Communication Time](commstime.d)
* [Selection Time](selecttime.d)
* [Monte Carlo &pi;](montecarlopi.d)

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
| Other selection types                     | no      |
| Selection of outgoing messages            | no      |
| Multi-party synchronisation               | yes     |
| Selection on multi-party synchronisation  | no      |

### Synchronous Communication

Although D's concurrency model is actor-based with a mailbox, the mailbox can be configured to block with one message.

```
setMaxMailboxSize(proc, 1, block);
```

The mailbox will only accept one message before blocking any further senders.  The synchronisation is therefore not full as the sender and receiver never truly synchronise.  The sender will deposit its message and continue before the receiver acts on it.  It is the following sender that will block.  Therefore synchronous communication is not supported.



