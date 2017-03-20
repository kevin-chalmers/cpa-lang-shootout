-module(commstime).
-export([start/0, commstime/1, printer/0, delta/0, succ/0, prefix/1]).

id(Out) ->
  receive N -> Out ! N end,
  id(Out).

prefix(N) ->
  receive Out -> Out ! N end,
  id(Out).

printer(Count) ->
  if
    Count rem 10000 == 0 ->
      {_, Total} = statistics(wall_clock),
      receive N -> io:format("~p~n", [Total / 10000]) end,
      printer(1);
    true ->
      receive N -> printer(Count + 1) end
  end.

printer() ->
  statistics(wall_clock),
  printer(1).

delta(Out0, Out1) ->
  receive N -> Out0 ! N, Out1 ! N end,
  delta(Out0, Out1).

delta() ->
  receive {Out0, Out1} -> delta(Out0, Out1) end.

succ(Out) ->
  receive N -> Out ! (N + 1) end,
  succ(Out).

succ() ->
  receive Out -> succ(Out) end.

commstime(N) ->
  Printer = spawn(commstime, printer, []),
  Delta = spawn(commstime, delta, []),
  Succ = spawn(commstime, succ, []),
  Pre = spawn(commstime, prefix, [N]),
  Delta ! {Printer, Succ},
  Pre ! Delta,
  Succ ! Pre.

start() ->
  commstime(0).
