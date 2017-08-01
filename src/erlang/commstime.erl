-module(commstime).
-export([start/0, commstime/2, printer/1, delta/1, succ/1, prefix/2]).

id(Out, Count) ->
  receive N -> Out ! N end,
  if
    Count > 0 -> id(Out, Count - 1);
    true -> ok
  end.

prefix(N, Count) ->
  receive Out -> Out ! N end,
  id(Out, Count).

save_results([], File) ->
  ok;
save_results([X|Results], File) ->
  file:write(File, io_lib:fwrite("~p\n", [X])),
  save_results(Results, File).

printer(Count, Total, Results) ->
  if
    Count rem 10000 == 0 ->
      {_, Time} = statistics(wall_clock),
      receive _ ->
        if
          Total > 1 -> printer(1, Total - 1, [Time / 10000|Results]);
          true ->
            {ok, File} = file:open("ct-erl.csv", [write]),
            save_results(Results, File)
        end
      end;
    true ->
      receive _ -> printer(Count + 1, Total - 1, Results) end
  end.

printer(Total) ->
  statistics(wall_clock),
  printer(1, Total, []).

delta(Out0, Out1, Total) ->
  receive N -> Out0 ! N, Out1 ! N end,
  if
    Total > 0 -> delta(Out0, Out1, Total - 1);
    true -> ok
  end.

delta(Total) ->
  receive {Out0, Out1} -> delta(Out0, Out1, Total) end.

succ(Out, Total) ->
  receive N -> Out ! (N + 1) end,
  if
    Total > 0 -> succ(Out, Total - 1);
    true -> ok
  end.

succ(Total) ->
  receive Out -> succ(Out, Total) end.

commstime(N, Total) ->
  Delta = spawn(commstime, delta, [Total]),
  Succ = spawn(commstime, succ, [Total]),
  Pre = spawn(commstime, prefix, [N, Total]),
  Delta ! {self(), Succ},
  Pre ! Delta,
  Succ ! Pre,
  printer(Total).

start() ->
  commstime(0, 10000000).
