-module(montecarlopi).
-export([start/0, montecarlopi/4]).

montecarlopi(Iterations, Count, InCircle, Master) ->
  if
    Count /= Iterations ->
      X = rand:uniform(),
      Y = rand:uniform(),
      Length = X * X + Y * Y,
      if
        Length =< 1.0 ->
          montecarlopi(Iterations, Count + 1, InCircle + 1, Master);
        true ->
          montecarlopi(Iterations, Count + 1, InCircle, Master)
      end;
    true ->
      Master ! (4.0 * InCircle) / Iterations
  end.

gather(Count, Pi) ->
  if
    Count > 0 ->
      receive N -> gather(Count - 1, Pi + N) end;
    true ->
      Pi
  end.

run(Count, Iterations, Master) ->
  if
    Count > 0 ->
      spawn(montecarlopi, montecarlopi, [Iterations, 0, 0, Master]),
      run(Count - 1, Iterations, Master);
    true ->
      ok
  end.

save_results([], File) ->
  ok;
save_results([X|Results], File) ->
  file:write(File, io_lib:fwrite("~p\n", [X])),
  save_results(Results, File).

experiment(Count, Iterations, Threads, Results) ->
  if
    Count > 0 ->
      run(Threads, Iterations / Threads, self()),
      Pi = gather(Threads, 0.0) / Threads,
      {_, Time} = statistics(wall_clock),
      io:format("~p ~p ~p~n", [Count, Pi, Time]),
      experiment(Count - 1, Iterations, Threads, [Time|Results]);
    true ->
      {ok, File} = file:open(io_lib:fwrite("mcp-erl-~p.csv", [Threads]), [write]),
      save_results(Results, File)
  end.

start() ->
  experiment(100, math:pow(2, 24), 1, []),
  experiment(100, math:pow(2, 24), 2, []),
  experiment(100, math:pow(2, 24), 4, []),
  experiment(100, math:pow(2, 24), 8, []),
  experiment(100, math:pow(2, 24), 16, []).
