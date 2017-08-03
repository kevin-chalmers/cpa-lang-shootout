-module(stressselect).
-export([start/0, stressedwriter/4]).

stressedwriter(Out, Writer, N, Max) when N =< Max ->
  Out ! {Writer, N},
  stressedwriter(Out, Writer, N + 1, Max);

stressedwriter(Out, Writer, N, Max) ->
  Out ! {Writer, N}.

stressedreader(N, Count, Writers, Max) ->
  if
    Count rem 10000 == 0 ->
      {_, Total} = statistics(wall_clock),
      receive {Writer, X} -> io:format("writers ~p time = ~p~n", [Writers, Total / 10000]) end,
      if
        N < Writers * Max ->
          stressedreader(N + 1, 1, Writers, Max);
        true -> {}
      end;
    true ->
      receive {Writer, X} -> stressedreader(N + 1, Count + 1, Writers, Max) end
  end.

createwriters(N, Idx, Reader, Max) when N > 0 ->
  spawn(stressselect, stressedwriter, [Reader, Idx, 0, Max]),
  createwriters(N - 1, Idx + 1, Reader, Max);

createwriters(N, Idx, Reader, Max) ->
  spawn(stressselect, stressedwriter, [Reader, Idx, 0, Max]).

stressselect(N, Max) ->
  statistics(wall_clock),
  createwriters(N, 0, self(), Max),
  stressedreader(0, 1, N, Max).

experiment(Writers, Max) when Writers < 8192 ->
  stressselect(Writers, Max),
  experiment(Writers * 2, Max);

experiment(Writers, Max) ->
  stressselect(Writers, Max).

start() ->
  experiment(1, 1000000).
