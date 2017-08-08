-module(selecttime).
-export([start/0, writer/3]).

writer(Out, Writer, Count) ->
    if
        Count > 0 ->
            Out ! {Writer, Count},
            writer(Out, Writer, Count - 1);
        true -> ok
    end.

save_results([], _) ->
    ok;
save_results([X|Results], File) ->
    file:write(File, io_lib:fwrite("~p\n", [X])),
    save_results(Results, File).

reader(N, Count, Total, Results) ->
    if
        Count rem 65536 == 0 ->
            {_, Time} = statistics(wall_clock),
            receive {W, C} ->
                if
                    Total > 1 ->
                        io:format("~p ~n", [Time / 65536]),
                        reader(N, 1, Total - 1, [Time / 65536|Results]);
                    true ->
                        {ok, File} = file:open(io_lib:fwrite("st-~p-erl.csv", [N]), [write]),
                        save_results(Results, File)
                end
            end;
        true ->
            receive {W, C} -> reader(N, Count + 1, Total - 1, Results) end
    end.

reader(N, Total) ->
    statistics(wall_clock),
    reader(N, 1, Total, []).

run(N, Count, Reader) ->
    if
        N > 0 ->
            spawn(selecttime, writer, [Reader, N, Count]),
            run(N - 1, Count, Reader);
        true ->
            ok
    end.

selecttime(N, Total) ->
    run(N, Total / N, self()),
    reader(N, Total).

start() ->
    selecttime(1, math:pow(2, 24)),
    selecttime(2, math:pow(2, 24)),
    selecttime(4, math:pow(2, 24)),
    selecttime(8, math:pow(2, 24)).