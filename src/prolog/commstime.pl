commstime :-
    message_queue_create(A, []),
    message_queue_create(B, []),
    message_queue_create(C, []),
    message_queue_create(D, []),
    thread_create(prefix(0, C, A), _, []),
    thread_create(delta(A, B, D), _, []),
    thread_create(succ(B, C), _, []),
    printer(D).

prefix(N, In, Out) :-
    thread_send_message(Out, N),
    id(In, Out).

id(In, Out) :-
    thread_get_message(In, V),
    thread_send_message(Out, V),
    id(In, Out).

delta(In, Out1, Out2) :-
    thread_get_message(In, V),
    thread_send_message(Out1, V),
    thread_send_message(Out2, V),
    delta(In, Out1, Out2).

succ(In, Out) :-
    thread_get_message(In, V),
    V2 is V + 1,
    thread_send_message(Out, V2),
    succ(In, Out).

printer(In) :-
    thread_get_message(In, V),
    writeln(V),
    printer(In).