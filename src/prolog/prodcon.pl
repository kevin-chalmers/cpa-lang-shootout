main :-
    message_queue_create(Id),
    thread_create(producer(Id), P, []),
    thread_create(consumer(Id), C, []),
    thread_join(P, _),
    thread_join(C, _).

producer(Id) :-
    thread_send_message(Id, 5).

consumer(Id) :-
    thread_get_message(Id, Val),
    writeln(Val).