structure CommsTime =
struct
    open CML

    fun id(input, output, count) =
        if count > 0 then
            let val x = (recv input)
            in
                send(output, x);
                id(input, output, count - 1)
            end
        else
            ()

    fun prefix(n, input, output, count) =
        (send(output, n);
        id(input, output, count))

    fun delta(input, output1, output2, count) =
        if count > 0 then
            let val x = (recv input)
            in
                send(output1, x);
                send(output2, x);
                delta(input, output1, output2, count - 1)
            end
        else
            ()

    fun succ(input, output, count) =
        if count > 0 then
            let val x = (recv input)
            in
                send(output, x + 1);
                succ(input, output, count - 1)
            end
        else
            ()

    fun save_results([], file) =
        ()

    fun save_results((v::results), file) =
        (TextIO.output(file, Int.toString(v));
        TextIO.output(results, "\n");
        save_results(results, file))

    fun print(input, count, start, results) =
        if count mod 10000 = 0 then
            let
                val total = Time.now() - start
            in
                if count > 0 ###### work here
            

        (TextIO.print(Int.toString(recv input));
        TextIO.print(Time.toString(Time.now()));
        TextIO.print("\n");
        print(input))

    fun commstime() =
        let
            val a : int chan = channel()
            val b : int chan = channel()
            val c : int chan = channel()
            val d : int chan = channel()
        in
            spawn(fn() => prefix(0, c, a));
            spawn(fn() => delta(a, b, d));
            spawn(fn() => succ(b, a));
            spawn(fn() => print(d));
            exit()
        end

    fun main() =
        RunCML.doit(fn() => ignore(spawn commstime), NONE)
end
