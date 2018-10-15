structure CommsTime =
struct
    open CML

    fun id(input, output) =
        let val x = (recv input)
        in
            send(output, x);
            id(input, output)
        end

    fun prefix(n, input, output) =
        (send(output, n);
        id(input, output))

    fun delta(input, output1, output2) =
        let val x = (recv input)
        in
            send(output1, x);
            send(output2, x);
            delta(input, output1, output2)
        end

    fun succ(input, output) =
        let val x = (recv input)
        in
            send(output, x + 1);
            succ(input, output)
        end

    fun print(input) =
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
