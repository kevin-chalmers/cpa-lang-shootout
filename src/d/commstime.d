import std.stdio;
import std.concurrency;
import std.datetime.stopwatch;
import core.thread;

void id(Tid output, int count)
{
    for (int i = 0; i < count; ++i)
    {
        int value = receiveOnly!int();
        output.send(value);
    }
}

void prefix(int N, int count)
{
    Tid output = receiveOnly!Tid();
    output.send(N);
    id(output, count);
}

void delta(int count)
{
    Tid out0 = receiveOnly!Tid();
    Tid out1 = receiveOnly!Tid();
    for (int i = 0; i < count; ++i)
    {
        int value = receiveOnly!int();
        out0.send(value);
        out1.send(value);
    }
}

void succ(int count)
{
    Tid output = receiveOnly!Tid();
    for (int i = 0; i < count; ++i)
    {
        int value = receiveOnly!int();
        output.send(++value);
    }
}

void printer(int count)
{
    StopWatch sw;
    long[100] results;
    sw.start();
    int value = 0;
    for (int i = 0; i < count; ++i)
    {
        if (i % 10000 == 0 && i > 0)
        {
            sw.stop();
            results[i / 10000] = sw.peek.total!"nsecs" / 10000;
            writeln(results[i / 10000]);
            sw.reset();
            sw.start();
        }
        value = receiveOnly!int();
    }
    File file = File("ct-d.csv", "w");
    for (int i = 0; i < 100; ++i)
    {
        file.writeln(results[i]);
    }
    file.close();
}

void main()
{
    int iterations = 1000000;
    Tid delta = spawn(&delta, iterations);
    Tid succ = spawn(&succ, iterations);
    Tid pre = spawn(&prefix, 0, iterations);
    delta.send(thisTid);
    delta.send(succ);
    pre.send(delta);
    succ.send(pre);
    printer(iterations);
}