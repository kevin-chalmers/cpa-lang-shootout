import std.stdio;
import std.concurrency;
import std.datetime.stopwatch;
import core.thread;

const int EXPERIMENTS = 100;
const int ITERATIONS_EXPERIMENT = 10000;

void id(Tid output)
{
    for (int i = 0; i < ITERATIONS_EXPERIMENT; ++i)
    {
        int value = receiveOnly!int();
        output.send(value);
    }
}

void prefix(int N)
{
    Tid output = receiveOnly!Tid();
    for (int i = 0; i < EXPERIMENTS; ++i)
    {
        output.send(N);
        id(output);
    }
}

void delta()
{
    Tid out0 = receiveOnly!Tid();
    Tid out1 = receiveOnly!Tid();
    for (int i = 0; i < EXPERIMENTS; ++i)
    {
        for (int j = 0; j < ITERATIONS_EXPERIMENT; ++j)
        {
            int value = receiveOnly!int();
            out0.send(value);
            out1.send(value);
        }
    }
}

void succ()
{
    Tid output = receiveOnly!Tid();
    for (int i = 0; i < EXPERIMENTS; ++i)
    {
        for (int j = 0; j < ITERATIONS_EXPERIMENT; ++j)
        {
            int value = receiveOnly!int();
            output.send(++value);
        }
    }
}

void printer()
{
    StopWatch sw;
    long[EXPERIMENTS] results;
    int value = 0;
    for (int i = 0; i < EXPERIMENTS; ++i)
    {
        sw.reset();
        sw.start();
        for (int j = 0; j < ITERATIONS_EXPERIMENT; ++j)
        {
            value = receiveOnly!int();
        }
        sw.stop();
        results[i] = sw.peek.total!"nsecs" / ITERATIONS_EXPERIMENT;
        writeln(results[i]);
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
    Tid delta = spawn(&delta);
    Tid succ = spawn(&succ);
    Tid pre = spawn(&prefix, 0);
    delta.send(thisTid);
    delta.send(succ);
    pre.send(delta);
    succ.send(pre);
    printer();
}