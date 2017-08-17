import std.stdio;
import std.concurrency;
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
    for (int i = 0; i < count; ++i)
    {
        int value = receiveOnly!int();
        writeln(value);
    }
}

void main()
{
    Tid delta = spawn(&delta, 10000);
    Tid succ = spawn(&succ, 10000);
    Tid pre = spawn(&prefix, 0, 10000);
    delta.send(thisTid);
    delta.send(succ);
    pre.send(delta);
    succ.send(pre);
    printer(10000);
}