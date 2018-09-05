import std.stdio;
import std.concurrency;
import std.datetime.stopwatch;
import core.thread;

struct Packet
{
    int writer;
    int id;
}

void writer(Tid output, int writer, int count)
{
    for (int i = 0; i < count; ++i)
    {
        Packet p = {writer, i};
        output.send(p);
    }
}

void reader(int N, int count)
{
    StopWatch sw;
    Packet p;
    long[100] results;
    for (int i = 0; i < count / 65536; ++i)
    {
        sw.start();
        for (int j = 0; j < 65536; ++j)
        {
            p = receiveOnly!Packet();
        }
        sw.stop();
        results[i] = sw.peek.total!"nsecs" / 65536;
        writeln(results[i]);
        sw.reset();
    }
}

void main()
{
    Tid reader = spawn(&reader, 0, 65536);
    Tid writer = spawn(&writer, reader, 0, 65536);
}