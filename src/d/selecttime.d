import std.stdio;
import std.string;
import std.concurrency;
import std.datetime.stopwatch;
import core.thread;

const int ITERATIONS = 1 << 24;
const int ITERATIONS_EXPERIMENT = 1 << 16;

struct Packet
{
    int writer;
    int id;
}

void writer(Tid output, int my_iters, int writer)
{
    for (int i = 0; i < my_iters; ++i)
    {
        Packet p = {writer, i};
        output.send(p);
    }
}

void reader(int writers)
{
    StopWatch sw;
    Packet p;
    long[ITERATIONS / ITERATIONS_EXPERIMENT] results;
    for (int i = 0; i < ITERATIONS / ITERATIONS_EXPERIMENT; ++i)
    {
        sw.start();
        for (int j = 0; j < ITERATIONS_EXPERIMENT; ++j)
        {
            p = receiveOnly!Packet();
        }
        sw.stop();
        results[i] = sw.peek.total!"nsecs" / ITERATIONS_EXPERIMENT;
        writeln(results[i]);
        sw.reset();
    }
    string str = format("st-d-%s.csv", writers);
    File file = File(str, "w");
}

void experiment(const int writers)
{
    for (int i = 0; i < writers; ++i)
    {
        Tid writer = spawn(&writer, thisTid, ITERATIONS / writers, i);
    }
    reader(writers);
}

void main()
{
    writeln("Select Time Benchmark");
    for (int i = 1; i <= 16; i *= 2)
    {
        string str = format("%s writers", i);
        writeln(str);
        experiment(i);
    }
}