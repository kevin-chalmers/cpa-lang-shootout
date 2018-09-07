import std.stdio;
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

void writer(Tid output, int writer)
{
    for (int i = 0; i < count; ++i)
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
    File file = File("st-d-" + to_string(writers) + ".csv", "w");
}

void main()
{
    Tid reader = spawn(&reader, 0);
    Tid writer = spawn(&writer, reader, 0);
}