import std.stdio;
import std.string;
import std.random;
import std.concurrency;
import std.datetime.stopwatch;
import core.thread;

const int EXPERIMENTS = 100;
const int ITERATIONS = 1 << 24;

void montecarlo(Tid master, int iters)
{
    for (int i = 0; i < EXPERIMENTS; ++i)
    {
        int in_circle = 0;
        for (int j = 0; j < iters; ++j)
        {
            auto x = uniform(0.0, 1.0);
            auto y = uniform(0.0, 1.0);
            if (x * x + y * y <= 1.0)
                ++in_circle;
        }
        float pi = (4.0 * float(in_circle)) / float(iters);
        master.send(pi);
    }
}

void master(int workers)
{
    StopWatch sw;
    long[EXPERIMENTS] results;
    for (int i = 0; i < EXPERIMENTS; ++i)
    {
        sw.reset();
        sw.start();
        float total_pi = 0.0;
        for (int j = 0; j < workers; ++j)
        {
            total_pi += receiveOnly!float();
        }
        total_pi /= float(workers);
        sw.stop();
        results[i] = sw.peek.total!"nsecs";
        writeln(format("%s in %sns", total_pi, results[i]));
    }
    File file = File(format("mcp-d-%s.csv", workers), "w");
    for (int i = 0; i < EXPERIMENTS; ++i)
    {
        file.writeln(results[i]);
    }
    file.close();
}

void experiment(const int workers)
{
    for (int i = 0; i < workers; ++i)
    {
        Tid worker = spawn(&montecarlo, thisTid, ITERATIONS / workers);
    }
    master(workers);
}

void main()
{
    writeln("Monte Carlo Pi Benchmark");
    for (int i = 1; i <= 16; i *= 2)
    {
        writeln(format("%s workers", i));
        experiment(i);
    }
}