import std.stdio;
import std.concurrency;
import std.datetime;
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
    
}