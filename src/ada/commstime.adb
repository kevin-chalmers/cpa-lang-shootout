with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Text_IO.Text_Streams;
use Ada.Text_IO.Text_Streams;
with Ada.Text_IO.Unbounded_IO;
use Ada.Text_IO.Unbounded_IO;
with Ada.Real_Time;
use Ada.Real_Time;

procedure CommsTime is

-- Parameters for the experimental run.
-- Experiments - number of data points collected
-- Iterations_Experiment - number of cycles round commstime for a single data point
Experiments : CONSTANT INTEGER := 100;
Iterations_Experiment : CONSTANT INTEGER := 10000;

task PREFIX is
    entry Send(Value : in INTEGER);
end PREFIX;

task SEQ_DELTA is
    entry Send(Value : in INTEGER);
end SEQ_DELTA;

task SUCC is
    entry Send(Value : in INTEGER);
end SUCC;

task PRINTER is
    entry Send(Value : in INTEGER);
end PRINTER;

task body PREFIX is
    N : INTEGER;
begin
    for i in 0..Experiments loop
        SEQ_DELTA.Send(0);
        for j in 0..Iterations_Experiment loop
            accept Send(Value : in INTEGER) do
                N := Value;
            end;
            SEQ_DELTA.Send(N);
        end loop;
        -- Accept last value in
        accept Send(Value : in INTEGER);
    end loop;
end PREFIX;

task body SEQ_DELTA is
    N : INTEGER;
begin
    for i in 0..Experiments loop
        for j in 0..Iterations_Experiment loop
            accept Send(Value : in INTEGER) do
                N := Value;
            end;
            PRINTER.Send(Value => N);
            SUCC.Send(Value => N);
        end loop;
    end loop;
end SEQ_DELTA;

task body SUCC is
    N : INTEGER;
begin
    for i in 0..Experiments loop
        for j in 0..Iterations_Experiment loop
            accept Send(Value : in INTEGER) do
                N := Value;
            end;
            PREFIX.Send(N + 1);
        end loop;
    end loop;
end SUCC;

task body PRINTER is
    N : INTEGER;
    Start : Time;
    Total : Time_Span;
    Results : File_Type;
begin
    Create(File => Results, Mode => Out_File, Name => "commstime_ada.csv");
    for i in 0..Experiments loop
        Start := Clock;
        for j in 0..Iterations_Experiment loop
            accept Send(Value : in INTEGER) do
                N := Value;
            end;
        end loop;
        -- Divide by iterations, then convert to nanos.
        -- Four communications.
        Total := (((Clock - Start) / Iterations_Experiment) * 1000000000) / 4;
        Put_Line(results, Float'Image(Float(To_Duration(Total))));
        Put(".");
    end loop;
    Close(Results);
end PRINTER;

begin
    Put_Line("Communication Time Benchmark");
end CommsTime;