with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Real_Time;
use Ada.Real_Time;

procedure CommsTime is

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
    SEQ_DELTA.Send(0);
    loop
        accept Send(Value : in INTEGER) do
            N := Value;
        end;
        SEQ_DELTA.Send(N);
    end loop;
end PREFIX;

task body SEQ_DELTA is
    N : INTEGER;
begin
    loop
        accept Send(Value : in INTEGER) do
            N := Value;
        end;
        PRINTER.Send(Value => N);
        SUCC.Send(Value => N);
    end loop;
end SEQ_DELTA;

task body SUCC is
    N : INTEGER;
begin
    loop
        accept Send(Value : in INTEGER) do
            N := Value;
        end;
        PREFIX.Send(N + 1);
    end loop;
end SUCC;

task body PRINTER is
    N : INTEGER;
    Start : Time;
    Total : Time_Span;
begin
    for i in 0 .. 100 loop
        Start := Clock;
        for j in 0 .. 10000 loop
            accept Send(Value : in INTEGER) do
                N := Value;
            end;
        end loop;
        -- Divide by iterations, then convert to nanos.
        -- Four communications.
        Total := (((Clock - Start) / 10000) * 1000000000) / 4;
        Put_Line(Duration'Image(To_Duration(Total)));
    end loop;
end PRINTER;

begin
    Put_Line("This is an example of use of a task type");
end CommsTime;