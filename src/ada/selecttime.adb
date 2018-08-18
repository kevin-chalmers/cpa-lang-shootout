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

procedure SelectTime is

Number_Writers : INTEGER;
Iterations : CONSTANT INTEGER := 2**24;
Iterations_Experiment : CONSTANT INTEGER := 2**16;

type Stressed_Packet is record
    Writer : INTEGER;
    N : INTEGER;
end record;

task type Reader is
    entry Send0(Value : in Stressed_Packet);
    entry Send1(Value : in Stressed_Packet);
    entry Send2(Value : in Stressed_Packet);
    entry Send3(Value : in Stressed_Packet);
    entry Send4(Value : in Stressed_Packet);
    entry Send5(Value : in Stressed_Packet);
    entry Send6(Value : in Stressed_Packet);
    entry Send7(Value : in Stressed_Packet);
end Reader;

task body Reader is
    Start : Time;
    Total : Time_Span;
    Results : File_Type;
    i : INTEGER := 0;
begin
    Create(File => Results, Mode => Out_File, Name => "st-ada-" & Integer'Image(Number_Writers) & ".csv");
    Start := Clock;
    for count in 0..Iterations loop
        if count mod 65536 = 0 then
            Total := (((Clock - Start) / Iterations_Experiment) * 1000000000) / 4;
            Put_Line(Integer'Image(i) & " " & Float'Image(Float(To_Duration(Total))));
            Put_Line(Results, Float'Image(Float(To_Duration(Total))));
            i := i + 1;
            Start := Clock;
        end if;
        select
            accept Send0(Value : in Stressed_Packet);
            when Number_Writers > 1 =>
                accept Send1(Value : in Stressed_Packet);
            when Number_Writers > 2 =>
                accept Send2(Value : in Stressed_Packet);
                accept Send3(Value : in Stressed_Packet);
            when Number_Writers > 4 =>
                accept Send4(Value : in Stressed_Packet);
                accept Send5(Value : in Stressed_Packet);
                accept Send6(Value : in Stressed_Packet);
                accept Send7(Value : in Stressed_Packet);
        end select;
    end loop;
    Close(Results);
end Reader;

task type Writer is
    entry Construct(ID : in INTEGER; iters : in INTEGER);
end Writer;

Reader_Task : access Reader := null;

task body Writer is
    idx : INTEGER;
    writes : INTEGER;
    packet : Stressed_Packet;
begin
    accept Construct(ID : in INTEGER; iters : in INTEGER) do
        idx := ID;
        writes := iters;
    end;
    for i in 0..writes loop
        packet.Writer := idx;
        packet.N := i;
        case idx is
            when 0 => Reader_Task.Send0(packet);
            when 1 => Reader_Task.Send1(packet);
            when 2 => Reader_Task.Send2(packet);
            when 3 => Reader_Task.Send3(packet);
            when 4 => Reader_Task.Send4(packet);
            when 5 => Reader_Task.Send5(packet);
            when 6 => Reader_Task.Send6(packet);
            when 7 => Reader_Task.Send7(packet);
            when others => Put_Line("Error");
        end case;
    end loop;
end Writer;

Reader_Task : access Reader := null;

procedure experiment(writers : INTEGER) is
begin
    null;
end experiment;

begin
    Put_Line("Select Time Benchmark");
    Number_Writers := 1;
end SelectTime;