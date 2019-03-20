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

Iterations : CONSTANT INTEGER := 2**24;
Iterations_Experiment : CONSTANT INTEGER := 2**16;

type Stressed_Packet is record
    Writer : INTEGER;
    N : INTEGER;
end record;

task type Reader(writers : INTEGER) is
    entry Send1(Value : in Stressed_Packet);
    entry Send2(Value : in Stressed_Packet);
    entry Send3(Value : in Stressed_Packet);
    entry Send4(Value : in Stressed_Packet);
    entry Send5(Value : in Stressed_Packet);
    entry Send6(Value : in Stressed_Packet);
    entry Send7(Value : in Stressed_Packet);
    entry Send8(Value : in Stressed_Packet);
end Reader;

task body Reader is
    Start : Time;
    Total : Time_Span;
    Results : File_Type;
    i : INTEGER := 0;
begin
    Create(File => Results, Mode => Out_File, Name => "results/st-ada-" & Integer'Image(writers) & ".csv");
    Start := Clock;
    for count in 1..Iterations loop
        if count mod 65536 = 0 then
            Total := (((Clock - Start) / Iterations_Experiment) * 1000000000) / 4;
            Put_Line(Integer'Image(i) & " " & Float'Image(Float(To_Duration(Total))));
            Put_Line(Results, Float'Image(Float(To_Duration(Total))));
            i := i + 1;
            Start := Clock;
        end if;
        case writers is
            when 1 =>
                select
                    accept Send1(Value : in Stressed_Packet);
                end select;
            when 2 =>
                select
                    accept Send1(Value : in Stressed_Packet);
                or    
                    accept Send2(Value : in Stressed_Packet);
                end select;
            when 4 =>
                select
                    accept Send1(Value : in Stressed_Packet);
                or
                    accept Send2(Value : in Stressed_Packet);
                or
                    accept Send3(Value : in Stressed_Packet);
                or
                    accept Send4(Value : in Stressed_Packet);
                end select;
            when 8 =>
                select
                    accept Send1(Value : in Stressed_Packet);
                or
                    accept Send2(Value : in Stressed_Packet);
                or
                    accept Send3(Value : in Stressed_Packet);
                or
                    accept Send4(Value : in Stressed_Packet);
                or
                    accept Send5(Value : in Stressed_Packet);
                or
                    accept Send6(Value : in Stressed_Packet);
                or
                    accept Send7(Value : in Stressed_Packet);
                or
                    accept Send8(Value : in Stressed_Packet);
                end select;
            when others =>
                Put_Line("Error");
        end case;
    end loop;
    Close(Results);
end Reader;

task type Writer(idx : INTEGER; writes : INTEGER);

Reader_Task : access Reader := null;

task body Writer is
    packet : Stressed_Packet;
begin
    for i in 1..writes loop
        packet.Writer := idx;
        packet.N := i;
        case idx is
            when 1 => Reader_Task.Send1(packet);
            when 2 => Reader_Task.Send2(packet);
            when 3 => Reader_Task.Send3(packet);
            when 4 => Reader_Task.Send4(packet);
            when 5 => Reader_Task.Send5(packet);
            when 6 => Reader_Task.Send6(packet);
            when 7 => Reader_Task.Send7(packet);
            when 8 => Reader_Task.Send8(packet);
            when others => Put_Line("Error");
        end case;
    end loop;
end Writer;

procedure experiment(writers : INTEGER) is
    writer_tasks : array(1..writers) of access Writer;
begin
    Reader_Task := new Reader(writers);
    for i in 1..writers loop
        writer_tasks(i) := new Writer(i, Iterations / writers);
    end loop;
end experiment;

begin
    Put_Line("Select Time Benchmark");
    Put_Line("1");
    experiment(1);
    Put_Line("2");
    experiment(2);
    Put_Line("4");
    experiment(4);
    Put_Line("8");
    experiment(8);
end SelectTime;