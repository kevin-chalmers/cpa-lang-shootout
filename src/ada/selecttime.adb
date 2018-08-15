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

type Stressed_Packet is record
    Writer : INTEGER;
    N : INTEGER;
end record;

task Writer is
begin
    Put_Line("Writer");
end Writer;

begin
    Put_Line("Select Time Benchmark");
end SelectTime;