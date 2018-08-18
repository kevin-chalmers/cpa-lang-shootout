with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure MonteCarlo is

task type Master(workers : INTEGER) is
    entry Send(pi : FLOAT);
end Master;

task body Master is
begin
end Master;

begin
    Put_Line("Monte Carlo Pi Benchmark");
end MonteCarlo;