with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Real_Time;
use Ada.Real_Time;
with Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;

procedure MonteCarloPi is

   EXPERIMENTS : constant Integer := 100;
   ITERATIONS : constant Integer := 2**24;
   
   task type Master(workers : Integer) is
      entry Result(pi : in Float);
   end Master;
   
   task body Master is
      Start : Time;
      Total : Time_Span;
      Results : File_Type;
      Total_Pi : Float;
   begin
      Create(File => Results, Mode => Out_File, Name => "mcp-ada-" & Integer'Image(workers) & ".csv");
      Start := Clock;
      for i in 1..EXPERIMENTS loop
         Total_Pi := 0.0;
         for w in 1..workers loop
            accept Result(pi : in Float) do
               Total_Pi := Total_Pi + pi;
            end Result;
         end loop;
         Total_Pi := Total_Pi / Float(workers);
         Total := (Clock - Start) * 1000000000;
         Put_Line(Float'Image(Total_Pi) & " in " & Float'Image(Float(To_Duration(Total))) & "ns");
         Put_Line(Results, Float'Image(Float(To_Duration(Total))));
         Start := Clock;
      end loop;
   end Master;
   
   Master_Task : access Master := null;
   
   task type Worker(iters : Integer);
   
   task body Worker is
      gen : Generator;
      in_circle : Integer := 0;
      x : Float;
      y : Float;
      pi : Float;
   begin
      for i in 1..EXPERIMENTS loop
         for j in 1..iters loop
            x := Random(gen);
            y := Random(gen);
            if x**2 + y**2 <= 1.0 then
               in_circle := in_circle + 1;
            end if;
         end loop;
         pi := (4.0 * Float(in_circle)) / Float(iters);
         Master_Task.Result(pi);
         in_circle := 0;
      end loop;
   end Worker;
   
   procedure Experiment(writers : Integer) is
      workers : array(1..writers) of access Worker;
   begin
      Master_Task := new Master(writers);
      for i in 1..writers loop
         workers(i) := new Worker(ITERATIONS / writers);
      end loop;
   end Experiment;
   
begin
   Put_Line("Monte Carlo Pi Benchmark");
   Put_Line("1");
   Experiment(1);
   Put_Line("2");
   Experiment(2);
   Put_Line("4");
   Experiment(4);
   Put_Line("8");
   Experiment(8);
   Put_Line("16");
   Experiment(16);
end MonteCarloPi;
