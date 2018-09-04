with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Test_Fairness is

   task Reader is
      entry Read_1;
      entry Read_2;
      entry Read_3;
      entry Read_4;
   end Reader;
   
   task body Reader is
   begin
      for i in 0..100000 loop
         select
            accept Read_1 do
               Put_Line("1");
            end;
         or
            accept Read_2 do
               Put_Line("2");
            end;
         or
            accept Read_3 do
               Put_Line("3");
            end;
         or
            accept Read_4 do
               Put_Line("4");
            end;
         end select;
      end loop;
   end Reader;
   
   task Writer_1;
   task Writer_2;
   task Writer_3;
   task Writer_4;
   
   task body Writer_1 is
   begin
      for i in 0..25000 loop
         Reader.Read_1;
      end loop;
   end Writer_1;
   
   task body Writer_2 is
   begin
      for i in 0..25000 loop
         Reader.Read_2;
      end loop;
   end Writer_2;
   
   task body Writer_3 is
   begin
      for i in 0..25000 loop
         Reader.Read_3;
      end loop;
   end Writer_3;
   
   task body Writer_4 is
   begin
      for i in 0..25000 loop
         Reader.Read_4;
      end loop;
   end Writer_4;
   
begin
   Put_Line("Test");
end Test_Fairness;
