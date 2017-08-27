with Ada.Text_IO;
use Ada.Text_IO;

procedure TaskType is
task type SHORT_LINE is
end SHORT_LINE;

task type LONG_LINE is
end LONG_LINE;

Cow, Dog, Pig           : SHORT_LINE;
Elephant, Hippopotamus  : LONG_LINE;

task body SHORT_LINE is
begin
    for Index in 1..4 loop
        delay 0.0;
        Put_Line("This is a short line");
    end loop;
end SHORT_LINE;

task body LONG_LINE is
begin
    for Index in 1..3 loop
        delay 0.0;
        Put_Line("This is a much longer line to be displayed");
    end loop;
end LONG_LINE;

begin
    Put_Line("This is an example of use of a task type");
end TaskType;