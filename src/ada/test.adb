with Ada.Text_IO;
use Ada.Text_IO;

procedure Test is

task type Sender is
    entry OK;
end Sender;

task type Receiver is
    entry Send(T : in Sender);
end Receiver;

task body Sender is
begin
    accept OK do
        Put_Line("OK");
    end;
end Sender;

task body Receiver is
begin
    accept Send(T : in Sender) do
        T.OK;
    end;
end Receiver;

S : Sender;
R : Receiver;

begin
    R.Send(S);
    Put_Line("Sent");
end;