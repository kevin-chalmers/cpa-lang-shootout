with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Multi_Cast is
    Number_Of_Children : constant Positive := 3;

    subtype Children is Positive range 1..Number_Of_Children;
    type Group is new Natural range 0..Number_Of_Children;
    type Group_Data_Arrived is array(Group) of Boolean;

    protected type Group_Controller is
        procedure Send(To_Group : in Group; This_Data : in integer);
        entry Receive(Group)(Data : out integer);

    private
        Arrived : Group_Data_Arrived := (others => false);
        The_Data : integer;
    end Group_Controller;

    protected body Group_Controller is

        procedure Send(To_Group : in Group; This_Data : in integer) is
        begin
            if Receive(To_Group)'Count > 0 then
                Arrived(To_Group) := true;
                The_Data := This_Data;
            end if;
        end Send;

        entry Receive(for From in Group)(Data : out integer) when Arrived(From) is
        begin
            if Receive(From)'Count = 0 then
                Arrived(From) := false;
            end if;
            Data := The_Data;
        end Receive;

    end Group_Controller;

    Controller_Instance : Group_Controller;

    task type Child_Task is
        entry Child_Id(Id : in Children);
    end Child_Task;

    Task_Array : array(Children) of Child_Task;

    task body Child_Task is
        Local_Data : integer;
        Local_Id : Children;
    begin
        accept Child_Id(Id : in Children) do
            Local_Id := Id;
        end Child_Id;
        