program networkcalculatortest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, networkcalctest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

