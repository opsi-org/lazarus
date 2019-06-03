program regextest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, regularexprtest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

