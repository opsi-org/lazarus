program osregexptest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, osregularexprtest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

