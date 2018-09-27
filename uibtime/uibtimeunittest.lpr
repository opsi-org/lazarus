program uibtimeunittest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testdatetime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

