program urlparsertest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, urlparserbuildertest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

