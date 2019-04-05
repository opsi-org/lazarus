program OsZipTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ZipTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

