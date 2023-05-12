program opsiconfigrulesmgr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, ocr_form, ocrbasedata, ocrconfigdlg,
  osmessagedialog, oswebservice, osjson, osparserhelper
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFOSDConfigdlg, FOSDConfigdlg);
  Application.CreateForm(TMyMessageDlg, MyMessageDlg);
  Application.Run;
end.

