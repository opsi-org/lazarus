program opsisetupdetector;

{$mode delphi}{$H+}

//{$APPTYPE CONSOLE}
//{$DEFINE debug}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Interfaces,
  Forms,
  osdform,
  //VersionInfoX,
  //help,
  printers, fileinfo, winpeimagereader, lcltranslator, runtimetypeinfocontrols,
  osdanalyze, osdhelper, osdbasedata, osdconfigdlg, osdcreate,
  osddlgnewdependency, oscheckbinarybitness, osddlgnewproperty;


{$R *.res}
//{$R manifest.rc}


begin
  Application.Scaled:=True;
  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}
  Application.Initialize;
  Application.Title:='opsi-setup-detector';
  Application.CreateForm(TresultForm1, resultForm1);
    Application.CreateForm(TFOSDConfigdlg, FOSDConfigdlg);
  Application.CreateForm(TFNewDepDlg, FNewDepDlg);
  Application.CreateForm(TFNewPropDlg, FNewPropDlg);
  //Application.CreateForm(TFormHelp, FormHelp);
  Application.Run;
  //main;
end.

