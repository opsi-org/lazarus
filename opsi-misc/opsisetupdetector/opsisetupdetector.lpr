program opsisetupdetector;

{$mode delphi}{$H+}


//{$DEFINE debug}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  //Classes, SysUtils,
  {$IFNDEF WINDOWS}
  //CustApp,
  {$ENDIF WINDOWS}
  Interfaces, Forms, osdform, printers, fileinfo,
  winpeimagereader, lcltranslator, runtimetypeinfocontrols, osdanalyzewin,
  osdhelper, osdbasedata, osdconfigdlg, osdcreate, osddlgnewdependency,
  oscheckbinarybitness, osencoding, osddlgnewproperty, osddatamod,
  osjson, oswebservice, oscrypt,
  osdanalyzegeneral, ChooseInstallerDlg;


{$R *.res}
//{$R manifest.rc}


begin
  Application.Scaled:=True;
  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  (*
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  *)
  {$ENDIF DEBUG}
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.Title:='opsi-setup-detector';
  Application.CreateForm(TresultForm1, resultForm1);
  Application.CreateForm(TFNewDepDlg, FNewDepDlg);
  Application.CreateForm(TFNewPropDlg, FNewPropDlg);
  Application.CreateForm(TFOSDConfigdlg, FOSDConfigdlg);
  Application.CreateForm(TFChooseInstallerDlg, FChooseInstallerDlg);
  Application.Run;
end.

