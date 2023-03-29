program opsisetupdetector;

{$mode delphi}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces,
  Forms,
  osdform,
  Printers,
  fileinfo,
  winpeimagereader,
  lcltranslator,
  runtimetypeinfocontrols,
  osdanalyzewin,
  osdhelper,
  osdbasedata,
  osdconfigdlg,
  osdcreate,
  osddlgnewdependency,
  oscheckbinarybitness,
  osencoding,
  osddlgnewproperty,
  osddatamod,
  osjson,
  oswebservice,
  oscrypt,
  osmessagedialog,
  osdanalyzegeneral,
  ChooseInstallerDlg;


{$R *.res}
//{$R manifest.rc}


begin
  Application.Scaled := True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Title := 'opsi-setup-detector';
  Application.CreateForm(TresultForm1, resultForm1);
  Application.CreateForm(TFNewDepDlg, FNewDepDlg);
  Application.CreateForm(TFNewPropDlg, FNewPropDlg);
  Application.CreateForm(TFOSDConfigdlg, FOSDConfigdlg);
  Application.CreateForm(TFChooseInstallerDlg, FChooseInstallerDlg);
  Application.CreateForm(TMyMessageDlg, MyMessageDlg);
  Application.Run;
end.
