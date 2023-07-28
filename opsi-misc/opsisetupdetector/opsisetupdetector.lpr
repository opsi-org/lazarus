program opsisetupdetector;

{$mode delphi}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces,
  {$IFDEF OSDGUI}
  Forms,
  osdform,
  Printers,
    osdconfigdlg,
  osddlgnewdependency,
  osddlgnewproperty,
    osmessagedialog,
  osdanalyzegeneral,
  ChooseInstallerDlg,
  {$ELSE OSDGUI}
  custapp,
  {$ENDIF OSDGUI}
  fileinfo,
  winpeimagereader,
  lcltranslator,
  runtimetypeinfocontrols,
  osdanalyzewin,
  osdhelper,
  osdbasedata,
  osdcreate,
  oscheckbinarybitness,
  osencoding,
  osddatamod,
  osjson,
  oswebservice,
  oscrypt,
osdmain, osdcontrolfile_io;


(*
{$IFDEF WINDOWS}
{$R manifest.rc}
{$ENDIF WINDOWS}
*)


{$R *.res}
//{$R manifest.rc}




begin
  {$IFDEF OSDGUI}
  Application.Scaled:=True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Title:='opsi-setup-detector';
  Application.CreateForm(TresultForm1, resultForm1);
  Application.CreateForm(TFNewDepDlg, FNewDepDlg);
  Application.CreateForm(TFNewPropDlg, FNewPropDlg);
  Application.CreateForm(TFOSDConfigdlg, FOSDConfigdlg);
  Application.CreateForm(TFChooseInstallerDlg, FChooseInstallerDlg);
  Application.CreateForm(TMyMessageDlg, MyMessageDlg);
  Application.Run;
  {$ELSE OSDGUI}
  //Application.Scaled:=True;
Application := TOSD.Create(nil);
  Application.Title:='opsi-setup-detector';
Application.Initialize;
Application.DoRun;
Application.Free;
{$ENDIF OSDGUI}

end.
