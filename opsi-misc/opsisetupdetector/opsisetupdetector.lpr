program opsisetupdetector;

{$mode delphi}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, {$IFDEF OSDGUI}
  Forms,
  osdform,
  Printers,
  osdconfigdlg,
  osddlgnewdependency,
  osddlgnewproperty,
  osmessagedialog,
  osdanalyzegeneral,
  ChooseInstallerDlg, {$ELSE OSDGUI}
  custapp, {$ENDIF OSDGUI}
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
  osdmain,
  {$IFNDEF DARWIN}
  osd_lessmsi,
  {$ENDIF DARWIN}
  osdcontrolfile_io,
  osd_md_html_dlg,
  osdmeta,
  osdanalyze_by_die,
  osd_detect_it_easy,
  osd_jyt_convert;


(*
{$IFDEF WINDOWS}
{$R manifest.rc}
{$ENDIF WINDOWS}
*)


  {$R *.res}
  //{$R manifest.rc}




begin
  {$IFDEF OSDGUI}
  Application.Scaled := True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Title := 'opsi-setup-detector';
  {$IFDEF WINDOWS}
  // enable the possibility to share the Application window while a video conference
  // https://forum.lazarus.freepascal.org/index.php?topic=55417.0
  Application.MainFormOnTaskBar := True;
  {$ENDIF WINDOWS}
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
  Application.Title := 'opsi-setup-detector';
  Application.Initialize;
  Application.DoRun;
  Application.Free;
  {$ENDIF OSDGUI}

end.
