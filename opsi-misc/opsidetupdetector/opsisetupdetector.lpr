program opsisetupdetector;

{$mode delphi}{$H+}

//{$APPTYPE CONSOLE}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Interfaces,
  Forms,
  resultform,
  //VersionInfoX,
  //help,
  printers, fileinfo, winpeimagereader, lcltranslator, runtimetypeinfocontrols,
  osdanalyze, osdhelper, osdbasedata;


{$R *.res}
//{$R manifest.rc}


begin
  Application.Initialize;
  Application.Title:='opsi setup detector';
  Application.CreateForm(TresultForm1, resultForm1);
  //Application.CreateForm(TFormHelp, FormHelp);
  Application.Run;
  //main;
end.

