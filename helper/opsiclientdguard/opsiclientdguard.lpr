Program opsiclientdguard;

Uses
{$DEFINE svcdebug}
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  FileUtil,
  ExtCtrls,
  daemonapp,
  lazdaemonapp,
  ocdgu1, ocdgmapperunit1,
  ServiceManager,
  JwaWinSvc,
//HeapTrc,
eventlog;


{$R *.res}

begin
  Application.Title:='Daemon application';
  //RegisterDaemonClass(TTheDaemon);
  //RegisterDaemonMapper(TTheDaemonMapper);
  //RegisterDaemonApplicationClass(TCustomDaemonApplication);
 // heaptrc.SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.heap'));
  with Application do
  begin
    Title := 'Daemon Application';
    EventLog.LogType := ltFile;
    EventLog.DefaultEventType := etDebug;
    EventLog.AppendContent := true;
    EventLog.FileName := ChangeFileExt(ParamStr(0), '.log');
    Initialize;
    Run;
  end;
end.
