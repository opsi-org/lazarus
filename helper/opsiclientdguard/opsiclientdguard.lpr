Program opsiclientdguard;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  FileUtil,
  ExtCtrls,
  DaemonApp,
  lazdaemonapp,
  ocdgmapperunit1,
  ocdgu1,
  ServiceManager,
  JwaWinSvc,
eventlog;

begin
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
