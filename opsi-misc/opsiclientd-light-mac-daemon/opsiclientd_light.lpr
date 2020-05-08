Program opsiclientd_light;

Uses
{$IFDEF UNIX}//{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}//{$ENDIF}
  DaemonApp, lazdaemonapp, DaemonMapperOpsiClientd, DaemonOpsiHTTPSServer
  { add your units here };

{$R *.res}

begin
  Application.Title:='opsiclientd-light';
  Application.Initialize;
  Application.Run;
end.
