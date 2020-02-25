Program opsiclientd_mac;

Uses
{$IFDEF UNIX}//{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}//{$ENDIF}
  DaemonApp, lazdaemonapp, DaemonMapperOpsiClientd, DaemonOpsiHTTPSServer
  { add your units here };

begin
  Application.Initialize;
  Application.Run;
end.
