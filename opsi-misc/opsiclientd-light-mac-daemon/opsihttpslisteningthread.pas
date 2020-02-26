unit OpsiHTTPSListeningThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpsiHTTPSAcceptingThread, blcksock, sockets, OpsiClientdLog, IniFiles;

type

  { TOpsiHTTPSListeningThread }

  TOpsiHTTPSListeningThread = class(TThread)
  private
    ListenerSocket:TTCPBlockSocket;
    AcceptingThread: TOpsiHTTPSAcceptingThread;
    FormerAcceptingThread: TOpsiHTTPSAcceptingThread;
    AcceptingThreadNumber: integer;
    SSLUsername: string;
    SSLPassword: string;
  public
    LogData: TLogData;
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
  end;


implementation

{ TOpsiHTTPSListeningThread }

constructor TOpsiHTTPSListeningThread.Create;
var
  ClientdConf: TIniFile;
begin
  FreeOnTerminate := True;
  LogData := TLogData.Create;
  ClientdConf := TIniFile.Create();
  ListenerSocket := TTCPBlockSocket.create;
  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(true,10000);
  ListenerSocket.Bind('0.0.0.0','4441'); //192.168.10.74
  ListenerSocket.Listen;
  inherited Create(false);
end;

destructor TOpsiHTTPSListeningThread.Destroy;
begin
  ListenerSocket.free;
  AcceptingThread.Terminate;
  AcceptingThread.WaitFor;
  FreeAndNil(AcceptingThread);
  FreeAndNil(LogData);
  inherited Destroy;
end;

procedure TOpsiHTTPSListeningThread.Execute;
var
  ClientSocket:TSocket;
begin
  LogData.FSourceOfLog:='opsiclientd-mac listening thread';
  LogData.FLogMessage := 'Server started (OpsiHTTPSListeningThread.pas)';
  LogData.FLevelofLine:= 5;
  Synchronize(@LogData.SendLog);
  FormerAcceptingThread := nil;
  AcceptingThreadNumber := 0;
  repeat
    if not Terminated then
    begin
      if ListenerSocket.CanRead(1000) then
        begin
          ClientSocket:=ListenerSocket.accept;
          LogData.FLogMessage := 'Accepting...  New socket: ' + ClientSocket.ToString + ' (OpsiHTTPSListeningThread.pas)' ;
          Synchronize(@LogData.SendLog);
          if ListenerSocket.LastError = 0 then
          begin
            inc(AcceptingThreadNumber);
            AcceptingThread := TOpsiHTTPSAcceptingThread.Create(ClientSocket, FormerAcceptingThread);
            AcceptingThread.SSLPassword:=SSLPassword;
            AcceptingThread.LogData.OnPassLog:= self.LogData.OnPassLog;
            //with TOpsiHTTPSAcceptingThread.Create(ClientSocket, FormerAcceptingThread) do
            //with TOpsiHTTPSAcceptingThread.Create(ClientSocket) do
            LogData.FLogMessage := 'New AcceptingThread created. ' + ' ThreadNumber:'+ IntToStr(AcceptingThreadNumber);
            //IntToStr(Integer(AcceptingThread.ThreadID))
            LogData.FLevelofLine:= 5;
            Synchronize(@LogData.SendLog);
            FormerAcceptingThread := AcceptingThread;
          end;
        end;
     end;
  until Terminated;
  if Terminated then
  begin
    LogData.FLogMessage := 'Server stopped (OpsiHTTPSListeningThread.pas)';
    LogData.FLevelofLine:= 5;
    Synchronize(@LogData.SendLog)
  end;
end;

end.

