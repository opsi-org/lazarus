unit OpsiHTTPSListeningThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpsiHTTPSAcceptingThread, blcksock, sockets, OpsiClientdLog;

type
  //TPassLog = procedure(AMsg: string) of object;

  { TOpsiHTTPSListeningThread }

  TOpsiHTTPSListeningThread = class(TThread)
  private
    ListenerSocket:TTCPBlockSocket;
    AcceptingThread: TOpsiHTTPSAcceptingThread;
    FormerAcceptingThread: TOpsiHTTPSAcceptingThread;
    //PassMessage: TPassLog;
    //StatusMessage: string;
    AcceptingThreadNumber: integer;
     //procedure SendStatusMessage;
  public
    LogData: TLogData;
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
    //property OnPassMessage: TPassLog read PassMessage write PassMessage;
  end;


implementation

{ TOpsiHTTPSListeningThread }

//procedure TOpsiHTTPSListeningThread.SendStatusMessage;
//begin
//  if Assigned(PassMessage) then
//    PassMessage(StatusMessage,5);
//end;

constructor TOpsiHTTPSListeningThread.Create;
begin
  FreeOnTerminate := True;
  LogData := TLogData.Create;
  ListenerSocket := TTCPBlockSocket.create;
  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(true,10000);
  //ListenerSocket.GetLocalSinIP;
  ListenerSocket.Bind('192.168.10.74','4441'); //192.168.10.74
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
  LogData.FLogMessage := 'Server started (OpsiHTTPSListeningThread.pas|68)';
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
          LogData.FLogMessage := 'Accepting...  New socket: ' + ClientSocket.ToString + ' (OpsiHTTPSListeningThread.pas|78)' ;
          Synchronize(@LogData.SendLog);
          if ListenerSocket.LastError = 0 then
          begin
            inc(AcceptingThreadNumber);
            AcceptingThread := TOpsiHTTPSAcceptingThread.Create(ClientSocket, FormerAcceptingThread);
            AcceptingThread.LogData.OnPassLog:= self.LogData.OnPassLog;
            //with TOpsiHTTPSAcceptingThread.Create(ClientSocket, FormerAcceptingThread) do
            //with TOpsiHTTPSAcceptingThread.Create(ClientSocket) do
            LogData.FLogMessage := 'New AcceptingThread created. ThreadID: '
                + IntToStr(Integer(AcceptingThread.ThreadID)) + ' ThreadNumber:'+ IntToStr(AcceptingThreadNumber);
            LogData.FLevelofLine:= 5;
            Synchronize(@LogData.SendLog);
            FormerAcceptingThread := AcceptingThread;
          end;
        end;
     end;
  until Terminated;
  if Terminated then
  begin
    LogData.FLogMessage := 'Server stopped (OpsiHTTPSListeningThread.pas|99)';
    LogData.FLevelofLine:= 5;
    Synchronize(@LogData.SendLog)
  end;
end;

end.

