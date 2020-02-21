unit OpsiHTTPSListeningThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpsiHTTPSAcceptingThread, blcksock, sockets;

type
  //TPassLog = procedure(AMsg: string) of object;

  { TOpsiHTTPSListeningThread }

  TOpsiHTTPSListeningThread = class(TThread)
  private
    ListenerSocket:TTCPBlockSocket;
    AcceptingThread: TOpsiHTTPSAcceptingThread;
    FormerAcceptingThread: TOpsiHTTPSAcceptingThread;
    PassMessage: TPassLog;
    StatusMessage: string;
    AcceptingThreadNumber: integer;
    procedure SendStatusMessage;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
    property OnPassMessage: TPassLog read PassMessage write PassMessage;
  end;


implementation

{ TOpsiHTTPSListeningThread }

procedure TOpsiHTTPSListeningThread.SendStatusMessage;
begin
  if Assigned(PassMessage) then
    PassMessage(StatusMessage,5);
end;

constructor TOpsiHTTPSListeningThread.Create;
begin
  FreeOnTerminate := True;
  ListenerSocket := TTCPBlockSocket.create;
  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(true,10000);
  //ListenerSocket.GetLocalSinIP;
  ListenerSocket.Bind('0.0.0.0','4441'); //192.168.10.74
  ListenerSocket.Listen;
  inherited Create(false);
end;

destructor TOpsiHTTPSListeningThread.Destroy;
begin
  ListenerSocket.free;
  FreeAndNil(AcceptingThread);
  inherited Destroy;
end;

procedure TOpsiHTTPSListeningThread.Execute;
var
  ClientSocket:TSocket;
begin
  StatusMessage := 'Server started';
  Synchronize(@SendStatusMessage);
  FormerAcceptingThread := nil;
  repeat
    if not Terminated then
    begin
      if ListenerSocket.CanRead(1000) then
        begin
          ClientSocket:=ListenerSocket.accept;
          StatusMessage := 'Accepting...  New socket: ' + ClientSocket.ToString;
          Synchronize(@SendStatusMessage);
          if ListenerSocket.LastError = 0 then
          begin
            AcceptingThread := TOpsiHTTPSAcceptingThread.Create(ClientSocket, FormerAcceptingThread);
            AcceptingThread.OnPassLog:= self.OnPassMessage;
            //with TOpsiHTTPSAcceptingThread.Create(ClientSocket, FormerAcceptingThread) do
            //with TOpsiHTTPSAcceptingThread.Create(ClientSocket) do
            StatusMessage := 'New AcceptingThread created. ThreadID: '
                + IntToStr(Integer(AcceptingThread.ThreadID) + AcceptingThreadNumber);
            Synchronize(@SendStatusMessage);
            FormerAcceptingThread := AcceptingThread;
          end;
        end;
     end;
  until Terminated;
  if Terminated then
  begin
    StatusMessage := 'Server stopped';
    Synchronize(@SendStatusMessage);
  end;
end;

end.

