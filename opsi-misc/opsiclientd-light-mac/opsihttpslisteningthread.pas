unit OpsiHTTPSListeningThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpsiHTTPSAcceptingThread, blcksock, sockets;

type
  TPassMessage = procedure(AMsg: string) of object;

  { TOpsiHTTPSListeningThread }

  TOpsiHTTPSListeningThread = class(TThread)
  private
    ListenerSocket:TTCPBlockSocket;
    AcceptingThread: TOpsiHTTPSAcceptingThread;
    FormerAcceptingThread: TOpsiHTTPSAcceptingThread;
    PassMessage: TPassMessage;
    StatusMessage: string;
    procedure DisplayMessage;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
    property OnPassMessage: TPassMessage read PassMessage write PassMessage;
  end;


implementation

{ TOpsiHTTPSListeningThread }

procedure TOpsiHTTPSListeningThread.DisplayMessage;
begin
  if Assigned(PassMessage) then
    PassMessage(StatusMessage);
end;

constructor TOpsiHTTPSListeningThread.Create;
begin
  FreeOnTerminate := True;
  ListenerSocket := TTCPBlockSocket.create;
  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(true,10000);
  ListenerSocket.Bind('192.168.10.74','4441'); //192.168.10.70
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
  Synchronize(@DisplayMessage);
  FormerAcceptingThread := nil;
  repeat
    if not Terminated then
    begin
      if ListenerSocket.CanRead(1000) then
        begin
          ClientSocket:=ListenerSocket.accept;
          StatusMessage := 'Accepting...  New socket: ' + ClientSocket.ToString;
          Synchronize(@DisplayMessage);
          if ListenerSocket.LastError = 0 then
          begin
            AcceptingThread := TOpsiHTTPSAcceptingThread.Create(ClientSocket, FormerAcceptingThread);
            //with TOpsiHTTPSAcceptingThread.Create(ClientSocket, FormerAcceptingThread) do
            //with TOpsiHTTPSAcceptingThread.Create(ClientSocket) do
            StatusMessage := 'New AcceptingThread created. ThreadID: '
                + IntToStr(Int64(AcceptingThread.ThreadID));
            Synchronize(@DisplayMessage);
            FormerAcceptingThread := AcceptingThread;
          end;
        end;
     end;
  until Terminated;
  if Terminated then
  begin
    StatusMessage := 'Server stopped';
    Synchronize(@DisplayMessage);
  end;
end;

end.

