unit notifier_base;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  LazFileUtils,
  oslog,
  IdTCPClient,
  IdTCPServer,
  Variants,
  fileinfo,
  winpeimagereader,
  notifierguicontrol,
  notifier_json;

type

  Tmythread = class(TThread)
  private
    myMessage: string;
    procedure messageToMainThread;
    procedure messageFromMainThread;
  public
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean);
  end;



procedure Main;

var
  stopped: boolean;
  mythread: Tmythread;
  myJsonAnswer: string = '';


implementation

uses
  notifierdatamodule;

var
  myTCPClient: TIdTCPClient;


constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

procedure TMyThread.messageToMainThread;
begin
  // pass recieved message to notifier gui control
  newMessageFromService(myMessage);
  //logdatei.log('From hm: '+myMessage, LLInfo);
end;

procedure TMyThread.messageFromMainThread;
begin
  // pass answer to thread to send by tcp
  if myJsonAnswer <> '' then
  begin
    myMessage := myJsonAnswer;
    logdatei.log('messageFromMainThread: ' + myMessage, LLDebug2);
    myJsonAnswer := '';
  end;
end;

procedure Tmythread.Execute;
var
  receiveline: string;
  i: integer;

begin
  if not Terminated then
  begin
    myTCPClient := TIdTCPClient.Create;
    myTCPClient.Port := myport;
    myTCPClient.Host := '127.0.0.1';
    myTCPClient.ReadTimeout := 500;
    repeat
      try
        myTCPClient.Connect;
      except
      end;
    until myTCPClient.Connected;
    i := 1;
    while (not Terminated) do
    begin
      myMessage := '';
      myMessage := myTCPClient.Socket.ReadLn();
      if myMessage <> '' then
      begin
        logdatei.log('Received: ' + mymessage, LLDebug2);
        Synchronize(@messageToMainThread);
        myMessage := '';
      end;
      Synchronize(@messageFromMainThread);
      if myMessage <> '' then
      begin
        myTCPClient.Socket.WriteLn(myMessage);
        logdatei.log('Sended: ' + mymessage, LLDebug2);
      end;
    end;
    stopped := True;
    myTCPClient.Disconnect;
    myTCPClient.Free;
  end
  else
  begin
    stopped := True;
    myTCPClient.Disconnect;
    myTCPClient.Free;
  end;
end;



procedure Main;
begin
  stopped := False;
  DataModule1.createNform;
  openSkinIni(myconfigfile);
  if myport > 0 then
  begin
    mythread := Tmythread.Create(False);
    mythread.WaitFor;

  end;
end;


end.

