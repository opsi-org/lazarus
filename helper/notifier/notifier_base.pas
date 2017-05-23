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
    myMessage2: string;
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
  myJsonAnswer2: string = '';


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
    myMessage2 := myJsonAnswer2;
    logdatei.log('messageFromMainThread: ' + myMessage, LLDebug2);
    myJsonAnswer := '';
    myJsonAnswer2 := '';
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
    myMessage2 := '';
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
      (*
      else
      begin
        if myMessage2 <> '' then
        begin
          myTCPClient.Socket.WriteLn(myMessage2);
          logdatei.log('Sended: ' + mymessage2, LLDebug2);
          myMessage2 := '';
        end;
      end;
      *)
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

