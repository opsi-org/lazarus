unit notifier_base;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  LazFileUtils,
  oslog,
  //IdTCPClient,
  Variants,
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




constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

procedure TMyThread.messageToMainThread;
begin
  // pass recieved message to notifier gui control
  newMessageFromService(myMessage);
  logdatei.log('From hm: '+myMessage, LLInfo);
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
  end
  else logdatei.log('Nothing in messageFromMainThread.', LLDebug2);
  DataModule1.ProcessMess;
end;

(*
procedure Tmythread.Execute;
//var
//  receiveline: string;
//  i: integer;

begin
  logdatei.log('Starting TCP-Thread: ' +TimeToStr(now), LLDebug2);
  if not Terminated then
  begin
    myTCPClient := TIdTCPClient.Create;
    myTCPClient.Port := myport;
    myTCPClient.Host := '127.0.0.1';
    myTCPClient.ReadTimeout := 100;
    repeat
      try
        myTCPClient.Connect;
      except
      end;
    until myTCPClient.Connected;
    //i := 1;
    myMessage2 := '';
    logdatei.log('TCP-Thread connected, starting .loop: ' +TimeToStr(now), LLDebug2);
    while (not Terminated) and myTCPClient.Connected do
    begin
      myMessage := '';
      myMessage := myTCPClient.Socket.ReadLn();
      if myMessage <> '' then
      begin
        logdatei.log('Received: ' + mymessage, LLDebug2);
        Synchronize(@messageToMainThread);
        //DataModule1.ProcessMess;
        logdatei.log('After Received: ' + mymessage, LLDebug2);
        myMessage := '';
      end;
      Synchronize(@messageFromMainThread);
      if myMessage <> '' then
      begin
        myTCPClient.Socket.WriteLn(myMessage);
        logdatei.log('Sended: ' + mymessage, LLDebug2);
      end;
      logdatei.log('tcploop :' +TimeToStr(now), LLDebug2);
      //sleep(1000);
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
*)

procedure Tmythread.Execute;
//var
//  receiveline: string;
//  i: integer;
var
  myTCPClient: TTCPBlockSocket;

begin
  logdatei.log('Starting TCP-Thread: ' +TimeToStr(now), LLDebug2);
  if not Terminated then
  begin
    myTCPClient := TTCPBlockSocket.Create;
    myTCPClient.Bind ('127.0.0.1', myport);
    myTCPClient.Listen;
    if myTCPClient.canread (timeout) then
    begin
      a :=  TTCPBlockSocket.Create;
      a.socket := s.Accept;
    end;
    //myTCPClient.Port := myport;
    //myTCPClient.Host := '127.0.0.1';
    //myTCPClient.ReadTimeout := 100;
    repeat
      try
        myTCPClient.Connect;
      except
      end;
    until myTCPClient.Connected;
    //i := 1;
    myMessage2 := '';
    logdatei.log('TCP-Thread connected, starting .loop: ' +TimeToStr(now), LLDebug2);
    while (not Terminated) and myTCPClient.Connected do
    begin
      myMessage := '';
      myMessage := myTCPClient.Socket.ReadLn();
      if myMessage <> '' then
      begin
        logdatei.log('Received: ' + mymessage, LLDebug2);
        Synchronize(@messageToMainThread);
        //DataModule1.ProcessMess;
        logdatei.log('After Received: ' + mymessage, LLDebug2);
        myMessage := '';
      end;
      Synchronize(@messageFromMainThread);
      if myMessage <> '' then
      begin
        myTCPClient.Socket.WriteLn(myMessage);
        logdatei.log('Sended: ' + mymessage, LLDebug2);
      end;
      logdatei.log('tcploop :' +TimeToStr(now), LLDebug2);
      //sleep(1000);
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
s := TTCPBlockSocket.Create;
s.Bind ('0.0.0.0', port);
s.Listen;
if s.canread (timeout) then
begin
  a :=  TTCPBlockSocket.Create;
  a.socket := s.Accept;
end;


procedure Main;
begin
  stopped := False;
  DataModule1.createNform;
  openSkinIni(myconfigfile);
  DataModule1.ProcessMess;
  if myport > 0 then
  begin
    mythread := Tmythread.Create(False);
    mythread.WaitFor;

  end;
end;


end.

