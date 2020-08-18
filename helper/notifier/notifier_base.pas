unit notifier_base;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  //{$IFDEF UseCThreads}
  cthreads,
  //{$ENDIF}
  {$ENDIF}
  Classes,
  SysUtils,
  LazFileUtils,
  oslog,
  //IdTCPClient,
  Variants,
  winpeimagereader,
  notifierguicontrol,
  notifier_json,
  blcksock;// winsock, Synautil, ssl_openssl;

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


//var
//myTCPClient : TTCPBlockSocket;

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
  logdatei.log('From hm: ' + myMessage, LLInfo);
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
  else
    logdatei.log('Nothing in messageFromMainThread.', LLDebug2);
  DataModule1.ProcessMess;
end;


procedure Tmythread.Execute;
var
  myTCPClient: TTCPBlockSocket;
  Connected: boolean;
  //  receiveline: string;
  //  i: integer;

begin
  logdatei.log('Starting TCP-Thread: ' + TimeToStr(now), LLnotice);
  if not Terminated then
  begin
    myTCPClient := TTCPBlockSocket.Create; //TIdTCPClient.Create;
    //myTCPClient.Port := myport;
    //myTCPClient.Host := '127.0.0.1';
    //myTCPClient.ReadTimeout := 100;
    myTCPClient.ConnectionTimeout := 0; //uses default system value
    Connected := False;
    repeat
      try
        myTCPClient.Connect('127.0.0.1', IntToStr(myPort));
        Connected := True;
      except
        Connected := False;
      end;
    until Connected;
    //i := 1;
    myMessage2 := '';
    logdatei.log('TCP-Thread connected, starting .loop: ' + TimeToStr(now), LLDebug2);
    while (not Terminated) and Connected do
    begin
      myMessage := '';
      myMessage := myTCPClient.RecvString(100);
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
        myTCPClient.SendString(myMessage);
        logdatei.log('Sended: ' + mymessage, LLDebug2);
      end;
      logdatei.log('tcploop :' + TimeToStr(now), LLDebug2);
      //sleep(1000);
    end;
    stopped := True;
    myTCPClient.CloseSocket;
    myTCPClient.Free;
  end
  else
  begin
    stopped := True;
    myTCPClient.CloseSocket;
    myTCPClient.Free;
  end;
end;


procedure Main;
begin
  stopped := False;
  DataModule1.createNform;
  openSkinIni(myconfigfile);
  DataModule1.ProcessMess;
  if showtest then
  begin
    {show notifier 20 seconds (for tests only) }
    if Assigned(LogDatei) then LogDatei.log('show test',LLnotice);
    Sleep(20000);
    if Assigned(LogDatei) then LogDatei.log('shutdown after show test',LLnotice);
    shutdownNotifier;
  end
  else
  begin
    if myport > 0 then
    begin
      mythread := Tmythread.Create(False);
      mythread.WaitFor;
    end
    else
    if Assigned(LogDatei) then
      LogDatei.log('Critical Error: given port not > 0 : ' + IntToStr(myport), LLcritical);
  end;
end;


end.



