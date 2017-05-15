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
  notifierguicontrol;

type

  Tmythread = class(TThread)
  public
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean);
  end;


(*
type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;
*)
procedure Main;
(*
var
  myport: integer;
  myevent: string;
  myconfigpath, myconfigfile: string;
  myexepath: string;
  myVersion: string;
  *)


implementation

uses
  notifierdatamodule;

var
  myTCPClient: TIdTCPClient;
  myTCPServer: TIdTCPServer;
  stopped: boolean;
  mythread: Tmythread;
// myApplication: TMyApplication;
//Application: TMyApplication;

  (*
  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  var
    filename: string;
  begin
    { add your help code here }
    filename := ExtractFileName(ExeName);
    writeln('This is ', filename, ' version: ' + myVersion + ' (c) uib gmbh, AGPLv3');
    writeln('Usage: ', filename, ' Options');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    writeln(' -h -> write this help and exit');
    writeln(' --port=<port> -> tcp port for communication with opsiclientd; required');
    writeln(' -p <port> -> tcp port for communication with opsiclientd; required');
    writeln(' --showconfigfile=<path> -> relative path to config file; required');
    writeln(' -s <path> -> relative path to config file; required');
    writeln(' --inputevent=<event> -> running event; required');
    writeln(' -i <event> -> running event; required');
  end;
*)



constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

procedure Tmythread.Execute;
var
  receiveline: string;
  i: integer;

begin
  if not Terminated then
  begin
    (*
    myTCPServer:=TIdTCPServer.Create;
    myTCPServer.Bindings.Clear;
     myTCPServer.on
    with myTCPServer.Bindings.Add do
    Begin
      IP := '127.0.0.1';
      Port := myport;
    end;
    TCPServer.Active := True;
  end;
  *)
    //myFreeze := TIdAntiFreeze.Create;
    //myFreeze.Active:=true;
    myTCPClient := TIdTCPClient.Create;
    myTCPClient.Port := myport;
    myTCPClient.Host := '127.0.0.1';
    myTCPClient.ReadTimeout := 1000;
    myTCPClient.Socket.;
    //myTCPClient.OnStatus:=;

    //Application.ProcessMessages;
    repeat
      try
        myTCPClient.Connect;
      except
      end;
    until myTCPClient.Connected;
    i := 1;
    while (not Terminated) and (i < 160) do
    begin
      ;
      //Application.ProcessMessages;
      receiveline := myTCPClient.Socket.ReadLn();
      logdatei.log(receiveline, LLInfo);
      logdatei.log(IntToStr(i) +
        ' --------------------------------------------------', LLInfo);
      Inc(i);
    end;
  end
  else
    stopped := True;
end;



procedure Main;
begin
  stopped := False;
  DataModule1.createNform;
  openSkinIni(myconfigfile);
  if myport > 0 then
  begin
    mythread := Tmythread.Create(False);

    while not stopped do
    begin
      Sleep(100);
    end;
  end;
end;


end.

