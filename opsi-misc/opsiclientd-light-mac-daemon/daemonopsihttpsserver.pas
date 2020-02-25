unit DaemonOpsiHTTPSServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp, OpsiHTTPSListeningThread, osLog;

type

  { TOpsiHTTPSServerDaemon }

  TOpsiHTTPSServerDaemon = class(TDaemon)
    procedure ServerStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure ServerStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    FListeningThread: TOpsiHTTPSListeningThread;
    //FLogInfo: TLogInfo;
    procedure Log(aMessage: string; aLevelofLine:integer; aSourceOfLog:string);
  public

  end;

var
  OpsiHTTPSServerDaemon: TOpsiHTTPSServerDaemon;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TOpsiHTTPSServerDaemon)
end;

{$R *.lfm}

{ TOpsiHTTPSServerDaemon }

procedure TOpsiHTTPSServerDaemon.ServerStart(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  OK := True;
  LogDatei:= TLogInfo.Create;
  //LogDatei.StandardLogPath:= '/tmp/';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  LogDatei.CreateTheLogfile('opsiclientd.log',false); //('/var/log/opsi-client-agent/opsiclientd/opsiclientd.log',false);
  LogDatei.LogLevel:= 7;
  LogDatei.Log('Logging startet', LLNotice);
  //LogDatei.initiate();
  FListeningThread := TOpsiHTTPSListeningThread.Create;
  FListeningThread.LogData.OnPassLog:=@Log;
end;

procedure TOpsiHTTPSServerDaemon.Log(aMessage: string; aLevelofLine:integer; aSourceOfLog:string);
begin
  LogDatei.AktProduktId:=aSourceOfLog;
  LogDatei.Log(aMessage,aLevelofLine);
end;

procedure TOpsiHTTPSServerDaemon.ServerStop(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  if FListeningThread <> nil then
     FListeningThread.Terminate;
  FlisteningThread.WaitFor;
  LogDatei.Close;
  FreeAndNil(LogDatei);
  OK := true;
end;


initialization
  RegisterDaemon;
end.

