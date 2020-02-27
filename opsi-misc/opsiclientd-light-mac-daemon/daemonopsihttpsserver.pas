unit DaemonOpsiHTTPSServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp, OpsiHTTPSListeningThread, osLog, IniFiles;

type

  { TOpsiHTTPSServerDaemon }

  TOpsiHTTPSServerDaemon = class(TDaemon)
    procedure ServerStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure ServerStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    ListeningThread: TOpsiHTTPSListeningThread;
    SSLUsername: string;
    SSLPassword: string;
    //FLogInfo: TLogInfo;
    procedure Log(aMessage: string; aLevelofLine:integer; aSourceOfLog:string);
    procedure ReadClientdConf;
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
  LogDatei.LogProduktId:=True;
  LogDatei.CreateTheLogfile('opsiclientd.log',false); //('/var/log/opsi-client-agent/opsiclientd/opsiclientd.log',false);
  LogDatei.LogLevel:= 5;
  LogDatei.AktProduktId:='opsiclientd-mac';
  LogDatei.Log('Daemon startet', LLNotice);
  //LogDatei.initiate();
  ReadClientdConf;
  LogDatei.Loglevel := 9;
  LogDatei.Log('read opsiclientd.conf', LLNotice);
  LogDatei.Log('SSL Username: '+ SSLUsername, LLDebug3);
  LogDatei.Log('SSL Password: '+ SSLPassword, LLDebug3);
  ListeningThread := TOpsiHTTPSListeningThread.Create(SSLUsername, SSLPassword);
  ListeningThread.LogData.OnPassLog:=@Log;

end;

procedure TOpsiHTTPSServerDaemon.Log(aMessage: string; aLevelofLine:integer; aSourceOfLog:string);
begin
  LogDatei.AktProduktId :=aSourceOfLog;
  LogDatei.Log(aMessage,aLevelofLine);
end;

procedure TOpsiHTTPSServerDaemon.ReadClientdConf;
var
  Clientdconf: TIniFile;
begin
  ClientdConf := TInifile.Create('/etc/opsi-client-agent/opsiclientd.conf');
  SSLUsername := ClientdConf.ReadString('global', 'host_id', '');//'vmmacdev1onmm1.uib.local';
  SSLPassword := ClientdConf.ReadString('global', 'opsi_host_key', '');//'aead8f8c57a92e14ac820bf8d3df1805'; //'linux123';
  LogDatei.LogLevel := StrToInt(ClientdConf.ReadString('global', 'log_level', ''));
  FreeAndNil(ClientdConf);
end;

procedure TOpsiHTTPSServerDaemon.ServerStop(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  if ListeningThread <> nil then
     ListeningThread.Terminate;
  //ListeningThread.WaitFor;
  LogDatei.AktProduktId:='opsiclientd-mac';
  LogDatei.Log('Daemon stopped', LLNotice);
  LogDatei.Close;
  FreeAndNil(LogDatei);
  OK := true;
end;


initialization
  RegisterDaemon;
end.

