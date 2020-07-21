// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: detlef oertel
//
// derivated from:
// fpc example cleandirs.lpi (C:\lazarus\examples\cleandir)
// http://wiki.lazarus.freepascal.org/Daemons_and_Services
// https://www.freepascal.org/~michael/articles/daemons/daemons.pdf
// https://www.freepascal.org/~michael/articles/services/services.pdf
// Thanks to Michaël Van Canneyt


unit svcservice;

{$mode objfpc}{$H+}

interface

{$R *.lfm}

uses
  Classes, SysUtils, daemonapp,
  eventlog,
  ServiceManager,
  JwaWinSvc,
  interfaces,
  oslog,
  osprocesses,
  fileinfo,
  winpeimagereader,
  LazFileUtils,
  LazUTF8;

type
  { TServiceThread }

  TServiceThread = Class(TThread)
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TServiceDaemon }

  TServiceDaemon = class(TDaemon)
    procedure DataModuleAfterInstall(Sender: TCustomDaemon);
    procedure ServiceDaemonCreate(Sender: TObject);
    procedure ServiceDaemonStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure ServiceDaemonStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    { private declarations }
    //FConfigFile : String;
    FThread : TServiceThread;
    //FLog : TEventLog;
    //procedure StartLog;
    procedure ThreadStopped(Sender: TObject);
  public
    { public declarations }
    Function Start : Boolean; override;
    Function Stop : Boolean; override;
    //Function Pause : Boolean; override;
    //Function Continue : Boolean; override;
    //Function Execute : Boolean; override;
    //Function ShutDown : Boolean; override;
    //Function Install : Boolean; override;
    //Function UnInstall: boolean; override;
  end; 
  

var
  ServiceDaemon: TServiceDaemon;
  preLogfileLogList: TStringList;

implementation

uses dateutils;
// Include windows messages for eventlog component.
{$ifdef mswindows}
{ $r fclel.res}
{$endif}

procedure RegisterDaemon; 
begin
  RegisterDaemonClass(TServiceDaemon)
end; 

{ TServiceDaemon }

procedure StartLog;
var
  i : integer;
  FileVerInfo : TFileVersionInfo;
  myVersion : string;
  logfilename : string;
begin
  try
    //Result := True;
    logdatei := TLogInfo.Create;
    logfilename := 'opsiclientdguard.log';
    LogDatei.WritePartLog := False;
    LogDatei.WriteErrFile:= False;
    LogDatei.WriteHistFile:= False;
    logdatei.CreateTheLogfile(logfilename, False);
    logdatei.LogLevel := 7;
    for i := 0 to preLogfileLogList.Count-1 do
      logdatei.log(preLogfileLogList.Strings[i], LLessential);
    FileVerInfo := TFileVersionInfo.Create(nil);

    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
    logdatei.log('opsiclientdguard: version: ' + myVersion, LLessential);
  finally
    FileVerInfo.Free;
    preLogfileLogList.Free;
  end;
end;

procedure TServiceDaemon.DataModuleAfterInstall(Sender: TCustomDaemon);
begin
  Application.Log(etInfo, 'AfterInstall');
end;

procedure TServiceDaemon.ServiceDaemonCreate(Sender: TObject);
begin
  Application.Log(etInfo, 'ServiceDaemonCreate');

end;

function TServiceDaemon.Start: Boolean;
begin
Result:=inherited Start;
//AWriteln('Daemon Start',Result);
Application.Log(etDebug, 'Daemon.Start: ' + BoolToStr(result));
FThread:=TServiceThread.Create(True);
FThread.OnTerminate:=@ThreadStopped;
FThread.FreeOnTerminate:=False;
FThread.Resume;
end;

function TServiceDaemon.Stop: Boolean;
begin
Result:=inherited Stop;
Application.Log(etDebug, 'Daemon.Stop: ' + BoolToStr(result));
//AWriteln('Daemon Stop: ',Result);
FThread.Terminate;
LogDatei.Close;
end;

procedure TServiceDaemon.ThreadStopped(Sender : TObject);

begin
  FThread:=Nil;
end;

procedure TServiceDaemon.ServiceDaemonStart(Sender: TCustomDaemon;
  var OK: Boolean);
  
begin
  //StartLog;
  { Start should return immediatly. Therefore, a thread is started if
    one is not yet running. Usually, here one opens a TCP/IP socket
    and starts listening }
  Application.Log(etDebug, 'ServiceDaemonStart');
  OK:=(FThread=Nil);// and (FConfigFile<>'');
  If OK then
    FThread:=TServiceThread.Create(false);
end;

procedure TServiceDaemon.ServiceDaemonStop(Sender: TCustomDaemon;
  var OK: Boolean);
  
Var
  I : Integer;
  
begin
  Application.Log(etDebug, 'ServiceDaemonStop');
  If Assigned(FThread) then
    begin
    FThread.Terminate;
    I:=0;
    // Wait at most 5 seconds.
    While (FThread<>Nil) and (I<50) do
      begin
      Sleep(100);
      ReportStatus;
      end;
    // Let the thread die silently.
    If (FThread<>Nil) then
      FThread.OnTerminate:=Nil;
    end;
  OK:=FThread=Nil;
end;

{ TServiceThread }
function IsServiceRunning(ServiceName: string): boolean;
  {description Checks if a Windows service is running}
var
  Services: TServiceManager;
  ServiceStatus: TServiceStatus;
begin
  //Check for existing services
  //equivalent to sc query <servicename>
  Services := TServiceManager.Create(nil);
  try
    try
      Services.Access := SC_MANAGER_CONNECT; //Note typo in property.
      //We don't need more access permissions than this; by default
      //the servicemanager is trying to get all access
      Services.Connect; //Now connect with requested access level
      Services.GetServiceStatus(ServiceName, ServiceStatus);
      Result := (ServiceStatus.dwCurrentState = SERVICE_RUNNING);
      Services.Disconnect;
    except
      on E: EServiceManager do
      begin
        // A missing service might throw a missing handle exception? No?
        if Assigned(LogDatei) then
         LogDatei.log('Error getting service information for ' + ServiceName +
                   '. Technical details: ' + E.ClassName + '/' + E.Message, LLWarning);
        Result := False;
        //raise; //rethrow original exception
      end;
      on E: Exception do
      begin
       if Assigned(LogDatei) then
         LogDatei.log('Error getting service information for ' + ServiceName +
                  '. Technical details: ' + E.ClassName + '/' + E.Message, LLWarning);
        Result := False;
        //raise; //rethrow original exception
      end;
    end;
  finally
    Services.Free;
  end;
end;

function startService(ServiceName: string): boolean;
  {description start a Windows service }
var
  Services: TServiceManager;
  ServiceStatus: TServiceStatus;
begin
  if Assigned(LogDatei) then
          LogDatei.log( 'Try to start service: '+ServiceName,LLnotice);
  //Check for existing services
  //equivalent to sc query <servicename>
  Services := TServiceManager.Create(nil);
  try
    try
      Services.Access := SERVICE_START; //Note typo in property.
      //We don't need more access permissions than this; by default
      //the servicemanager is trying to get all access
      Services.Connect; //Now connect with requested access level
      Services.StartService(ServiceName,nil);
      Result := True;
      //Services.GetServiceStatus(ServiceName, ServiceStatus);
      //Result := (ServiceStatus.dwCurrentState = SERVICE_RUNNING);
      Services.Disconnect;
    except
      on E: EServiceManager do
      begin
        // A missing service might throw a missing handle exception? No?
        {LogOutput('Error getting service information for ' + ServiceName +
                   '. Technical details: ' + E.ClassName + '/' + E.Message); }
        Application.Log(etDebug, 'Servicemanager Exception: '+e.Message);
        if Assigned(LogDatei) then
          LogDatei.log( 'Servicemanager Exception: '+e.Message,LLwarning);
        Result := False;
        //raise; //rethrow original exception
      end;
      on E: Exception do
      begin
       {LogOutput('Error getting service information for ' + ServiceName +
                  '. Technical details: ' + E.ClassName + '/' + E.Message); }
        Application.Log(etDebug, 'Exception: '+e.Message);
        if Assigned(LogDatei) then
        LogDatei.log( 'Exception: '+e.Message,LLwarning);
        Result := False;
        //raise; //rethrow original exception
      end;
    end;
  finally
    Services.Free;
  end;
end;

procedure TServiceThread.Execute;
var
  i: int64;
  sleeptime : integer;
  log : boolean;
begin
  i := 0;
  sleeptime := 5000;
  log := true;
  Application.Log(etDebug, 'Thread.Execute');
  try
    StartLog;
    repeat
      Sleep(sleeptime); //milliseconds
      if i < 60 then
      begin
        inc(i);
        //Application.Log(etDebug, 'Thread.Loop ' + Format('Tick :%d', [i]));
        LogDatei.log('Thread.Loop ' + Format('Tick :%d', [i]),LLDebug);
      end
      else
      begin
        log := false;
        sleeptime := 30000;
      end;
      if IsServiceRunning('opsiclientd') then
      begin
        //if log then Application.Log(etDebug, 'opsiclientd is running');
        if ProcessIsRunning('opsiclientd.exe') then
        begin
          LogDatei.log('opsclientd runs via API and opsiclientd.exe process in processlist',LLInfo);
        end
        else
        begin
          LogDatei.log('opsclientd runs via API, but opsiclientd.exe process not in processlist',LLwarning);
        end;
      end
      else
      begin
        if ProcessIsRunning('opsiclientd.exe') then
        begin
        //Application.Log(etDebug, 'opsiclientd is not running');
        LogDatei.log('No opsclientd via API, but opsiclientd.exe process in processlist',LLwarning);
        end
        else
        begin
          LogDatei.log('No opsclientd via API and no opsiclientd.exe process in processlist',LLwarning);
        if startService('opsiclientd') then
        begin
          Application.Log(etDebug, 'opsiclientd is started');
          LogDatei.log('opsiclientd is started',LLInfo);
        end
        else
        begin
          Application.Log(etDebug, 'opsiclientd start failed');
          LogDatei.log('opsiclientd start failed',LLError);
        end;
        end;
      end;
    until Terminated;
  finally
    Application.Log(etDebug, 'Thread.LoopStopped');
  end;
end;

destructor TServiceThread.Destroy;
begin
  Application.Log(etDebug, 'Thread.Destroy');
  inherited Destroy;
end;

initialization
  RegisterDaemon;
  preLogfileLogList:= TStringList.Create;
end.

