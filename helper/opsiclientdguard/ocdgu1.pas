unit ocdgu1;

{$mode objfpc}{$H+}
{$DEFINE svcdebug}
interface

uses

  Classes,
  SysUtils,
  FileUtil,
  ExtCtrls,
  DaemonApp,
  ServiceManager,
  JwaWinSvc,
  interfaces,
 //   HeapTrc,
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF} Cmem,{$ENDIF}
   EventLog;

const
  ServiceToTest = 'opsiclientd';

type
  TTheThread = class(TDaemonThread)
    procedure Execute; override;
    destructor Destroy; override;
  end;
  { TTheDaemon }

  TTheDaemon = class(TDaemon)
    procedure DataModuleContinue(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleControlCode(Sender: TCustomDaemon; ACode: DWord;
      var Handled: Boolean);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleExecute(Sender: TCustomDaemon);
    procedure DataModulePause(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleShutDown(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    FThread: TTheThread;
  public
    function Install: boolean; override;
    function UnInstall: boolean; override;
    (*
    function Start: boolean; override;
    function Stop: boolean; override;
    function Pause: boolean; override;
    function Continue: boolean; override;
    function Execute: boolean; override;
    function ShutDown: boolean; override;
    *)
  end;


  (*
  TTheDaemonMapper = class(TCustomDaemonMapper)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ToDoOnInstall(Sender: TObject);
    procedure ToDoOnRun(Sender: TObject);
    procedure ToDoOnUninstall(Sender: TObject);
    procedure ToDoOnDestroy(Sender: TObject);
  end;
*)

var
  TheDaemon: TTheDaemon;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TTheDaemon)
end;

{$R *.lfm}


function BoolToStr(AVal: Boolean): String;
begin
  if AVal = True then result := 'true' else result := 'false';
end;

procedure TTheThread.Execute;
var i: integer;
begin
  i := 0;
  Application.Log(etDebug, 'Thread.Execute');
  try
    repeat
      Sleep(1000); //milliseconds
      inc(i);
      Application.Log(etDebug, 'Thread.Loop ' + Format('Tick :%d', [i]));
    until Terminated;
  finally
    Application.Log(etDebug, 'Thread.LoopStopped');
  end;
end;

destructor TTheThread.Destroy;
begin
  Application.Log(etDebug, 'Thread.Destroy');
  inherited Destroy;
end;

{$REGION ' - Daemon - '}

procedure TTheDaemon.DataModuleContinue(Sender: TCustomDaemon; var OK: Boolean);
begin
  OK := inherited Continue;
  Application.Log(etDebug, 'Daemon.Continue: ' + BoolToStr(OK));
  FThread.Resume;
end;

procedure TTheDaemon.DataModuleControlCode(Sender: TCustomDaemon; ACode: DWord;
  var Handled: Boolean);
begin
  Application.Log(etDebug, 'Daemon.ControlCode: ');
  Handled := true;
end;

procedure TTheDaemon.DataModuleCreate(Sender: TObject);
begin
  Application.Log(etDebug, 'Daemon.Create: ');
end;

procedure TTheDaemon.DataModuleDestroy(Sender: TObject);
begin
  Application.Log(etDebug, 'Daemon.Destroy: ');
end;

procedure TTheDaemon.DataModuleExecute(Sender: TCustomDaemon);
var
  ok : boolean;
begin
  ok := inherited Execute;
  Application.Log(etDebug, 'Daemon.Execute: ' + BoolToStr(ok));
end;

procedure TTheDaemon.DataModulePause(Sender: TCustomDaemon; var OK: Boolean);
begin
  ok := inherited Pause;
  Application.Log(etDebug, 'Daemon.Pause: ' + BoolToStr(ok));
  FThread.Suspend;
end;

procedure TTheDaemon.DataModuleShutDown(Sender: TCustomDaemon);
var
  ok : boolean;
begin
  ok := inherited ShutDown;
  Application.Log(etDebug, 'Daemon.ShutDown: ' + BoolToStr(ok));
end;

procedure TTheDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
  ok := inherited Start;
  Application.Log(etDebug, 'Daemon.Start: ' + BoolToStr(ok));
  FThread := TTheThread.Create(Sender);
  FThread.FreeOnTerminate := true;
  FThread.Resume;
end;

procedure TTheDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
begin
  ok := inherited Stop;
  Application.Log(etDebug, 'Daemon.Stop: ' + BoolToStr(ok));
  FThread.Terminate;
  FThread.WaitFor;
  FThread := nil;
end;

function TTheDaemon.Install: boolean;
begin
  result := inherited Install;
  Application.Log(etDebug, 'Daemon.installed: ' + BoolToStr(result));
end;

function TTheDaemon.UnInstall: boolean;
begin
  result := inherited UnInstall;
  Application.Log(etDebug, 'Daemon.Uninstall: ' + BoolToStr(result));
end;

(*
function TTheDaemon.Start: boolean;
begin
end;

function TTheDaemon.Stop: boolean;
begin
end;

function TTheDaemon.Pause: boolean;
begin
end;

function TTheDaemon.Continue: boolean;
begin
end;

function TTheDaemon.Execute: boolean;
begin
end;

function TTheDaemon.ShutDown: boolean;
begin
end;
 *)

{ TTheDaemon }
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
      Services.Acces := SC_MANAGER_CONNECT; //Note typo in property.
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
        {LogOutput('Error getting service information for ' + ServiceName +
                   '. Technical details: ' + E.ClassName + '/' + E.Message); }
        Result := False;
        raise; //rethrow original exception
      end;
      on E: Exception do
      begin
       {LogOutput('Error getting service information for ' + ServiceName +
                  '. Technical details: ' + E.ClassName + '/' + E.Message); }
        Result := False;
        raise; //rethrow original exception
      end;
    end;
  finally
    Services.Free;
  end;
end;



(*
constructor TTheDaemonMapper.Create(AOwner: TComponent);
begin
  Application.Log(etDebug, 'DaemonMapper.Create');
  inherited Create(AOwner);
  with DaemonDefs.Add as TDaemonDef do
  begin
    DaemonClassName := 'TTheDaemon';
    Name := 'theDaemon';
    Description := 'The Daemon Exsample';
    DisplayName := 'The Daemon';
    RunArguments := '--run';
    Options := [doAllowStop,doAllowPause];
    Enabled := true;
    with WinBindings do
    begin
      StartType := stBoot;
      WaitHint := 0;
      IDTag := 0;
      ServiceType := stWin32;
      ErrorSeverity := esNormal;//esIgnore;
    end;
//    OnCreateInstance := ?;
    LogStatusReport := false;
  end;
  OnInstall := @Self.ToDoOnInstall;
  OnRun := @Self.ToDoOnRun;
  OnUnInstall := @Self.ToDoOnUninstall;
  OnDestroy := @Self.ToDoOnDestroy;
  Application.Log(etDebug, 'DaemonMapper.Createted');
end;

procedure TTheDaemonMapper.ToDoOnInstall(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Install');
end;

procedure TTheDaemonMapper.ToDoOnRun(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Run');
end;

procedure TTheDaemonMapper.ToDoOnUnInstall(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Uninstall');
end;

procedure TTheDaemonMapper.ToDoOnDestroy(Sender: TObject);
begin
  //doesn't comes here
  Application.Log(etDebug, 'DaemonMapper.Destroy');
end;
 *)

initialization
  RegisterDaemon;
end.

