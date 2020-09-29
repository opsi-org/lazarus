program opsi_script_pilot;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  osversioninfo,
  osencoding,
  oslog,
  SysUtils,
  strutils,
  lazFileUtils,
  custapp,
  Process,
  oscheck_gui_startable,
  pilot_helper;

 type

  { Topsiscript }

  Topsiscript = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    //procedure WriteHelp; virtual;
  end;

var
  Application: Topsiscript;

{ Topsiscript }
constructor Topsiscript.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Topsiscript.Destroy;
begin
  inherited Destroy;
end;

procedure Topsiscript.DoRun;
var
  myexitcode : longint;
begin
  myexitcode := pilot_helper.main();
  Application.Terminate(myexitcode);
end;


//{$R *.res}

{$R *.res}

begin
  Application:=Topsiscript.Create(nil);
  Application.Title:='opsi-script-pilot';
  Application.Initialize;
  Application.DoRun;
  Application.Free;
end.





