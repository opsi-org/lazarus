program opsi_icon_collector;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, IconCollector, Interfaces
  { you can add units after this };

type

  { TOpsiIconCollector }

  TOpsiIconCollector = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure ShowOpsiSetupScriptPaths(DepotPath:String);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TOpsiIconCollector }

procedure TOpsiIconCollector.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    ReadLn;//only for testing, remove in productive environment
    Terminate;
    Exit;
  end;

  { add your program here }
  WriteLn('Hello Opsi');
  ShowOpsiSetupScriptPaths('C:\Users\Jan\Test');

  // stop program loop
  ReadLn;//only for testing, remove in productive environment
  Terminate;
end;

procedure TOpsiIconCollector.ShowOpsiSetupScriptPaths(DepotPath: String);
var
  IconCollector :TIconCollector;
begin
  WriteLn('Creating IconCollector...');
  IconCollector := TIconCollector.Create(DepotPath);
  WriteLn('Done');
  IconCollector.ShowFilenames;
  IconCollector.GetPathToIcon('');
  //IconCollector.ExtractIconFromExe('C:\Users\Jan\Test\anydesk\AnyDesk.exe');
end;

constructor TOpsiIconCollector.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOpsiIconCollector.Destroy;
begin
  inherited Destroy;
end;

procedure TOpsiIconCollector.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExeName, ' -h');
end;

var
  Application: TOpsiIconCollector;

{$R *.res}

begin
  Application:=TOpsiIconCollector.Create(nil);
  Application.Title:='opsi-icon-collector';
  Application.Run;
  Application.Free;
end.

