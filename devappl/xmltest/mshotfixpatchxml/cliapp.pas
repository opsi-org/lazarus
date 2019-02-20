unit cliapp;

{$mode objfpc}{$H+}



interface

uses
  Classes, SysUtils,custapp,xmlpatch;

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

var
  Application: TMyApplication;

implementation





{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  optionlist : TStringlist;
  logpath, inifilename, packagename : string;
begin
  logpath := copy(GetUserDir,1,Length(getUserDir)-1);      // default
  inifilename :='';
  packagename := '';
  // quick check parameters
  optionlist := TStringlist.Create;
  optionlist.Add('help');
  optionlist.Add('inidatei:');
  optionlist.Add('package:');
  optionlist.Add('logpath:');
  ErrorMsg:=CheckOptions('hipl',optionlist);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    WriteHelp;
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('i', 'inidatei') then
  begin
    inifilename := GetOptionValue('i','inidatei');
  end
  else
  begin
    writeln('Error: missing parameter for ini file');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('p', 'package') then
  begin
    packagename := GetOptionValue('p','package');
  end
  else
  begin
    writeln('Error: missing parameter package');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('l', 'logpath') then
  begin
    logpath := GetOptionValue('l','logpath');
  end;
  initlog(logpath);
  openAndPatch(inifilename,packagename);

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
var
  filename : string;
begin
  filename := ExtractFileName(ExeName);
  writeln('Usage: ', filename);
  writeln('Options:');
  writeln('-h, --help : print this help');
  writeln('-i, --inidatei : path to ini file * required');
  writeln('-p, --package : path to package.xml * required');
  writeln('-l, --logpath : set log path * optional, default userdir');
end;


end.

