unit cliapp;

{$mode objfpc}{$H+}



interface

uses
  Classes,
  SysUtils,
  custapp,
  oslog,
  osversioninfo,
  osencoding,
  beautifyopsiscript,
  lazFileUtils
  ;

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

procedure initLogging;
var
  logfilename : string;
begin
  logdatei := TLogInfo.Create;
  logfilename := ExtractFileNameOnly(reencode(ParamStr(0), 'system'))+'-beautifier.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := 7;
  (*
  for i := 0 to preLogfileLogList.Count-1 do
    logdatei.log(preLogfileLogList.Strings[i], LLessential);
  preLogfileLogList.Free;
  *)
  logdatei.log('opsi-generic launcher version: ' + getversioninfo, LLessential);
  logdatei.log('Called as: ' + ExtractFileNameOnly(reencode(ParamStr(0), 'system')), LLessential);
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  optionlist : TStringlist;
  logpath, beautiparamsfilename, opsiscriptfile : string;
begin
  logpath := copy(GetUserDir,1,Length(getUserDir)-1);      // default
  beautiparamsfilename :='';
  opsiscriptfile := '';
  // quick check parameters
  optionlist := TStringlist.Create;
  optionlist.Add('help');
  optionlist.Add('beautiparams:');
  optionlist.Add('file:');
  optionlist.Add('logpath:');
  ErrorMsg:=CheckOptions('hbfl',optionlist);
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

  if HasOption('b', 'beautiparams') then
  begin
    beautiparamsfilename := GetOptionValue('b','beautiparams');
  end
  else
  begin
    writeln('Error: missing parameter for file beautiparams');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('f', 'file') then
  begin
    opsiscriptfile := GetOptionValue('f','file');
  end
  else
  begin
    writeln('Error: missing parameter file');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('l', 'logpath') then
  begin
    logpath := GetOptionValue('l','logpath');
  end;
  initLogging;
  writeln('file with beautifier params: ' + beautiparamsfilename);
  logdatei.log('file with beautifier params: ' + beautiparamsfilename, LLessential);
  logdatei.log('opsiscriptfile to beautify: ' + opsiscriptfile, LLessential);
  beautifyopsiscript.initialize(beautiparamsfilename,opsiscriptfile);

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
  writeln('-b, --beautiprams : path to beautiparams file * required');
  writeln('-f, --file : path to file.opsiscript * required');
  writeln('-l, --logpath : set log path * optional, default userdir');
end;


end.

