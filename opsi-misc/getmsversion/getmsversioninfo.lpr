program getmsversioninfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp , windows
  { you can add units after this };

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


{$R manifest.rc}
{ TMyApplication }


function GetMSVersionString: string;
var
  vi: TOSVersionInfo;

begin
  Result := '';
  vi.dwOSVersionInfoSize := SizeOf(vi);
  if not GetVersionEx(vi) then
    raise Exception.Create('Fehler ' + IntToStr(GetLastError) +
      ' ("' + syserrormessage(GetLastError) + '")')
  else
    Result := intToStr(vi.dwMajorVersion) + '.' + intToStr(vi.dwMinorVersion);
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  writeln(GetMSVersionString);
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
  filename := ExtractFileName(paramstr(0));
  writeln(paramstr(0));
  writeln(filename);
  writeln('Prints the internal version of the Windows NT version to stdout');
  writeln('Usage:');
  writeln(filename+ ' [Options]');
  writeln('Options:');
  writeln(' --help -> write this help and exit');
  Terminate;
  halt(-1);
  Exit;
end;


var
  Application: TMyApplication;

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

