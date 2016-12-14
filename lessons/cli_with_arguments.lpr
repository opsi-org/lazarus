program cli_with_arguments;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
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

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  sum1, sum2 : integer;
  optionlist : TStringlist;
begin
  // quick check parameters
  optionlist := TStringlist.Create;
  optionlist.Add('help');
  optionlist.Add('summant1:');
  optionlist.Add('summant2:');
  ErrorMsg:=CheckOptions('',optionlist);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption( 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  if HasOption( 'summant1') then
  begin
    sum1 := strToint(GetOptionValue('summant1'));
    sum2 := strToint(GetOptionValue('summant2'));
    writeln(sum1+sum2);
    Terminate;
    Exit;
  end;

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
  { add your help code here }
  filename := ExtractFileName(ExeName);
  writeln('Usage: ', filename, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

