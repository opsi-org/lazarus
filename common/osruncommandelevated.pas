unit osRunCommandElevated;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, osLog;

type

  { TRunCommandElevated }

  TRunCommandElevated = class(TObject)
  private
    //FUser: string;
    FPassword: string;
    FSudo: boolean;
  public
    constructor Create(aPassword:string; aSudo:boolean);overload;
    {Set FPassword,  FSudo and creates LogDatei}
    destructor Destroy;override;

    function Run(aCommandLine: string): string;
    {Runs the command line as root (FSudo = false) or sudo (FSudo = true) using FPassword
    aCommandLine can be every correct shell command}

    property Password: string write FPassword;
    property Sudo: boolean read FSudo write FSudo;
  end;


implementation

{ TRunCommandElevated }

constructor TRunCommandElevated.Create(aPassword: string; aSudo: boolean);
begin
  inherited Create;
  FPassword := aPassword;
  FSudo := aSudo;
  //LogDatei := TLogInfo.Create;
end;

destructor TRunCommandElevated.Destroy;
begin
  //LogDatei.Free;
  inherited Destroy;
end;

function TRunCommandElevated.Run(aCommandLine: string): string;
var
  Output: string;
begin
  //aCommandLine := 'chown -c $USER /etc/apt/sources.list.d/opsi.list'; //for testing
  LogDatei.log('Shell command: ' + aCommandLine, LLInfo);
  case FSudo of
    True: aCommandLine := 'sudo -S ' + aCommandLine;
    False: aCommandLine := 'su -c ' + '"' + aCommandLine + '"'; //AnsiQuotedStr(aCommandLine, '"');
  end;
  LogDatei.log('Shell command: ' + aCommandLine, LLDebug);
  if RunCommand('/bin/sh', ['-c', 'echo ' + FPassword + ' | ' + aCommandLine],
    Output, [poWaitOnExit, poUsePipes]) then
  begin
    Result := Output;
    LogDatei.log('Shell command successful', LLInfo);
    LogDatei.log('Shell output: ' + Output, LLDebug);
  end
  else
  begin
    LogDatei.log('Error in RunCommand ' + Output, LLInfo);
    Result := 'Error in RunCommand: ' + Output;
  end;
end;


end.
