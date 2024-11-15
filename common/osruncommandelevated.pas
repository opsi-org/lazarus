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
    FShell: string;
    FShellOption: string;
  public
    constructor Create(aPassword: string; aSudo: boolean; aShell: string = '/bin/sh'; aShellOption: string = '-c');overload;
    {Set FPassword,  FSudo and creates LogDatei}
    destructor Destroy;override;

    function Run(aCommandLine: string; out Output:string; Confidential:boolean = false): boolean;
    {Runs the command line as root (FSudo = false) or sudo (FSudo = true) using FPassword
    aCommandLine can be every correct shell command}

    property Shell: string read FShell write FShell;
    Property ShellOption: string read FShellOption write FShellOption;
    property Password: string write FPassword;
    property Sudo: boolean read FSudo write FSudo;
  end;


implementation

{ TRunCommandElevated }

constructor TRunCommandElevated.Create(aPassword: string; aSudo: boolean; aShell: string = '/bin/sh'; aShellOption: string = '-c');
begin
  inherited Create;
  FPassword := aPassword; // AnsiQuotedStr(aPassword,'''');
  FSudo := aSudo;
  FShell := aShell;
  FShellOption := aShellOption;
  //LogDatei := TLogInfo.Create;
end;

destructor TRunCommandElevated.Destroy;
begin
  //LogDatei.Free;
  inherited Destroy;
end;

function TRunCommandElevated.Run(aCommandLine: string; out Output:string; Confidential:boolean = false): boolean;
begin
  //aCommandLine := 'chown -c $USER /etc/apt/sources.list.d/opsi.list'; //for testing
  LogDatei.log('Shell command: ' + aCommandLine, LLInfo);
  case FSudo of
    True: aCommandLine := 'sudo -S ' + aCommandLine;
    False: aCommandLine := 'su -c ' + '"' + aCommandLine + '"'; //AnsiQuotedStr(aCommandLine, '"');
  end;
  if Confidential then
    LogDatei.log('Shell command: ' + aCommandLine, LLConfidential)
  else
    LogDatei.log('Shell command: ' + aCommandLine, LLDebug);
  if RunCommand(FShell, [FShellOption, 'echo ' +'"'+ FPassword + '" | ' + aCommandLine],
    Output, [poWaitOnExit, poUsePipes, poStderrToOutPut], swoShow) then
  begin
    LogDatei.log('Shell output: ' + Output, LLDebug);
    Result := True;
  end
  else
  begin
    if Length(Output) > 1000 then
      LogDatei.log('Error in RunCommand: ' + Copy(Output, 1, 1000) + '...', LLInfo)
    else
      LogDatei.log('Error in RunCommand: ' + Output, LLInfo);

    Output := 'Error in RunCommand: ' + Output;
    Result := False;
  end;
end;


end.
