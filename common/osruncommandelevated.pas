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
<<<<<<< HEAD
  public
    constructor Create(aPassword:string; aSudo:boolean);overload;
    {Set FPassword,  FSudo and creates LogDatei}
    destructor Destroy;override;

    function Run(aCommandLine: string): string;
    {Runs the command line as root (FSudo = false) or sudo (FSudo = true) using FPassword
    aCommandLine can be every correct shell command}

=======
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
>>>>>>> experimental
    property Password: string write FPassword;
    property Sudo: boolean read FSudo write FSudo;
  end;


implementation

{ TRunCommandElevated }

<<<<<<< HEAD
constructor TRunCommandElevated.Create(aPassword: string; aSudo: boolean);
=======
constructor TRunCommandElevated.Create(aPassword: string; aSudo: boolean; aShell: string = '/bin/sh'; aShellOption: string = '-c');
>>>>>>> experimental
begin
  inherited Create;
  FPassword := aPassword;
  FSudo := aSudo;
<<<<<<< HEAD
=======
  FShell := aShell;
  FShellOption := aShellOption;
>>>>>>> experimental
  //LogDatei := TLogInfo.Create;
end;

destructor TRunCommandElevated.Destroy;
begin
  //LogDatei.Free;
  inherited Destroy;
end;

<<<<<<< HEAD
function TRunCommandElevated.Run(aCommandLine: string): string;
var
  Output: string;
begin
  //aCommandLine := 'chown -c $USER /etc/apt/sources.list.d/opsi.list'; //for testing
  LogDatei.log('Shell command: ' + aCommandLine, LLInfo);
=======
function TRunCommandElevated.Run(aCommandLine: string; out Output:string; Confidential:boolean = false): boolean;
begin
  //aCommandLine := 'chown -c $USER /etc/apt/sources.list.d/opsi.list'; //for testing
  //LogDatei.log('Shell command: ' + aCommandLine, LLInfo);
>>>>>>> experimental
  case FSudo of
    True: aCommandLine := 'sudo -S ' + aCommandLine;
    False: aCommandLine := 'su -c ' + '"' + aCommandLine + '"'; //AnsiQuotedStr(aCommandLine, '"');
  end;
<<<<<<< HEAD
  LogDatei.log('Shell command: ' + aCommandLine, LLDebug);
  if RunCommand('/bin/sh', ['-c', 'echo ' + FPassword + ' | ' + aCommandLine],
    Output, [poWaitOnExit, poUsePipes]) then
  begin
    Result := Output;
    LogDatei.log('Shell command succesful', LLInfo);
    LogDatei.log('Shell output: ' + Output, LLDebug);
=======
  if Confidential then
    LogDatei.log('Shell command: ' + aCommandLine, LLConfidential)
  else
    LogDatei.log('Shell command: ' + aCommandLine, LLDebug);
  if RunCommand(FShell, [FShellOption, 'echo ' + FPassword + ' | ' + aCommandLine],
    Output, [poWaitOnExit, poUsePipes], swoHIDE) then
  begin
    LogDatei.log('Shell command succesful', LLInfo);
    LogDatei.log('Shell output: ' + Output, LLDebug);
    Result := True;
>>>>>>> experimental
  end
  else
  begin
    LogDatei.log('Error in RunCommand ' + Output, LLInfo);
<<<<<<< HEAD
    Result := 'Error in RunCommand: ' + Output;
=======
    Output := 'Error in RunCommand: ' + Output;
    Result := False;
>>>>>>> experimental
  end;
end;


end.
