unit OckLinux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

function isAdmin:boolean;
function GetUserName_: string;

implementation

function isAdmin: boolean;
begin
  Result := True;
end;

function GetUserName_: string;
var
  Output: String;
begin
  RunCommand('/bin/sh',['echo $USER'],Output,[],swoHIDE);
  Result := Output;
end;


procedure MountDepot(const User: String; Password: String; PathToDepot: String);
var
  Shell,
  ShellOptions,
  ShellCommand,
  ShellOutput: String;
begin
  try
    LogDatei.log('Mounting ' + PathToDepot ,LLInfo);
    {set shell and options}
    Shell := '/bin/sh';
    ShellOptions := '-c';
    ShellCommand := 'mount -t cifs' + ' ' + PathToDepot + ' ' + Password + ' ' + 'username:' + User;
    if RunCommand(Shell, [ShellOptions , ShellCommand], ShellOutput) then
    begin
      ShellCommand := '';
      LogDatei.log('Mounting done', LLInfo);
      //ShowMessage(ShellOutput);
    end
    else LogDatei.log('Error while trying to run command net use ' +
      PathToDepot + ' ' + User + ' on ' + Shell, LLError);
  except
    LogDatei.log('Exception during mounting of ' + PathToDepot, LLDebug);
  end;
end;

procedure UnmountDepot(const PathToDepot: String);
var
  Shell,
  ShellOptions,
  ShellCommand,
  ShellOutput: String;
begin
  try
    LogDatei.log('Unmounting ' + PathToDepot, LLInfo);
    {set shell and options}
    Shell := '/bin/sh';
    ShellOptions := '-c';
    ShellCommand := 'unmount' + ' ' + PathToDepot;
    {Run Command}
    if RunCommand(Shell, [ShellOptions, ShellCommand], ShellOutput) then
    begin
      LogDatei.log('Unmounting done', LLInfo);
     //ShowMessage(ShellOutput);
    end
    else
    begin
      LogDatei.log('Error while trying to run command ' +
        ShellCommand + ' on ' + Shell, LLError);
    end;
  except
    LogDatei.log('Exception during unmounting of ' + PathToDepot, LLDebug);
  end;
end;

end.

