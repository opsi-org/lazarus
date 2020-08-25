unit OckLinux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, osRunCommandElevated, osLog;


function isAdmin: boolean;
function GetUserName_: string;
procedure MountDepot(const User: string; Password: string; PathToDepot: string);
procedure UnmountDepot(const PathToDepot: string);
function Copy(Source:string; Destination:string):boolean;

var
  RunCommandElevated: TRunCommandElevated;

implementation

function isAdmin: boolean;
begin
  Result := True;
end;

function GetUserName_: string;
var
  Output: string;
begin
  RunCommand('/bin/sh', ['echo $USER'], Output, [], swoHIDE);
  Result := Output;
end;


procedure MountDepot(const User: string; Password: string; PathToDepot: string);
var
  Shell, ShellOptions, ShellCommand, ShellOutput: string;
begin
  try
    LogDatei.log('Mounting ' + PathToDepot, LLInfo);
    {set shell and options}
    Shell := '/bin/sh';
    ShellOptions := '-c';
    //mount -t cifs -o username=werner //bonifax.uib.gmbh/opsi_depot /home/jan/opsi_depot
    ShellCommand := 'mount -t cifs' + ' ' + PathToDepot + ' ' +
      Password + ' ' + 'username:' + User;
    if assigned(RunCommandElevated) then
    begin
      if RunCommandElevated.Run(Shell, [ShellOptions, ShellCommand], ShellOutput) then
      begin
        ShellCommand := '';
        LogDatei.log('Mounting done', LLInfo);
        //ShowMessage(ShellOutput);
      end
      else
      begin
        LogDatei.log('Error while trying to run command mount' + PathToDepot +
          ' ' + User + ' on ' + Shell, LLError);
      end;
    end
    else
    begin
      LogDatei.log('RunCommandElevated not assigned',LLError);
    end;
  except
    LogDatei.log('Exception during mounting of ' + PathToDepot, LLDebug);
  end;
end;

procedure UnmountDepot(const PathToDepot: string);
var
  Shell, ShellOptions, ShellCommand, ShellOutput: string;
begin
  try
    LogDatei.log('Unmounting ' + PathToDepot, LLInfo);
    {set shell and options}
    Shell := '/bin/sh';
    ShellOptions := '-c';
    ShellCommand := 'unmount' + ' ' + PathToDepot;
    {Run Command}
    if assigned(RunCommandElevated) then
    begin
        if RunCommandElevated.Run(Shell, [ShellOptions, ShellCommand], ShellOutput) then
      begin
        LogDatei.log('Unmounting done', LLInfo);
        //ShowMessage(ShellOutput);
      end
      else
      begin
        LogDatei.log('Error while trying to run command ' + ShellCommand +
          ' on ' + Shell, LLError);
      end;
    end
    else
    begin
      LogDatei.log('RunCommandElevated not assigned',LLError);
    end;
  except
    LogDatei.log('Exception during unmounting of ' + PathToDepot, LLDebug);
  end;
end;

function Copy(Source: string; Destination: string): boolean;
var
  Output: string;
begin
  Output := '';
  Result := RunCommandElevated.Run('cp -r --remove-destination' + ' '+ Source + ' ' + Destination, Output);
end;

initialization
RunCommandElevated := TRunCommandElevated.Create('',True);

finalization
FreeAndNil(RunCommandElevated);

end.





