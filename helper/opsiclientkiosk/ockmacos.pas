unit OckMacOS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, StrUtils, osRunCommandElevated, osLog;


function isAdmin: boolean;
function GetUserName_: string;
procedure MountDepot(const User: string; Password: string; PathToDepot: string);
procedure UmountDepot(const PathToDepot: string);
function IsDepotMounted(const PathToDepot: string):boolean;
function Copy(Source:string; Destination:string):boolean;
function PasswordCorrect:boolean;

var
  RunCommandElevated: TRunCommandElevated;

const
  MountPoint = '/mnt/opsi_depot_rw';

implementation

var
  MountPointAlreadyExists: boolean = False; //exists the mountpoint already in the system?

function isAdmin: boolean;
begin
  Result := True;
end;

function GetUserName_: string;
var
  Output: string;
begin
  RunCommand('/bin/sh', ['-c','echo $USER'], Output, [poUsePipes, poWaitOnExit], swoHIDE);
  //LogDatei.log('opsi-client-kiosk used by: ' + Output   , LLInfo);
  Result := Trim(Output);
end;


procedure MountDepot(const User: string; Password: string; PathToDepot: string);
var
  ShellCommand: string;
  ShellOutput: string;
begin
  try
    LogDatei.log('Mounting ' + PathToDepot + ' on' + MountPoint , LLInfo);
    {set shell and options}
    if assigned(RunCommandElevated) then
    begin
      //RunCommandElevated.Shell := '/bin/sh'; //not necessary to set because this is the default value
      //RunCommandElevated.ShellOptions := '-c'; //not necessary to set because this is the default value
      if not DirectoryExists(MountPoint) then
      begin
        RunCommandElevated.Run('mkdir ' + MountPoint, ShellOutput);
        MountPointAlreadyExists := False;
      end
      else MountPointAlreadyExists := True;
      ShellCommand := 'mount -t cifs' + ' '
                      + '-o username=' + User + ',' + 'password=' + Password + ' '
                      + PathToDepot + ' '
                      + MountPoint;
      if RunCommandElevated.Run(ShellCommand, ShellOutput, True) then
      begin
        ShellCommand := '';
        LogDatei.log('Mounting done', LLInfo);
        //ShowMessage(ShellOutput);
      end
      else
      begin
        LogDatei.log('Error while trying to run command mount for path: ' + PathToDepot, LLError);
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

procedure UmountDepot(const PathToDepot: string);
var
  ShellCommand, ShellOutput: string;
begin
  try
    LogDatei.log('Umount ' + PathToDepot, LLInfo);
    {Run Command}
    if assigned(RunCommandElevated) then
    begin
      //RunCommandElevated.Shell := '/bin/sh'; //not necessary to set because this is the default value
      //RunCommandElevated.ShellOptions := '-c'; //not necessary to set because this is the default value
      ShellCommand := 'umount' + ' ' + PathToDepot;
      if RunCommandElevated.Run(ShellCommand, ShellOutput) then
      begin
        LogDatei.log('Umount done', LLInfo);
        if DirectoryExists(MountPoint) and (not MountPointAlreadyExists) then RunCommandElevated.Run('rm -d ' + MountPoint, ShellOutput)
        //ShowMessage(ShellOutput);
      end
      else
      begin
        LogDatei.log('Error while trying to run command.', LLError);
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

function IsDepotMounted(const PathToDepot:string): boolean;
var
  ShellOutput:String;
begin
  ShellOutput := '';
  RunCommand('/bin/sh', ['-c', 'mount | grep -i ' + '"' + PathToDepot + '"'], ShellOutput);
  if ShellOutput <> '' then
  begin
    Result := True;
    LogDatei.log(PathToDepot + ' already mounted', LLInfo);
  end
  else
  begin
    Result := False;
    LogDatei.log(PathToDepot + ' not mounted', LLInfo);
  end;
end;

function Copy(Source: string; Destination: string): boolean;
var
  Output: string;
begin
  Output := '';
  Result := RunCommandElevated.Run('cp -r --remove-destination' + ' '+ Source + ' ' + Destination, Output);
end;

function PasswordCorrect: boolean;
var
  Output:string;
begin
  if RunCommandElevated.Run('echo "Passwort korrekt"', Output) then
  begin
    Result := ContainsStr(Output, 'Passwort korrekt');
  end
  else Result := False;
end;

initialization
RunCommandElevated := TRunCommandElevated.Create('',True);

finalization
FreeAndNil(RunCommandElevated);

end.





