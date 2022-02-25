unit OckMacOS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, StrUtils, FileUtil, LazFileUtils, osRunCommandElevated, osLog, OckPathsUtils;


type

  { TPathsOnClientMacOS }

  TPathsOnClientMacOS = class(TPathsOnClient)
  private
    procedure CopyCustomSettingsToWriteableFolder;
  public
    procedure SetAdminMode(theAdminMode: boolean); override;
    procedure SetAdminModePaths; override;
    procedure SetUserModePaths; override;
    //constructor Create; override;
    //destructor Destroy; override;
  end;

  { TPathsOnDepotMacOS }

  TPathsOnDepotMacOS = class(TPathsOnDepot)
    procedure SetDepotPaths; override;
  end;

function isAdmin: boolean;
function GetUserName_: string;
procedure MountDepot(const User: string; Password: string; PathToDepot: string);
procedure UmountDepot(const PathToDepot: string);
function IsDepotMounted(const PathToDepot: string):boolean;
function Copy(Source:string; Destination:string):boolean;
function PasswordCorrect:boolean;




var
  RunCommandElevated: TRunCommandElevated;
  PathsOnClient: TPathsOnClientMacOS;
  PathsOnDepot: TPathsOnDepotMacOS;

const
  // Include paths here. Setting of the used paths (admin mode vs. user mode) then occures in TOckPathsMacOS
  MountPoint = '/opsi_depot_rw';
  DefaultFolder = '/default';
  CustomFolder = '/ock_custom';
  AbsolutePathCustomSettingsAdminMode =  '/tmp/org.opsi.OpsiClientKiosk' + CustomFolder;
  AbsolutePathSettings = '/Library/Application Support/org.opsi.OpsiClientKiosk';
  AbsolutePathCustomSettingsUserMode = AbsolutePathSettings + CustomFolder;
  RelativePathDefaultSettings = '/../Resources' + DefaultFolder;
  RelativePathProductIcons = '/product_icons';
  RelativePathScreenShots = '/screenshots';
  RelativePathSkin ='/skin';
  {$IFDEF KIOSK_IN_AGENT} //if the kiosk is in the opsi-client-agent product
  PathKioskAppOnDepot = '/opsi-client-agent/files/opsi/opsiclientkiosk/app';
  {$ELSE} //if the kiosk is a standalone opsi product
  PathKioskAppOnDepot = '/opsi-client-kiosk/files/app';
  {$ENDIF KIOSK_IN_AGENT}

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
    LogDatei.log('Mounting ' + PathToDepot + ' on ' + MountPoint , LLInfo);
    {set shell and options}
    if assigned(RunCommandElevated) then
    begin
      //RunCommandElevated.Shell := '/bin/sh'; //not necessary to set because this is the default value
      //RunCommandElevated.ShellOptions := '-c'; //not necessary to set because this is the default value
      if not DirectoryExists(MountPoint) then
      begin
        //RunCommandElevated.Run('mkdir ' + MountPoint, ShellOutput);
        if RunCommand('/bin/sh', ['-c', 'mkdir ' + MountPoint], ShellOutput) then
        begin
          LogDatei.log('mkdir ' + MountPoint + ' done ', LLInfo);
        end
        else
        begin
          LogDatei.log('Error while trying to run command "mkdir ' + MountPoint + '"', LLError);
        end;
        MountPointAlreadyExists := False;
      end
      else MountPointAlreadyExists := True;
      Delete(PathToDepot, 1, 2);
      ShellCommand := 'mount_smbfs' + ' '
                      + '//' + User + '@'
                      + PathToDepot + ' '
                      + MountPoint;
      //':' + Password + '@'
      //if RunCommandElevated.Run(ShellCommand, ShellOutput, True) then
      if RunCommand('/bin/sh', ['-c', ShellCommand], ShellOutput) then
      begin
        ShellCommand := '';
        LogDatei.log('Mounting done', LLInfo);
        //ShowMessage(ShellOutput);
      end
      else
      begin
        LogDatei.log('Error while trying to run command: ' + ShellCommand, LLError);
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

{ TPathsOnDepotMacOS }

procedure TPathsOnDepotMacOS.SetDepotPaths;
begin
  FKioskApp := PathKioskAppOnDepot;
  FCustomSettings := FKioskApp;
  FCustomIcons := FCustomSettings + RelativePathProductIcons;
  FCustomScreenShots := FCustomSettings + RelativePathScreenShots;
end;


{ TPathsOnClientMacOS}

procedure TPathsOnClientMacOS.CopyCustomSettingsToWriteableFolder;
var
  Output: string;
begin
  //RunCommand('/bin/sh',
  //  ['-c','cp -R' + ' ' + AbsolutePathCustomSettingsUserMode + ' ' + AbsolutePathCustomSettingsAdminMode ],
  //  Output, [poUsePipes, poWaitOnExit], swoHIDE);
  LogDatei.log('Removing old settings from ' + AbsolutePathCustomSettingsAdminMode, LLInfo);
  if DeleteDirectory(AbsolutePathCustomSettingsAdminMode, False) then
  begin
    LogDatei.log('Removing old settings done', LLInfo);
  end;
  CopyDirTree(AbsolutePathCustomSettingsUserMode, AbsolutePathCustomSettingsAdminMode,[cffOverwriteFile, cffCreateDestDirectory]);
  if not DirectoryExists(AbsolutePathCustomSettingsAdminMode + PathDelim + RelativePathSkin) then
    CreateDir(AbsolutePathCustomSettingsAdminMode + PathDelim + RelativePathSkin);
  if not DirectoryExists(AbsolutePathCustomSettingsAdminMode + PathDelim + RelativePathProductIcons) then
    CreateDir(AbsolutePathCustomSettingsAdminMode + PathDelim + RelativePathProductIcons);
  if not DirectoryExists(AbsolutePathCustomSettingsAdminMode + PathDelim + RelativePathScreenShots) then
    CreateDir(AbsolutePathCustomSettingsAdminMode + PathDelim + RelativePathScreenShots);
end;

procedure TPathsOnClientMacOS.SetAdminMode(theAdminMode: boolean);
begin
  inherited SetAdminMode(theAdminMode);
  if FAdminMode then CopyCustomSettingsToWriteableFolder;
end;

procedure TPathsOnClientMacOS.SetAdminModePaths;
begin
  FKioskApp := ChompPathDelim(ProgramDirectory);
  //Default
  FDefaultSettings := FKioskApp + RelativePathDefaultSettings;
  FDefaultIcons := FDefaultSettings + RelativePathProductIcons;
  FDefaultSkin := FDefaultSettings + RelativePathSkin;
  //Custom
  FCustomSettings := AbsolutePathCustomSettingsAdminMode;
  FCustomSkin := FCustomSettings + RelativePathSkin;
  FCustomIcons := FCustomSettings + RelativePathProductIcons;
  FCustomScreenShots := FCustomSettings + RelativePathScreenShots;
end;

procedure TPathsOnClientMacOS.SetUserModePaths;
begin
  FKioskApp := ChompPathDelim(ProgramDirectory);
  //Dfault
  FDefaultSettings := FKioskApp + RelativePathDefaultSettings;
  FDefaultIcons := FDefaultSettings + RelativePathProductIcons;
  FDefaultSkin := FDefaultSettings + RelativePathSkin;
  //Custom
  FCustomSettings := AbsolutePathCustomSettingsUserMode;
  FCustomSkin := FCustomSettings + RelativePathSkin;
  FCustomIcons := FCustomSettings + RelativePathProductIcons;
  FCustomScreenShots := FCustomSettings + RelativePathScreenShots;
end;


initialization
  RunCommandElevated := TRunCommandElevated.Create('',True);
  PathsOnClient := TPathsOnClientMacOS.Create;
  PathsOnDepot := TPathsOnDepotMacOS.Create;

finalization
  FreeAndNil(RunCommandElevated);
  FreeAndNil(PathsOnClient);
  FreeAndNil(PathsOnDepot);

end.





