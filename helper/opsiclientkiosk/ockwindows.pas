unit OckWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, DSiWin32, jwawinbase, FileUtil,
  osLog, OckPathsUtils;


type

  { TOckPathsWindows }

  TOckPathsWindows = class(TOckPaths)
  private
    //procedure CopyCustomSettingsToWriteableFolder;
  public
    procedure SetAdminModePaths; override;
    procedure SetUserModePaths; override;
    constructor Create; override;
    destructor Destroy; override;
  end;



function isAdmin:boolean;
function GetUserName_: string;
procedure MountDepot(const User: string; Password: string; PathToDepot: string);
procedure UmountDepot(const PathToDepot: string);
function IsDepotMounted(const PathToDepot:string): boolean;
function Copy(Source:string; Destination:string):boolean;

const
  // Include paths here. Setting of the used paths (admin mode vs. user mode) then occures in TOckPathsMacOS
  DefaultFolder = '/default';
  CustomFolder = '/ock_custom';
  RelativePathProductIcons = '/product_icons';
  RelativePathScreenShots = '/screenshots';
  RelativePathSkin ='/skin';
  {$IFDEF KIOSK_IN_AGENT} //if the kiosk is in the opsi-client-agent product
  PathKioskAppOnDepot = '/opsi-client-agent/files/opsi/opsiclientkiosk/app';
  {$ELSE} //if the kiosk is a standalone opsi product
  PathKioskAppOnDepot = '/opsi-client-kiosk/files/app';
  {$ENDIF KIOSK_IN_AGENT}

implementation

function isAdmin: boolean;
begin
  Result := DSiIsAdmin;
end;

{:Returns user name of the current thread.
  @author  Miha-R, Lee_Nover
  @since   2002-11-25
}
function GetUserName_: string;
var
  buffer: PChar;
  bufferSize: DWORD;
begin
  bufferSize := 256; //UNLEN from lmcons.h
  buffer := AllocMem(bufferSize * SizeOf(char));
  try
    GetUserName(buffer, bufferSize);
    Result := string(buffer);
  finally
    FreeMem(buffer, bufferSize);
  end;
end; { DSiGetUserName}


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
    Shell := 'cmd.exe';
    ShellOptions := '/c';
    ShellCommand := 'net use' + ' ' + PathToDepot + ' ' + Password + ' ' + '/user:' + User;
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

procedure UmountDepot(const PathToDepot: String);
var
  Shell,
  ShellOptions,
  ShellCommand,
  ShellOutput: String;
begin
  try
    LogDatei.log('Unmounting ' + PathToDepot, LLInfo);
    {set shell and options}
    Shell := 'cmd.exe';
    ShellOptions := '/c';
    ShellCommand := 'net use /delete' + ' ' + PathToDepot;
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

function IsDepotMounted(const PathToDepot: string): boolean;
begin
  if DirectoryExists(PathToDepot) then
  begin
    Result := True;
    LogDatei.log('opsi_depot_rw already mounted', LLInfo);
  end
  else
  begin
    Result := False;
    LogDatei.log('opsi_depot_rw not mounted', LLInfo);
  end;
end;

function Copy(Source: string; Destination: string): boolean;
begin
  Result := CopyDirTree(Source, Destination,[cffOverwriteFile, cffCreateDestDirectory]);
end;

{ TOckPathsWindows }

procedure TOckPathsWindows.SetAdminModePaths;
begin
  FOnClient.FKioskApp := ProgramDirectory;
  FOnClient.FDefaultIcons := FOnClient.FKioskApp + DefaultFolder + RelativePathProductIcons;
  FOnClient.FCustomSettings := FOnClient.FKioskApp + CustomFolder;
  FOnClient.FCustomSkin := FOnClient.FCustomSettings + RelativePathSkin;
  FOnClient.FCustomIcons := FOnClient.FCustomSettings + RelativePathProductIcons;
  FOnClient.FCustomScreenShots := FOnClient.FCustomSettings + RelativePathScreenShots;

  FOnDepot.FKioskApp := PathKioskAppOnDepot;
  FOnDepot.FCustomSettings := FOnDepot.FKioskApp;
  FOnDepot.FCustomIcons := FOnDepot.FCustomSettings + RelativePathProductIcons;
  FOnDepot.FCustomScreenShots := FOnDepot.FCustomSettings + RelativePathScreenShots;
end;

procedure TOckPathsWindows.SetUserModePaths;
begin
  FOnClient.FKioskApp := ProgramDirectory;
  //Default
  FOnClient.FDefaultIcons := FOnClient.FKioskApp + DefaultFolder + RelativePathProductIcons;
  FOnClient.FDefaultSkin := FOnClient.FKioskApp + DefaultFolder + RelativePathSkin;
  //Custom
  FOnClient.FCustomSettings := FOnClient.FKioskApp + CustomFolder;
  FOnClient.FCustomSkin := FOnClient.FCustomSettings + RelativePathSkin;
  FOnClient.FCustomIcons := FOnClient.FCustomSettings + RelativePathProductIcons;
  FOnClient.FCustomScreenShots := FOnClient.FCustomSettings + RelativePathScreenShots;
end;

constructor TOckPathsWindows.Create;
begin
  inherited Create;
  InitPaths;
end;

destructor TOckPathsWindows.Destroy;
begin
  inherited Destroy;
end;

initialization
OckPaths := TOckPathsWindows.Create;

finalization
FreeAndNil(OckPaths);

end.

