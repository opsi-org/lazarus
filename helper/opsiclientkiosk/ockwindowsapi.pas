unit OckWindowsAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, osLog,
  DSiWin32,
  ShellAPI,
  OckSystemAPI
  ;

type

{ TWindowsAPI }

  TWindowsAPI = class(TSystemAPI)
  private
    function RunAsAdmin(const Handle: DWord; const Path: string; Params: string): Boolean;
    procedure RunApplicationElevated(const PathToExe:string);
  public
    Handle: DWord;
    function IsAdmin:boolean;override;
    procedure RunApplication(const PathToExe:string);override;
    constructor Create(const Handle:DWord);
    destructor Destroy;override;
  end;

var
  SystemAPI : TSystemAPI;

implementation

{ TWindowsAPI }

function TWindowsAPI.IsAdmin: boolean;
begin
  result := DSiIsAdmin;
end;

function TWindowsAPI.RunAsAdmin(const Handle: DWord; const Path: string;
  Params: string): Boolean;
var
  ShellExecuteInfoA: TShellExecuteInfoA;
begin
  FillChar(ShellExecuteInfoA, SizeOf(ShellExecuteInfoA), 0);
  with ShellExecuteInfoA do
  begin
    cbSize := SizeOf(ShellExecuteInfoA);
    Wnd := Handle;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    lpVerb := 'runas';
    lpFile := PAnsiChar(Path);
    lpParameters := PAnsiChar(Params);
    nShow := 1;
  end;
  Result := ShellExecuteExA(@ShellExecuteInfoA);
end;

procedure TWindowsAPI.RunApplication(const PathToExe: string);
var
  //Shell,
  //ShellOptions,
  //ShellCommand,
  //ShellOutput: String;
  PathToExe: String;
begin
  try
    //Result := '';
    PathToExe := TrimFilename(AppLocation + 'images_to_depot\images_to_depot.exe');
    LogDatei.log('Saving images on depot...',LLInfo);
    {set shell and options}
    //Shell := 'powershell.exe';//PathToExe;//'powershell.exe';
    //ShellOptions := '/c'; //-Verb runAs  'Start-Process PowerShell -Verb RunAs | '
    //ShellCommand := 'Start-Process ' + QuotedStr(PathToExe) + ' -Verb RunAs';
    //if RunCommand(Shell, ['Start-Process', QuotedStr(PathToExe), '-Verb RunAs'], ShellOutput) then
    if RunAsAdmin(Handle, PathToExe,'') then
    begin
      //ShellCommand := '';
      //Result := ShellOutput;
      LogDatei.log('Images saved on depot', LLInfo);
      //LogDatei.log('Shell command: ' + ShellCommand, LLDebug);
      //ShowMessage(ShellOutput);
    end
    else
    begin
      //Result := '';
      LogDatei.log('Error while trying to run as admin ' + PathToExe, LLError);
    end;
  except
    //Result := '';
    LogDatei.log('Exception during SaveImagesOnDepot', LLDebug);
  end;
end;

initialization
   SystemAPI := TWindowsAPI.Create;

finalization
  FreeAndNil(SystemAPI);

end.

