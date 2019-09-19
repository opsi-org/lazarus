unit imagestodepot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Process, oslog, FileUtil, LazFileUtils, opsiconnection, LazProgInfo, jwawinbase;

type

  { TFormSaveImagesOnDepot }

  TFormSaveImagesOnDepot = class(TForm)
    ButtonCancel: TButton;
    ButtonCopy: TButton;
    CheckBoxMountDepot: TCheckBox;
    DirectoryEditPathToDepot: TDirectoryEdit;
    EditPassword: TEdit;
    EditPathToDepot: TEdit;
    EditUser: TEdit;
    GroupBoxMountDepot: TGroupBox;
    LabelPathMountDepot: TLabel;
    LabelPassword: TLabel;
    LabelPathToDepot: TLabel;
    LabelUser: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure CheckBoxMountDepotChange(Sender: TObject);
    procedure DirectoryEditPathToDepotChange(Sender: TObject);
    procedure DirectoryEditPathToDepotEnter(Sender: TObject);
    procedure EditPathToDepotChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetUserName_: string;
    function InitLogging(const LogFileName: String; MyLogLevel: integer): boolean;
    procedure MountDepotNT(const User: String; Password: String;
      PathToDepot: String);
    function SaveImagesOnDepot(const PathToDepot: String):boolean;
    procedure UnmountDepotNT(const PathToDepot: String);
    procedure MountDepotUnix(const User: String; Password: String;
      PathToDepot: String);
    procedure UnmountDepotUnix(const PathToDepot: String);
    function SetRights(Path:String):boolean;


  public
    constructor Create;virtual;
    destructor Destroy;override;
  end;

var
  FormSaveImagesOnDepot: TFormSaveImagesOnDepot;

resourcestring
  rsCouldNotSaveIcons = 'Could not save icons on depot.';
  rsCouldNotSaveScreenshots = 'Could not save screenshots on depot.';
  rsCouldNotUnmount = 'Could not unmount depot.';
  rsIsAnInvalidDir = '"%s" is an invalid directory.%sPlease set "%s" to a '
    +'valid directory.';
  rsImagesNotSaved = 'Images could not be saved on opsi depot. %sPlease'
    +' check if depot is mounted with write privileges.';
  rsImagesSaved = 'Images saved on';


implementation

{$R *.lfm}

{ TFormSaveImagesOnDepot }

procedure TFormSaveImagesOnDepot.ButtonCopyClick(Sender: TObject);
var
  PathToDepot : String;
  User: String;
  AlreadyMounted :boolean;
  CopySuccess: boolean;
begin
  CopySuccess := False;
  PathToDepot :=  TrimFilename(DirectoryEditPathToDepot.Text);
  {Mount opsi depot}
  if CheckBoxMountDepot.Checked then
  begin
    if DirectoryExists(PathToDepot) then AlreadyMounted := True
    else
    begin
      AlreadyMounted := False;
      User := EditUser.Text;
      User := '/user:' + user;
     {$IFDEF Windows}
      MountDepotNT(User, EditPassword.Text, PathToDepot);
     {$ENDIF Windows}
     {$IFDEF Unix}
      MountDepotUnix(User, EditPassword.Text, PathToDepot, AlreadyMounted);
     {$ENDIF Unix}
    end;
  end;
  if DirectoryExists(PathToDepot) then
  begin
    CopySuccess := SaveImagesOnDepot(PathToDepot);
  end
  else
  begin
    ShowMessage(Format(rsIsAnInvalidDir, [DirectoryEditPathToDepot.Text,
      LineEnding, LabelPathToDepot.Caption]));
    CopySuccess := False;
    DirectoryEditPathToDepot.Font.Color:= clRed;
  end;
 {$IFDEF Windows}
  if CheckBoxMountDepot.Checked then
  begin
    if not AlreadyMounted then UnmountDepotNT(PathToDepot);
  end;
 {$ENDIF Windows}
 {$IFDEF Unix}
  if CheckBoxMountDepot.Checked then
  begin
   if not AlreadyMounted then UnmountDepotUnix(PathToDepot);
  end;
 {$ENDIF Unix}
  if CopySuccess then Close;
end;

procedure TFormSaveImagesOnDepot.CheckBoxMountDepotChange(Sender: TObject);
begin
  if CheckBoxMountDepot.Checked then GroupBoxMountDepot.Enabled := True
    else GroupBoxMountDepot.Enabled := False;
end;

procedure TFormSaveImagesOnDepot.DirectoryEditPathToDepotChange(Sender: TObject
  );
begin
  DirectoryEditPathToDepot.Font.Color:= clDefault;
end;

procedure TFormSaveImagesOnDepot.DirectoryEditPathToDepotEnter(Sender: TObject);
begin
  DirectoryEditPathToDepot.Font.Color:= clDefault;
end;

procedure TFormSaveImagesOnDepot.EditPathToDepotChange(Sender: TObject);
begin
  DirectoryEditPathToDepot.Text := EditPathToDepot.Text;
end;

procedure TFormSaveImagesOnDepot.FormCreate(Sender: TObject);
begin
   InitLogging('kiosk-images-' + GetUserName_ +'.log', LLDebug);
end;

procedure TFormSaveImagesOnDepot.MountDepotNT(const User: String;
  Password: String; PathToDepot: String);
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
    ShellCommand := 'net use' + ' ' + PathToDepot + ' ' + Password + ' ' + user;
    if RunCommand(Shell, [ShellOptions , ShellCommand], ShellOutput) then
    begin
      ShellCommand := '';
      LogDatei.log('Mounting done', LLInfo);
      //ShowMessage(ShellOutput);
    end
    else LogDatei.log('Error while trying to run command net use ' +
      PathToDepot + ' ' + user + ' on ' + Shell, LLError);
  except
    LogDatei.log('Exception during mounting of ' + PathToDepot, LLDebug);
  end;
end;

procedure TFormSaveImagesOnDepot.UnmountDepotNT(const PathToDepot: String);
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
      ShowMessage(rsCouldNotUnmount);
      LogDatei.log('Error while trying to run command ' +
        ShellCommand + ' on ' + Shell, LLError);
    end;
  except
    LogDatei.log('Exception during unmounting of ' + PathToDepot, LLDebug);
  end;
end;

procedure TFormSaveImagesOnDepot.MountDepotUnix(const User: String;
  Password: String; PathToDepot: String);
begin

end;


procedure TFormSaveImagesOnDepot.UnmountDepotUnix(const PathToDepot: String);
begin

end;

function TFormSaveImagesOnDepot.SetRights(Path:String): boolean;
var
  OpsiConnection: TOpsiConnection;
begin
  Result := False;
  try
    try
      OpsiConnection := TOpsiConnection.Create(False);
      LogDatei.log('ClientID :' + OpsiConnection.MyClientID +
      ' Service_URL :' + OpsiConnection.MyService_URL +
      ' Hostkey :' + OpsiConnection.MyHostkey +
      ' Error :' + OpsiConnection.MyError,LLDebug);
      OpsiConnection.SetRights(path);
      Result := True;
    except
      Result := False;
    end;
  finally
    OpsiConnection.Free;
  end;
end;

constructor TFormSaveImagesOnDepot.Create;
begin
  inherited;
end;

destructor TFormSaveImagesOnDepot.Destroy;
begin
  inherited Destroy;
  LogDatei.Free;
end;


function TFormSaveImagesOnDepot.SaveImagesOnDepot(const PathToDepot: String):boolean;
var
  PathToKioskOnDepot: String;
  Target: String;
  Source: String;
begin
  Result := False;
  PathToKioskOnDepot:= SwitchPathDelims('\opsi-client-agent\files\opsi\opsiclientkiosk\',pdsSystem);
  Source := SwitchPathDelims(TrimFilename('C:\Users\Jan\git-workbench\lazarus\helper\opsiclientkiosk\ock_custom\'),pdsSystem);
  Target := SwitchPathDelims(TrimFilename(PathToDepot + PathToKioskOnDepot + 'ock_custom\'),pdsSystem);
  LogDatei.log('Copy ' + Source + ' to ' + Target, LLInfo);
  if CopyDirTree(Source, Target,[cffOverwriteFile, cffCreateDestDirectory]) then
  begin
    LogDatei.log('Copy done', LLInfo);
    ShowMessage(rsImagesSaved +' '+ Target);
    SetRights('/var/lib/opsi/depot/opsi-client-agent/files/opsi/opsiclientkiosk/ock_custom/');
    LogDatei.log('SetRights done', LLInfo);
    Result := True;
  end
  else
  begin
    LogDatei.log('Images could not be saved on opsi depot. ' + PathToDepot +
      ' Possible solution: mount depot with write privileges.' ,LLDebug);
    ShowMessage(Format(rsImagesNotSaved, [LineEnding]));
    Result := False;
  end;
end;

procedure TFormSaveImagesOnDepot.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

function TFormSaveImagesOnDepot.InitLogging(const LogFileName:String; MyLogLevel:integer): boolean;
begin
  try
    //LogDatei.free;
    LogDatei := TLogInfo.Create;
    LogDatei.WritePartLog := False;
    LogDatei.WriteErrFile:= False;
    LogDatei.WriteHistFile:= False;
    LogDatei.CreateTheLogfile(LogFileName, False);
    LogDatei.LogLevel := MylogLevel;
    LogDatei.log(ProgramInfo.InternalName+ ' ' + 'Version('+ProgramInfo.Version+')'
      + ' starting at ' + DateTimeToStr(now), LLEssential);
    LogDatei.log('Initialize Logging', LLNotice);
    InitLogging := True;
  except
    InitLogging := False;
    LogDatei.log('Error while initialising logging. Calling Method: TFormOpsiClientKiosk.InitLogging',LLDebug);
  end;
end;

function TFormSaveImagesOnDepot.GetUserName_: string;
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
end.

