unit imagestoshare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Process, oslog, FileUtil, LazFileUtils;

type

  { TFormSaveImagesOnShare }

  TFormSaveImagesOnShare = class(TForm)
    ButtonCancel: TButton;
    ButtonCopy: TButton;
    CheckBoxMountShare: TCheckBox;
    DirectoryEditPathToShare: TDirectoryEdit;
    EditPassword: TEdit;
    EditPathToShare: TEdit;
    EditUser: TEdit;
    GroupBoxMountShare: TGroupBox;
    LabelPathMountShare: TLabel;
    LabelPassword: TLabel;
    LabelPathToShare: TLabel;
    LabelUser: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure CheckBoxMountShareChange(Sender: TObject);
    procedure DirectoryEditPathToShareEnter(Sender: TObject);
    procedure EditPathToShareChange(Sender: TObject);
  private
    procedure MountShareNT(const User: String; Password: String;
      PathToShare: String);
    function SaveImagesOnShare(const PathToShare: String):boolean;
    procedure UnmountShareNT(const PathToShare: String);
    procedure MountShareUnix(const User: String; Password: String;
      PathToShare: String);
    procedure UnmountShareUnix(const PathToShare: String);

  public

  end;

var
  FormSaveImagesOnShare: TFormSaveImagesOnShare;

resourcestring
  rsCouldNotSaveIcons = 'Could not save icons on share.';
  rsCouldNotSaveScreenshots = 'Could not save screenshots on share.';
  rsCouldNotUnmount = 'Could not unmount share.';
  rsIsAnInvalidDir = '"%s" is an invalid directory.%sPlease set "%s" to a '
    +'valid directory.';
  rsImagesNotSaved = 'Images could not be saved on opsi depot (share).%sPlease'
    +' check if share is mounted with write privileges.';
  rsImagesSaved = 'Images saved on';


implementation

{$R *.lfm}

{ TFormSaveImagesOnShare }

procedure TFormSaveImagesOnShare.ButtonCopyClick(Sender: TObject);
var
  PathToShare : String;
  User: String;
  AlreadyMounted :boolean;
  CopySuccess: boolean;
begin
  CopySuccess := False;
  PathToShare :=  TrimFilename(DirectoryEditPathToShare.Text);
  {Mount opsi depot}
  if CheckBoxMountShare.Checked then
  begin
    if DirectoryExists(PathToShare) then AlreadyMounted := True
    else
    begin
      AlreadyMounted := False;
      User := EditUser.Text;
      User := '/user:' + user;
     {$IFDEF Windows}
      MountShareNT(User, EditPassword.Text, PathToShare);
     {$ENDIF Windows}
     {$IFDEF Unix}
      MountShareUnix(User, EditPassword.Text, PathToShare, AlreadyMounted);
     {$ENDIF Unix}
    end;
  end;
  if DirectoryExists(PathToShare) then
  begin
    CopySuccess := SaveImagesOnShare(PathToShare);
  end
  else
  begin
    ShowMessage(Format(rsIsAnInvalidDir, [DirectoryEditPathToShare.Text,
      LineEnding, LabelPathToShare.Caption]));
    CopySuccess := False;
    DirectoryEditPathToShare.Font.Color:= clRed;
  end;
 {$IFDEF Windows}
  if CheckBoxMountShare.Checked then
  begin
    if not AlreadyMounted then UnmountShareNT(PathToShare);
  end;
 {$ENDIF Windows}
 {$IFDEF Unix}
  if CheckBoxMountShare.Checked then
  begin
   if not AlreadyMounted then UnmountShareUnix(PathToShare);
  end;
 {$ENDIF Unix}
  if CopySuccess then Close;
end;

procedure TFormSaveImagesOnShare.CheckBoxMountShareChange(Sender: TObject);
begin
  if CheckBoxMountShare.Checked then GroupBoxMountShare.Enabled := True
    else GroupBoxMountShare.Enabled := False;
end;

procedure TFormSaveImagesOnShare.DirectoryEditPathToShareEnter(Sender: TObject);
begin
  DirectoryEditPathToShare.Font.Color:= clDefault;
end;

procedure TFormSaveImagesOnShare.EditPathToShareChange(Sender: TObject);
begin
  DirectoryEditPathToShare.Text := EditPathToShare.Text;
end;

procedure TFormSaveImagesOnShare.MountShareNT(const User: String;
  Password: String; PathToShare: String);
var
  Shell,
  ShellOptions,
  ShellCommand,
  ShellOutput: String;
begin
  try
    LogDatei.log('Mounting ' + PathToShare ,LLInfo);
    {set shell and options}
    Shell := 'cmd.exe';
    ShellOptions := '/c';
    ShellCommand := 'net use' + ' ' + PathToShare + ' ' + Password + ' ' + user;
    if RunCommand(Shell, [ShellOptions , ShellCommand], ShellOutput) then
    begin
      ShellCommand := '';
      LogDatei.log('Mounting done', LLInfo);
      //ShowMessage(ShellOutput);
    end
    else LogDatei.log('Error while trying to run command net use ' +
      PathToShare + ' ' + user + ' on ' + Shell, LLError);
  except
    LogDatei.log('Exception during mounting of ' + PathToShare, LLDebug);
  end;
end;

procedure TFormSaveImagesOnShare.UnmountShareNT(const PathToShare: String);
var
  Shell,
  ShellOptions,
  ShellCommand,
  ShellOutput: String;
begin
  try
    LogDatei.log('Unmounting ' + PathToShare, LLInfo);
    {set shell and options}
    Shell := 'cmd.exe';
    ShellOptions := '/c';
    ShellCommand := 'net use /delete' + ' ' + PathToShare;
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
    LogDatei.log('Exception during unmounting of ' + PathToShare, LLDebug);
  end;
end;

procedure TFormSaveImagesOnShare.MountShareUnix(const User: String;
  Password: String; PathToShare: String);
begin

end;


procedure TFormSaveImagesOnShare.UnmountShareUnix(const PathToShare: String);
begin

end;

function TFormSaveImagesOnShare.SaveImagesOnShare(const PathToShare: String):boolean;
var
  PathToKioskOnDepot: String;
  Target: String;
  Source: String;
begin
  Result := False;
  PathToKioskOnDepot:= '\opsi-client-agent\files\opsi\opsiclientkiosk\';
  Source := TrimFilename(Application.Location + 'ock_custom\');
  Target := TrimFilename(PathToShare + PathToKioskOnDepot + 'ock_custom\');
  LogDatei.log('Copy ' + Source + ' to ' + Target, LLInfo);
  if CopyDirTree(Source, Target,[cffOverwriteFile, cffCreateDestDirectory]) then
  begin
    LogDatei.log('Copy done', LLInfo);
    ShowMessage(rsImagesSaved +' '+ Target);
    Result := TRue;
  end
  else
  begin
    LogDatei.log('Images could not be saved on opsi depot (share).' +
      ' Possible solution: mount share with write privileges.' ,LLDebug);
    ShowMessage(Format(rsImagesNotSaved, [LineEnding]));
    Result := False;
  end;
end;

procedure TFormSaveImagesOnShare.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

