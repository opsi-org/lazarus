unit imagestoshare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Process, oslog, FileUtil;

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
    procedure EditPathToShareChange(Sender: TObject);
  private
    procedure MountShareNT(const User: String; Password: String;
      PathToShare: String);
    procedure SaveImagesOnShare(const PathToShare: String);
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


implementation

{$R *.lfm}

{ TFormSaveImagesOnShare }

procedure TFormSaveImagesOnShare.ButtonCopyClick(Sender: TObject);
var
  PathToShare : String;
  User: String;
  AlreadyMounted :boolean;
begin
  PathToShare :=  DirectoryEditPathToShare.Text;
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
  if DirectoryExists(PathToShare) then SaveImagesOnShare(PathToShare)
  else
  begin
    ShowMessage('"' + DirectoryEditPathToShare.Text + '" ' +  'is an invalid directory.' + LineEnding
      + 'Please set "' + LabelPathToShare.Caption + '" to a valid directory.');
    //DirectoryEditPathToShare.Font.Color:= clRed;
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
  Close;
end;

procedure TFormSaveImagesOnShare.CheckBoxMountShareChange(Sender: TObject);
begin
  if CheckBoxMountShare.Checked then GroupBoxMountShare.Enabled := True
    else GroupBoxMountShare.Enabled := False;
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
  {check if share is already mounted}
  try
    {set shell and options}
    Shell := 'cmd.exe';
    ShellOptions := '/c';
    ShellCommand := 'net use' + ' ' + PathToShare + ' ' + Password + ' ' + user;
    if RunCommand(Shell, [ShellOptions , ShellCommand], ShellOutput) then
    begin
      ShellCommand := '';
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
    {set shell and options}
    Shell := 'cmd.exe';
    ShellOptions := '/c';
    ShellCommand := 'net use /delete' + ' ' + PathToShare;
    {Run Command}
    if RunCommand(Shell, [ShellOptions, ShellCommand], ShellOutput) then
    begin
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

procedure TFormSaveImagesOnShare.SaveImagesOnShare(const PathToShare: String);
var
  PathToKioskOnDepot: String;
  Target: String;
  Source: String;
begin
  PathToKioskOnDepot:= '\opsi-client-agent\files\opsi\opsiclientkiosk\';
  Source := Application.Location + 'ock_custom\';
  Target := PathToShare + PathToKioskOnDepot + 'ock_custom\';
  if CopyDirTree(Source, Target,[cffOverwriteFile, cffCreateDestDirectory])
    then ShowMessage('Images saved on ' + Target);

end;

procedure TFormSaveImagesOnShare.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

