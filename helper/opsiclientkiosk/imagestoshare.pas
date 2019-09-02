unit imagestoshare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Process, oslog;

type

  { TFormSaveImagesOnShare }

  TFormSaveImagesOnShare = class(TForm)
    ButtonCancel: TButton;
    ButtonCopy: TButton;
    EditPassword: TEdit;
    EditPathToShare: TEdit;
    EditUser: TEdit;
    LabelPassword: TLabel;
    LabelUser: TLabel;
    LabelPathToShare: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
  private
    procedure MountShareNT(const User: String; Password: String;
      PathToShare: String; var mounted: boolean);
    procedure SaveImagesOnShareNT(const PathToShare: String);
    procedure UnmountShareNT(const PathToShare: String);
    procedure MountShareUnix(const User: String; Password: String;
      PathToShare: String; var mounted: boolean);
    procedure SaveImagesOnShareUnix(const PathToShare: String);
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
  PathToShare :=  EditPathToShare.Text;
  User := EditUser.Text;
  User := '/user:' + user;
  AlreadyMounted := False;
  {$IFDEF Windows}
    MountShareNT(User, EditPassword.Text, PathToShare, AlreadyMounted);
    SaveImagesOnShareNT(PathToShare);
    if not AlreadyMounted then UnmountShareNT(PathToShare);
  {$ENDIF Windows}
  {$IFDEF Unix}
    MountShareUnix(User, EditPassword.Text, PathToShare, AlreadyMounted);
    SaveImagesOnShareUnix(PathToShare);
    if not AlreadyMounted then UnmountShareUnix(PathToShare);
  {$ENDIF Unix}
  Close;
end;

procedure TFormSaveImagesOnShare.MountShareNT(const User: String;
  Password: String; PathToShare: String; var mounted: boolean);
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
    {check if remote folder is already mounted}
    ShellCommand := 'net use';
    if RunCommand(Shell, [ShellOptions , ShellCommand], ShellOutput) then
    begin
      if ShellOutput.Contains(PathToShare) then mounted := True else mounted := False;
    end
    else LogDatei.log('Error executing' + ShellCommand + 'on' + Shell, LLError);
    {mount remote folder if not mounted}
    if not mounted then
    begin
      ShellCommand := 'net use' + ' ' + PathToShare + ' ' + Password + ' ' + user;
      if RunCommand(Shell, [ShellOptions , ShellCommand], ShellOutput) then
      begin
        ShellCommand := '';
        //ShowMessage(ShellOutput);
      end
      else LogDatei.log('Error while trying to run command ' + ShellCommand + 'on' + Shell, LLError);
    end;
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
  {set shell and options}
  Shell := 'cmd.exe';
  ShellOptions := '/c';
  ShellCommand := 'net use /delete' + ' ' + PathToShare;
  {Run Command}
  if RunCommand(Shell, [ShellOptions, ShellCommand], ShellOutput) then
  begin
    //ShowMessage(ShellOutput);
  end
  else ShowMessage(rsCouldNotUnmount);
end;

procedure TFormSaveImagesOnShare.MountShareUnix(const User: String;
  Password: String; PathToShare: String; var mounted: boolean);
begin

end;

procedure TFormSaveImagesOnShare.SaveImagesOnShareUnix(const PathToShare: String
  );
begin

end;

procedure TFormSaveImagesOnShare.UnmountShareUnix(const PathToShare: String);
begin

end;

procedure TFormSaveImagesOnShare.SaveImagesOnShareNT(const PathToShare: String);
var
  Shell,
  ShellOptions,
  ShellCommand,
  ShellOutput: String;
  PathToClientAgent: String;
begin
  PathToClientAgent:= '\opsi-client-agent\files\opsi\opsiclientkiosk\custom\';
  {set shell and options}
  Shell := 'cmd.exe';
  ShellOptions := '/c';
  ShellCommand := 'xcopy' + ' ' + Application.Location + 'custom\product_icons' + ' '
   + PathToShare + PathToClientAgent + 'product_icons /S /Y /Z /I';
  {Run Command}
  if RunCommand(Shell, [ShellOptions, ShellCommand], ShellOutput) then
  begin
    ShowMessage(ShellOutput);
  end
  else ShowMessage(rsCouldNotSaveIcons);
  ShellCommand := 'xcopy' + ' ' + Application.Location + 'custom\screenshots' + ' '
   + PathToShare + PathToClientAgent + 'screenshots /S /Y /Z /I';
  {Run Command}
  if RunCommand(Shell, [ShellOptions, ShellCommand], ShellOutput) then
  begin
    ShowMessage(ShellOutput);
  end
  else ShowMessage(rsCouldNotSaveScreenshots);

end;

procedure TFormSaveImagesOnShare.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

