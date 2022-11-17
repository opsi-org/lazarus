unit OpsiLinuxInstaller_PasswordForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  MaskEdit, LCLType, cthreads,
  osRunCommandElevated,
  OpsiLinuxInstaller_LanguageObject;

type
  TOpsiLinuxInstallerPasswordForm = class(TForm)
    BtnBack: TButton;
    BtnFinish: TButton;
    CheckBoxShowPassword: TCheckBox;
    EditPassword: TEdit;
    LabelRights: TLabel;
    LabelPassword: TLabel;
    PanelPassword: TPanel;
    PanelRights: TPanel;
    RadioBtnRoot: TRadioButton;
    RadioBtnSudo: TRadioButton;
    procedure FormActivate(Sender: TObject); virtual;
    procedure BtnBackClick(Sender: TObject); virtual; abstract;
    procedure BtnFinishClick(Sender: TObject); virtual; abstract;

    procedure EditPasswordUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure CheckBoxShowPasswordChange(Sender: TObject);
    function IsPasswordCorrect: boolean;

    procedure CloseProject; virtual; abstract;
    procedure FormClose(Sender: TObject); virtual; abstract;


  public
    clientDataDir: string;
  protected
    Output: string;
    btnFinishClicked: boolean;
  end;


resourcestring
  rsBack = ' < back ';
  rsFinish = ' finish ';

  rsRights = 'Authentication is required for the installation. Are you identified as root or per sudo?';
  rsPassword = 'Password';
  rsShowPassword = 'Show password';
  rsWrongPassword = 'Authentication failed.' + #10 + 'Please check your password!';


implementation

procedure TOpsiLinuxInstallerPasswordForm.FormActivate(Sender: TObject);
begin
  // display password as dots
  EditPassword.EchoMode := emPassword;

  Language.TranslateResourceStrings('OpsiLinuxInstaller_PasswordForm',
    'PasswordForm.' + Language.Abbreviation + '.po');

  // text by resourcestrings
  Caption := rsPassword;
  LabelRights.Caption := rsRights;
  LabelPassword.Caption := rsPassword + ':';
  CheckBoxShowPassword.Caption := rsShowPassword;
  BtnBack.Caption := rsBack;
  BtnFinish.Caption := rsFinish;
end;

procedure TOpsiLinuxInstallerPasswordForm.EditPasswordUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  // finishing also possible by pressing enter after writing the password in EditPassword
  // #13 stands for the Enter key
  if UTF8Key = #13 then
    BtnFinish.Click;
end;

procedure TOpsiLinuxInstallerPasswordForm.CheckBoxShowPasswordChange(Sender: TObject);
begin
  // Change whether the password is shown as dots or text:
  // EditPassword.Text still gets the real text
  if CheckBoxShowPassword.Checked then
    // TMaskEdit can't do this!
    EditPassword.EchoMode := emNormal
  else
    EditPassword.EchoMode := emPassword;
end;

function TOpsiLinuxInstallerPasswordForm.IsPasswordCorrect: boolean;
var
  TestCommand: TRunCommandElevated;
begin
  Result := True;
  {$IFNDEF DARWIN}
  TestCommand := TRunCommandElevated.Create(EditPassword.Text, RadioBtnSudo.Checked);
  if TestCommand.Run('mkdir /root/testDir', Output) then
  begin
    TestCommand.Run('rm -rf /root/testDir', Output);
  end
  else
  begin
    ShowMessage(rsWrongPassword);
    Result := False;
  end;
  FreeAndNil(TestCommand);
  {$ENDIF}
end;

end.
