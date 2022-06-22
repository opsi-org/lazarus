unit OpsiLinuxInstaller_PasswordForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  MaskEdit, LCLType, cthreads,
  osLinuxRepository,
  osRunCommandElevated;

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
    procedure EditPasswordUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
  public
  var
    clientDataDir, Output: string;
    btnFinishClicked: boolean;
  end;

  TOpsiLinuxInstallerThread = class(TThread)
  public
    FInstallRunCommand: TRunCommandElevated;
    FShellCommand, FClientDataDir, Output: string;
    procedure prepareInstallation; virtual; abstract;
    procedure addRepo; virtual; abstract;
    procedure installConfiged; virtual; abstract;
  public
    constructor Create(password: string; sudo: boolean; shellCommand: string);
  end;

implementation

constructor TOpsiLinuxInstallerThread.Create(password: string; sudo: boolean; shellCommand: string);
begin
  FInstallRunCommand := TRunCommandElevated.Create(password, sudo);
  FShellCommand := shellCommand;

  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TOpsiLinuxInstallerPasswordForm.FormActivate(Sender: TObject);
begin
  // display password as dots
  EditPassword.EchoMode := emPassword;
end;

procedure TOpsiLinuxInstallerPasswordForm.EditPasswordUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  // finishing also possible by pressing enter after writing the password in EditPassword
  // #13 stands for the Enter key
  if UTF8Key = #13 then
    BtnFinish.Click;
end;

end.

