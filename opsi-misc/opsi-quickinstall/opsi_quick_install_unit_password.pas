unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPassword }

  TPassword = class(TForm)
    BtnBack: TButton;
    BtnFinish: TButton;
    CheckBoxShowPassword: TCheckBox;
    EditPassword: TEdit;
    LabelRights: TLabel;
    LabelPassword: TLabel;
    RadioBtnRoot: TRadioButton;
    RadioBtnSudo: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CheckBoxShowPasswordChange(Sender: TObject);
  private

  public

  end;

var
  Password: TPassword;

implementation

{$R *.lfm}

{ TPassword }

procedure TPassword.FormActivate(Sender: TObject);
begin
  EditPassword.EchoMode:=emPassword;
end;

procedure TPassword.BtnBackClick(Sender: TObject);
begin
  Password.Close;
end;

procedure TPassword.CheckBoxShowPasswordChange(Sender: TObject);
begin
  if CheckBoxShowPassword.Checked then
    EditPassword.EchoMode:=emNormal
  else
    EditPassword.EchoMode:=emPassword;
  // EditPassword.Text still gets the real text
  //ShowMessage(EditPassword.Text);
end;

end.

