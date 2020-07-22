unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPassword }

  TPassword = class(TForm)
    EditPassword: TEdit;
    LabelPassword: TLabel;
    RadioBtnRoot: TRadioButton;
    RadioBtnSudo: TRadioButton;
  private

  public

  end;

var
  Password: TPassword;

implementation

{$R *.lfm}

end.

