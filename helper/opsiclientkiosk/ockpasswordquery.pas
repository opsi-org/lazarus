unit OckPasswordQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OckLinux;

type

  { TFormPasswordQuery }

  TFormPasswordQuery = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    EditPassword: TLabeledEdit;
    LabelDescription: TLabel;
    SudoOrRoot: TRadioGroup;
    procedure ButtonOKClick(Sender: TObject);
  private

  public
    //RunCommandElevated : TRunCommandElevated;
  end;

var
  FormPasswordQuery: TFormPasswordQuery;

implementation

{$R *.lfm}

{ TFormPasswordQuery }

procedure TFormPasswordQuery.ButtonOKClick(Sender: TObject);
begin
  If SudoOrRoot.Items[SudoOrRoot.ItemIndex] = 'root' then
   OckLinux.RunCommandElevated.Sudo := False
  else if SudoOrRoot.Items[SudoOrRoot.ItemIndex] = 'sudo' then
   OckLinux.RunCommandElevated.Sudo := True;
  OckLinux.RunCommandElevated.Password := EditPassword.Text;
  FormSaveImagesOnDepot.Visible := True;
end;

end.

