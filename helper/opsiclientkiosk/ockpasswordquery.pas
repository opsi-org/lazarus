unit OckPasswordQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  {$IFDEF LINUX}
   {add Linux specific units here}
   OckLinux,
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
   {add MacOS specific units here}
   OckMacOS,
  {$ENDIF DARWIN}
  OckImagesToDepot;

type

  { TFormPasswordQuery }

  TFormPasswordQuery = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    EditPassword: TLabeledEdit;
    LabelDescription: TLabel;
    SudoOrRoot: TRadioGroup;
    procedure ButtonCancelClick(Sender: TObject);
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
var
  S:string;
begin
  If SudoOrRoot.Items[SudoOrRoot.ItemIndex] = 'root' then
    RunCommandElevated.Sudo := False
  else if SudoOrRoot.Items[SudoOrRoot.ItemIndex] = 'sudo' then
    RunCommandElevated.Sudo := True;
  S := EditPassword.Text;
  //Insert('',S,pos('',S)); //AnsiQuotedStr(EditPassword.Text,'''');
  RunCommandElevated.Password := S;
  S := '';
  if PasswordCorrect then
  begin
    Close;
    FormSaveImagesOnDepot.Visible := True;
  end
  else
    ShowMessage('Wrong Password');
end;

procedure TFormPasswordQuery.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.

