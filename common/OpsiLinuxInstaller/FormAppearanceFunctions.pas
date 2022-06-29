unit FormAppearanceFunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

procedure CenterFormOnScreen(Form: TForm);
procedure CenterFormOnForm(TopForm: TForm; BottomForm: TForm);
// switch from one form to the next (set position of new form and adjust visibilities)
procedure showForm(newForm: TForm; oldForm: TForm);

implementation

procedure CenterFormOnScreen(Form: TForm);
begin
  Form.Left := Round((Screen.Width - Form.Width) / 2);
  Form.Top := Round((Screen.Height - Form.Height) / 2);
end;

procedure CenterFormOnForm(TopForm: TForm; BottomForm: TForm);
begin
  TopForm.Left := BottomForm.Left + Round(BottomForm.Width / 2) -
    Round(TopForm.Width / 2);
  TopForm.Top := BottomForm.Top + Round(BottomForm.Height / 2) -
    Round(TopForm.Height / 2);
end;

// switch to the next form (set position of new form and adjust visibilities)
procedure showForm(newForm: TForm; oldForm: TForm);
begin
  newForm.Visible := True;

  newForm.Height := oldForm.Height;
  newForm.Left := oldForm.Left;
  newForm.Top := oldForm.Top;
  newForm.Width := oldForm.Width;

  oldForm.Visible := False;
end;

end.
