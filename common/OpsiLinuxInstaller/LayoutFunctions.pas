unit LayoutFunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

procedure DecorateForm(Sender: TForm);

implementation

procedure DrawRedLine(Sender: TForm);
var
  PanelRedLine: TPanel;
begin
  // thin decoration line above the big one in opsi-red
  PanelRedLine := TPanel.Create(Sender);
  PanelRedLine.ParentColor := False;
  PanelRedLine.BevelOuter := bvNone;
  PanelRedLine.Height := 8;
  PanelRedLine.Top := Sender.Height - 10;
  PanelRedLine.Left := 0;
  PanelRedLine.Width := Sender.Width;
  // opsi red: #cb1e58
  PanelRedLine.Color := TColor($00581ECB);
  PanelRedLine.BevelColor := TColor($00581ECB);
  PanelRedLine.Parent := Sender;
end;

procedure DecorateForm(Sender: TForm);
begin
  DrawRedLine(Sender);
end;

end.
