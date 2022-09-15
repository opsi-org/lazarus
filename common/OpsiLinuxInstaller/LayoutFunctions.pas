unit LayoutFunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

procedure DrawBigBlueLine(Sender: TForm);
procedure DrawSmallRedLine(Sender: TForm);
procedure DecorateForm(Sender: TForm);

implementation

procedure DrawBigBlueLine(Sender: TForm);
var
  PanelBigLine: TPanel;
begin
  // big decoration line at bottom of form in opsi-blue
  PanelBigLine := TPanel.Create(Sender);
  PanelBigLine.Parent := Sender;
  PanelBigLine.ParentColor := False;
  PanelBigLine.BevelOuter := bvNone;
  PanelBigLine.Height := 5;
  PanelBigLine.Top := Sender.Height - PanelBigLine.Height;
  PanelBigLine.Left := 0;
  PanelBigLine.Width := Sender.Width;
  // Opsi-Blau: #006599
  PanelBigLine.Color := TColor($00996500);
  PanelBigLine.BevelColor := TColor($00996500);
end;

procedure DrawSmallRedLine(Sender: TForm);
var
  PanelSmallLine: TPanel;
begin
  // thin decoration line above the big one in opsi-red
  PanelSmallLine := TPanel.Create(Sender);
  PanelSmallLine.Parent := Sender;
  PanelSmallLine.ParentColor := False;
  PanelSmallLine.BevelOuter := bvNone;
  PanelSmallLine.Height := 3;
  PanelSmallLine.Top := Sender.Height - 11;
  PanelSmallLine.Left := 0;
  PanelSmallLine.Width := Sender.Width;
  // Opsi-Rot: #B42554
  PanelSmallLine.Color := TColor($005425B4);
  PanelSmallLine.BevelColor := TColor($005425B4);
end;

procedure DecorateForm(Sender: TForm);
begin
  DrawBigBlueLine(Sender);
  DrawSmallRedLine(Sender);
end;

end.
