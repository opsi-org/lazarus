unit treescrolldown;

{$MODE Delphi}

interface

uses
(*
  {$IFDEF WINDOWS}
  Windows,
    {$ENDIF WINDOWS}
  Messages,*) SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls;

type
  TFTreeview = class(TForm)
    TreeView1: TTreeView;
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure FormShow(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormActivate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    remotechange: boolean;
  end;

var
  FTreeview: TFTreeview;

implementation

{$R *.lfm}

uses ontop, uibdata;

procedure TFTreeview.TreeView1Click(Sender: TObject);
begin
(*
 if (treeview1.GetNodeAt(x,y) <> nil)
     and (Treeview1.selected <> nil)
     and (treeview1.GetNodeAt(x,y) = Treeview1.selected) then
 begin
  TreeView1Change(sender,treeview1.Selected);
 end;
  *)
end;

procedure TFTreeview.TreeView1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
//var
//  nodestr, hintstr: string;
begin
  if (treeview1.GetNodeAt(x, y) <> nil) and (ontop.commentlist <> nil) then
  begin
    treeview1.Hint := ontop.commentlist.Values[treeview1.GetNodeAt(x, y).Text];
  end;
end;


procedure TFTreeview.FormShow(Sender: TObject);
begin
  datamodule1.debugOut(5, 'start TFTreeview.FormShow');

  Left := DataModule1.getLeftint + Fontop.Edit1.Left;
  Top := DataModule1.getOntopHeight;
  //remotechange := False;

end;

procedure TFTreeview.TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (treeview1.GetNodeAt(x, y) <> nil) and (Treeview1.selected <> nil) and
    (treeview1.GetNodeAt(x, y) = Treeview1.selected) then
  begin
    TreeView1Change(Sender, treeview1.Selected);
  end;
  if (treeview1.GetNodeAt(x, y) <> nil) and (Treeview1.selected <> nil) and
    (ssLeft in shift) then
  begin
    Fontop.edit1.Text := Treeview1.Selected.Text;
    ontop.oldselstr := Fontop.Edit1.Text;
    Visible := False;
  end;
end;

procedure TFTreeview.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  datamodule1.debugOut(5, 'start TFTreeview.Change');
  if not remotechange then
  begin
    datamodule1.debugOut(5, 'start TFTreeview.local.Change');
    if not (Fontop.edit1.Text = Treeview1.Selected.Text) then
    begin
      Fontop.edit1.Text := Treeview1.Selected.Text;
      ontop.oldselstr := Fontop.Edit1.Text;
      Visible := False;
      Fontop.eventhandler(Fontop.Edit1.Text);
    end;
  end;
end;

procedure TFTreeview.FormActivate(Sender: TObject);
begin
  datamodule1.debugOut(5, 'start TFTreeview.FormActivate');
  Left := DataModule1.getLeftint + Fontop.Edit1.Left;
  Top := DataModule1.getOntopHeight;
  //remotechange := false;


end;

end.
