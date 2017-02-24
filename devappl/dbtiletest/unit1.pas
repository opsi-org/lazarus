unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  TPPanel = ^TPanel;
  TPLabel = ^TLabel;


  (*
  TProductPanel = record
    id : TLabel;
    name : TLabel;
    binstall : Tbutton;
  end;
  *)

  TProductPanel = class(TPanel)
    constructor create(id : string);
  end;

  { TForm1 }

  TForm1 = class(TForm)
    FlowPanel1: TFlowPanel;
    procedure FormActivate(Sender: TObject);
    function create_sub_panel :  TPPanel;
    procedure create_sub_label(myp :  TPPanel);
  private
    { private declarations }
  public
    { public declarations }
  end;



var
  Form1: TForm1;

implementation

{$R *.lfm}

var
  myPanelList : TList;
  ppanel:TPPanel;
  plabel: TPLabel;
  labl : TLabel;

  constructor TProductPanel.create(id: string);
begin
  Inherited;
end;

{ TForm1 }

procedure TForm1.create_sub_label(myp :  TPPanel);

begin
  new(plabel);
  plabel^.Parent:= myp^;
  plabel^.Caption:='huhu';
  //repaint;
end;


function TForm1.create_sub_panel :  TPPanel;

begin
  new(result);
  result^ := TPanel.Create(FlowPanel1);
  result^.Parent:= FlowPanel1;
  result^.Width:=100;
  result^.Height:=100;
  (*
  new(plabel);
  plabel^.Parent:= result^;
  plabel^.Caption:='huhu';
  *)
  //repaint;
end;


procedure TForm1.FormActivate(Sender: TObject);
var
  mypanel1,mypanel2, panel : TPanel;
  mylabel1, mylabel2 :TLabel;
  myList : TList;
begin
  myList := TList.Create;
  myList.Add(create_sub_panel);
  create_sub_label(myList[0]);
  myList.Add(create_sub_panel);
  repaint;



  (*
  new(ppanel);
  ppanel^ := TPanel.Create(FlowPanel1);
  ppanel^.Parent:= FlowPanel1;
  ppanel^.
  *)
  (*
  mypanel1 := TPanel.Create(FlowPanel1);
  mypanel1.Parent:= FlowPanel1;
  mypanel1.Width:=100;
  mypanel1.Height:=100;
  mylabel1 := TLabel.Create(mypanel1);
  mylabel1.Parent:= mypanel1;
  mylabel1.Caption:='huhu';
  mypanel1.Controls[0].Caption:='haha';
  mypanel1.Repaint;

  mypanel2 := TPanel.Create(FlowPanel1);
  mypanel2.Parent:= FlowPanel1;
  //mypanel.Top:=5;
  //mypanel.Left:=5;
  mypanel2.Width:=100;
  mypanel2.Height:=100;
  mypanel2.Repaint;
  *)
end;

end.

