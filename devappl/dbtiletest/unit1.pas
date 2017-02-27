unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, DbCtrls, Buttons;

type


  TProductPanel = class(TFlowPanel)
    LabelId : TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    //FidCaption : String;
    //function getIdCaption : string;
    //procedure setIdCaption(str: string);
  public
    { public declarations }
    constructor create(TheOwner : TWincontrol);
    //property idCaption: string read getIdCaption write setIdCaption;
  end;

  TPanels = Array of TProductPanel;

  { TForm1 }

  TForm1 = class(TForm)
    FlowPanel1: TFlowPanel;
    Memo1: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
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
  labl : TLabel;

  { TProductPanel }

constructor TProductPanel.Create(TheOwner : TWincontrol);
begin
  Inherited Create(theOwner);
  parent := theOwner;
  width:= 100;
  Height:=100;;
  labelId := TLabel.Create(self);
  LabelId.Parent := self;
  LabelId.Caption:='id';
  //LabelId.Width:=width;
  LabelId.Alignment:=taCenter;
  labelId.Align:=alTop;
  Button1 := TButton.Create(self);
  Button1.Parent := self;
  Button1.Caption:='btn';
  Button1.Width:=80;
  Button1.Height:=20;
  Button1.OnClick:= @Button1Click;
end;

  (*
function TProductPanel.getIdCaption : string;
begin

end;

procedure TProductPanel.setIdCaption(str: string);
begin

end;
*)

{ TForm1 }


procedure TForm1.FormActivate(Sender: TObject);
var
  mypanel1,mypanel2, panel : TPanel;
  mylabel1, mylabel2 :TLabel;
  myList : TList;
  myppanel : TProductPanel;
  mypanelarray :  TPanels;
  i : integer;
begin
  panel := TPanel.Create(nil);
  panel.Parent := FlowPanel1;
  panel.Width:=100;
  panel.Height:=100;

  SetLength(mypanelarray,10);
  mypanelarray[0] :=  TProductPanel.create(FlowPanel1);
  mypanelarray[0].LabelId.Caption:='hihu';
  repaint;
  Application.ProcessMessages;
  sleep(1000);
  mypanelarray[0].LabelId.Caption:='hohu';
  mypanelarray[1] :=  TProductPanel.create(FlowPanel1);
  mypanelarray[1].LabelId.Caption:='hehe';
  for i := 2 to 5 do
  begin
    mypanelarray[i] :=  TProductPanel.create(FlowPanel1);
    mypanelarray[i].LabelId.Caption:= 'P'+inttostr(i);
    mypanelarray[i].Caption:=IntToStr(i);
  end;
  repaint;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin

end;

procedure TProductPanel.Button1Click(Sender: TObject);
begin
   form1.Memo1.Append('click: '+TButton(sender).Parent.Caption);
end;

end.

