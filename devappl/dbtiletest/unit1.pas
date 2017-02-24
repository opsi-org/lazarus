unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    FlowPanel1: TFlowPanel;
    procedure FormCreate(Sender: TObject);
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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  mypanel1,mypanel2 : TPanel;
  mylabel1, mylabel2 :TLabel;
begin
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
end;

end.

