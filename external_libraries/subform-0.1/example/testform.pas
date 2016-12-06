unit testform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  subform, Buttons, StdCtrls, MaskEdit, CheckLst, Grids, PairSplitter, ColorBox,
  ComCtrls, Spin, Arrow, Calendar, EditBtn, FileCtrl, Chart, ButtonPanel,
  DbCtrls, DBGrids, Dbf, DB;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Datasource1: TDatasource;
    DBEdit1: TDBEdit;
    Dbf1: TDbf;
    DBGrid1: TDBGrid;
    DBText1: TDBText;
    SubForm1: TSubForm;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DBEdit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

Uses LCLType;

procedure TForm1.Button2Click(Sender: TObject);
begin
//  SubForm1.TestProc;
  If SubForm1.Direction = sfVertical then
    SubForm1.Direction := sfHorizontal
  else
    SubForm1.Direction := sfVertical
end;

procedure TForm1.DBEdit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_RETURN then
  Begin
    if (Sender as TDBEdit).DataSource.DataSet.State in dsEditModes then
      (Sender as TDBEdit).DataSource.DataSet.Post
  End
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  DBF1.FilePath := ExtractFilePath(ParamStr(0));
  DBF1.Open
end;

procedure TForm1.Button1Click(Sender: TObject);
begin                                     (*
  Application.MessageBox(PChar('My Parent''s name is '+(Sender as TButton).Parent.Name+
    '.'#10#13+'My parent''s tag is '+IntToStr((Sender as TButton).Parent.Tag)+'.'{#10#13+
    'My parent''s xxx is'}),'Debug',0); *)
//  This would be a really nice touch, but needs fixing up.
//  SubForm1.Post
end;

initialization
  {$I testform.lrs}

end.

