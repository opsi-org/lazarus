unit oslistedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ExtCtrls, StdCtrls, Buttons;

type

  { TFListedit }

  TFListedit = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ValueListEditor1: TValueListEditor;
  private
    { private declarations }
  public
    { public declarations }
  end;

  procedure checkMapGUI(var list : Tstringlist);   overload;
  procedure checkMapGUI(var list : Tstringlist; focusrow : integer);   overload;


var
  FListedit: TFListedit;

implementation

{$R *.lfm}

procedure checkMapGUI(var list : Tstringlist);
begin
  checkMapGUI(list, 1);
end;

procedure checkMapGUI(var list : Tstringlist; focusrow : integer);
var
  i, int1 : integer;
  str1 : string;
begin
  FListedit.ValueListEditor1.Strings.Clear;
  FListedit.ValueListEditor1.Clean;
  int1 :=  FListedit.ValueListEditor1.RowCount;
  for i := int1-1 downto 1 do FListedit.ValueListEditor1.DeleteRow(i);
  int1 :=  list.Count;
  for i := 0 to int1-1 do FListedit.ValueListEditor1.InsertRow(list.Names[i],list.ValueFromIndex[i],true);
  FListedit.ValueListEditor1.Row:=focusrow;
  //int1 :=  FListedit.ValueListEditor1.Strings.Count;
  //for i := 0 to int1-1 do str1 :=  FListedit.ValueListEditor1.Strings[i];
  //FListedit.ValueListEditor1.Strings.AddStrings(list);
  FListedit.ValueListEditor1.Height:=list.Count*46;
  if mrOK = FListedit.ShowModal then
  begin
     list.Clear;
     list.AddStrings(FListedit.ValueListEditor1.Strings);
  end;
end;

end.

