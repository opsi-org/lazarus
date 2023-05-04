unit ocr_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, StdCtrls, Buttons,
  fileutil,
  lazfileutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    CBoxRules: TComboBox;
    Edit1: TEdit;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  ruleslist : TStringlist;
  i : integer;
begin
  try
    ruleslist :=  FindAllFiles(ExtractFileDir(paramstr(0))+PathDelim+'opsi-config-rules.d','*.ini',false);
    for i := 0 to  ruleslist.Count -1 do
      ruleslist[i] := ExtractFileNameOnly(ruleslist[i]);
    CBoxRules.Items.AddStrings(ruleslist);
    CBoxRules.ItemIndex:=0;

  finally
    FreeAndNil(ruleslist)
  end;
end;

end.

