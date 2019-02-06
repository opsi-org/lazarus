unit odg_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf,
  odg_main,
  odg_asciidoc, oslog,
  odg_pyasciidoc,
  StdCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bsave_ascii_show: TButton;
    ButtonConvert: TButton;
    ButtonPythonConvert: TButton;
    ButtonSave: TButton;
    ButtonOpen: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    procedure Bsave_ascii_showClick(Sender: TObject);
    procedure ButtonConvertClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonPythonConvertClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  infilename : string;


implementation

{$R *.lfm}

{ TForm1 }

{
procedure TForm1.ButtonOpenClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  if OpenDialog1.Execute then
  begin
    infilename := OpenDialog1.FileName;
    sourcelist.LoadFromFile(infilename);
    memo1.Lines.Assign(sourcelist);
    if ExtractFileExt(infilename) = '.opsiscript' then
    begin
      ButtonPythonConvert.enabled:=false;
      ButtonConvert.enabled:=true;
    end
    else
    begin
      ButtonConvert.enabled:=false;
      ButtonPythonConvert.enabled:=true;
    end
  end;
end;

}

procedure TForm1.ButtonOpenClick(Sender: TObject);
var
  filecount, totallines : integer;
  tempfile : TStringList;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  tempfile := TStringList.Create;
  tempfile.Clear;
  sourcelist.Clear;

  OpenDialog1.Options:= [ofAllowMultiSelect, ofFileMustExist];

  if OpenDialog1.Execute then
  begin
    for filecount:= 0 to OpenDialog1.Files.Count-1 do
    begin
      tempfile.LoadFromFile(OpenDialog1.Files[filecount]);
      //sourcelist.Append(tempfile.Text);
      sourcelist.AddStrings(tempfile);
    end;

    totallines:= sourcelist.Count;

    memo1.Lines.Assign(sourcelist);
    ButtonConvert.enabled:=true;
    ButtonPythonConvert.enabled:=true;

  end;
end;


procedure TForm1.ButtonSaveClick(Sender: TObject);
var
  savefilename : string;
begin
  if SaveDialog1.Execute then
  begin
    savefilename := SaveDialog1.FileName;
    targetlist.SaveToFile(savefilename);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  caption := 'opsi doc generator Version: '+myversion;
end;

procedure TForm1.ButtonConvertClick(Sender: TObject);
begin
  convertOslibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
end;

procedure TForm1.ButtonPythonConvertClick(Sender: TObject);
begin
  convertPylibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
end;


procedure TForm1.Bsave_ascii_showClick(Sender: TObject);
begin
  if ExtractFileExt(infilename) = '.opsiscript' then
    convertOslibToAsciidoc(infilename)
  else
    convertPylibToAsciidoc(infilename);

  memo2.Lines.Assign(targetlist);
  save_compile_show(infilename);
end;




initialization

end.

