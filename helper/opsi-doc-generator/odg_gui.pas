unit odg_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf,
  odg_main,
  oslog,
  odg_os_asciidoc,
  odg_py_asciidoc,
  StdCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bsave_ascii_show: TButton;
    ButtonRemoveSelected: TButton;
    ButtonRemoveAll: TButton;
    ButtonOSConvert: TButton;
    ButtonPythonConvert: TButton;
    ButtonSave: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    procedure Bsave_ascii_showClick(Sender: TObject);
    procedure ButtonRemoveSelectedClick(Sender: TObject);
    procedure ButtonOSConvertClick(Sender: TObject);
    procedure ButtonPythonConvertClick(Sender: TObject);
    procedure ButtonRemoveAllClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private

  public

  end;

var
  Form1: TForm1;
  infilename : string;
  filecontent : TStringList;
  tempFile : TStringList;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  caption := 'opsi doc generator Version: '+myversion;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  filecount: Integer;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  for filecount := Low(FileNames) to High(FileNames) do
    ListBox1.Items.Add(FileNames[filecount]);
end;

procedure TForm1.ListBox1SelectionChange(Sender: TObject; User: boolean);
var
  selectedFile : string;
begin
  Memo1.Lines.Clear;
  filecontent.Clear;
  selectedFile := ListBox1.Items[ListBox1.ItemIndex];
  filecontent.LoadFromFile(selectedFile);
  Memo1.Lines.Assign(filecontent);
end;

procedure TForm1.ButtonRemoveSelectedClick(Sender: TObject);
begin
  ListBox1.DeleteSelected;
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
end;

procedure TForm1.ButtonRemoveAllClick(Sender: TObject);
begin
  ListBox1.Clear;
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
end;

procedure TForm1.ButtonOSConvertClick(Sender: TObject);
var
  listboxitem : integer;
begin
  Bsave_ascii_show.Enabled:= false;
  tempFile.Clear;
  sourcelist.Clear;
  for listboxitem := 0 to (ListBox1.Items.Count -1) do
  begin
    tempFile.LoadFromFile(ListBox1.Items[listboxitem]);
    sourcelist.AddStrings(tempFile);
  end;
  convertOslibToAsciidoc();
  memo2.Lines.Assign(targetlist);
end;

procedure TForm1.ButtonPythonConvertClick(Sender: TObject);
var
  listboxitem : integer;
begin
  Bsave_ascii_show.Enabled:= false;
  tempFile.Clear;
  sourcelist.Clear;
  for listboxitem := 0 to (ListBox1.Items.Count -1) do
  begin
    tempFile.LoadFromFile(ListBox1.Items[listboxitem]);
    sourcelist.AddStrings(tempFile);
  end;
  convertPylibToAsciidoc();
  memo2.Lines.Assign(targetlist);
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

procedure TForm1.Bsave_ascii_showClick(Sender: TObject);
begin
  {
  if ExtractFileExt(infilename) = '.opsiscript' then
    convertOslibToAsciidoc(infilename)
  else
    convertPylibToAsciidoc(infilename);
  }
  convertOslibToAsciidoc();
  memo2.Lines.Assign(targetlist);
  save_compile_show(infilename);
end;


initialization
  filecontent := TStringList.Create;
  tempFile := TStringList.Create;

finalization
  filecontent.Free;
  tempFile.Free;
end.

