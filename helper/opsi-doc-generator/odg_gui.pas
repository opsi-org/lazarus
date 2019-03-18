unit odg_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf,
  odg_main,
  odg_asciidoc, //oslog,
  odg_pyasciidoc,
  StdCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bsave_ascii_show: TButton;
    ButtonRemoveSelected: TButton;
    ButtonRemoveAll: TButton;
    ButtonConvert: TButton;
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
    procedure ButtonConvertClick(Sender: TObject);
    //procedure ButtonOpenClick(Sender: TObject);
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

{
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

  // file browser lazarus pascal
  // https://www.youtube.com/watch?v=Vee9x90Qm4E
  // http://forum.lazarus.freepascal.org/index.php?topic=29670.0
  // http://www.delphitips.net/2007/10/16/directory-structure-with-associated-icons-and-file-info-in-listview/
  // https://stackoverflow.com/questions/2021139/listing-files-on-directory-on-a-tlistview
  // http://lazplanet.blogspot.com/2013/07/how-to-list-files-in-folder.html
  // https://www.swissdelphicenter.ch/en/showcode.php?id=421
  // https://www.tek-tips.com/viewthread.cfm?qid=1548839
  // http://lazplanet.blogspot.com/2013/05/drag-drop-files-lazarus-form.html

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
}

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
  filecontent : TStringList;
begin
  Memo1.Lines.Clear;
  filecontent := TStringList.Create;
  selectedFile := ListBox1.Items[ListBox1.ItemIndex];
  filecontent.LoadFromFile(selectedFile);
  Memo1.Lines.Assign(filecontent);
end;

{
procedure TForm1.ButtonConvertClick(Sender: TObject);
begin
  Bsave_ascii_show.Enabled:= true;
  convertOslibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
end;
}

procedure TForm1.ButtonConvertClick(Sender: TObject);
var
  listboxitem : integer;
  tempFile : TStringList;
begin
  Bsave_ascii_show.Enabled:= true;
  tempFile := TStringList.Create;
  tempFile.Clear;
  sourcelist.Clear;
  for listboxitem := 0 to (ListBox1.Items.Count -1) do
  begin
    tempFile.LoadFromFile(ListBox1.Items[listboxitem]);
    sourcelist.AddStrings(tempFile);
  end;
  convertOslibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
end;

{
procedure TForm1.ButtonPythonConvertClick(Sender: TObject);
begin
  Bsave_ascii_show.Enabled:= false;
  convertPylibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
end;
}

procedure TForm1.ButtonPythonConvertClick(Sender: TObject);
var
  listboxitem : integer;
  tempFile : TStringList;
begin
  Bsave_ascii_show.Enabled:= false;
  tempFile := TStringList.Create;
  tempFile.Clear;
  sourcelist.Clear;
  for listboxitem := 0 to (ListBox1.Items.Count -1) do
  begin
    tempFile.LoadFromFile(ListBox1.Items[listboxitem]);
    sourcelist.AddStrings(tempFile);
  end;
  convertPylibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
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


procedure TForm1.Bsave_ascii_showClick(Sender: TObject);
begin
  {
  if ExtractFileExt(infilename) = '.opsiscript' then
    convertOslibToAsciidoc(infilename)
  else
    convertPylibToAsciidoc(infilename);
  }
  convertOslibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
  save_compile_show(infilename);
end;


initialization

end.

