unit odg_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  fileinfo,
  winpeimagereader, // {need this for reading exe info}
  elfreader, // {needed for reading ELF executables}
  LCLIntf,
  Process,
  odg_main,
  odg_asciidoc,
  odg_pyasciidoc,
  StdCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bsave_ascii_show: TButton;
    ButtonPythonConvert: TButton;
    ButtonConvert: TButton;
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
  FileVerInfo : TFileVersionInfo;
  myversion : string;



implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    infilename := OpenDialog1.FileName;
    sourcelist.LoadFromFile(infilename);
    memo1.Lines.Assign(sourcelist);
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
  convertOslibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
  save_compile_show(infilename);
end;


initialization
 //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
 FileVerInfo := TFileVersionInfo.Create(nil);
 try
   FileVerInfo.FileName := ParamStr(0);
   FileVerInfo.ReadFileInfo;
   myversion := FileVerInfo.VersionStrings.Values['FileVersion'];
 finally
   FileVerInfo.Free;
 end;

end.

