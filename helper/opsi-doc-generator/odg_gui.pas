unit odg_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  odg_main,
  odg_asciidoc,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    procedure ButtonConvertClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  infilename : string;



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

procedure TForm1.ButtonConvertClick(Sender: TObject);
begin
  convertOslibToAsciidoc(infilename);
  memo2.Lines.Assign(targetlist);
end;

end.

