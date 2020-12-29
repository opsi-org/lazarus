unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PairSplitter,
  StdCtrls, Buttons,
  LazUTF8;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    MemoOut: TMemo;
    MemoIn: TMemo;
    OpenDialog1: TOpenDialog;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  OpenDialog1.Execute;
  MemoIn.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  line : string;
  i : integer;
begin
  MemoOut.Clear;
  for i := 0 to MemoIn.Lines.Count -1 do
  begin
    line := MemoIn.Lines[i];
    MemoOut.Lines.Add(line);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  line : string;
  i : integer;
begin
  MemoOut.Clear;
  for i := 0 to MemoIn.Lines.Count -1 do
  begin
    line := MemoIn.Lines[i];
    if FindInvalidUTF8Codepoint(PChar(line),length(line)) >= 0 then
      line := 'non utf8char found';;

    MemoOut.Lines.Add(line);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  line : string;
  i : integer;
begin
  MemoOut.Clear;
  for i := 0 to MemoIn.Lines.Count -1 do
  begin
    line := MemoIn.Lines[i];
    UTF8FixBroken(line);
    MemoOut.Lines.Add(line);
  end;
end;

end.

