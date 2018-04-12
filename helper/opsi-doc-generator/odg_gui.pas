unit odg_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
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
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

