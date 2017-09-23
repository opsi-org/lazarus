unit notifierform4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtDlgs, ExtCtrls, Buttons, notifierform4a, inifiles;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    ButtonLoadIni: TButton;
    ButtonLabel: TButton;
    ButtonShow: TButton;
    ButtonLoad: TButton;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure ButtonLabelClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonLoadIniClick(Sender: TObject);
    procedure ButtonShowClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonLoadClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Form2.Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    Form2.Show;
  end;
end;

procedure TForm1.ButtonLoadIniClick(Sender: TObject);
var
  myini : TIniFile;
begin
  if OpenDialog1.Execute then
  begin
  myini := TIniFile.Create(OpenDialog1.FileName);
  myini.Free;

  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.ButtonLabelClick(Sender: TObject);
var
  mylabel : TLabel;
begin
  mylabel := TLabel.Create(self);
  mylabel.Caption:= 'Hello World';;
  mylabel.Parent := form2;
end;

procedure TForm1.ButtonShowClick(Sender: TObject);
begin
  form2.show;
end;

end.

