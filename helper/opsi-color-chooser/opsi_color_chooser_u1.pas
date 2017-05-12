unit opsi_color_chooser_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ColorBox,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ColorBox1: TColorBox;
    ColorDialog1: TColorDialog;
    EditRGB: TEdit;
    EditAlias: TEdit;
    EditHex: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
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

function TColorToHex(Color: TColor): string;
begin
  Result := '$' +
    { red value }
    IntToHex(blue(Color), 2) +
    { green value }
    IntToHex(green(Color), 2) +
    { blue value }
    IntToHex(red(Color), 2);
end;

function TColorToRGB(Color: TColor): string;
begin
  Result :=
    { red value }
    IntToStr(red(Color)) + ',' +
    { green value }
    IntToStr(green(Color)) + ',' +
    { blue value }
    IntToStr(blue(Color));
end;



procedure TForm1.Button1Click(Sender: TObject);
var
  mycol: Tcolor;
begin
  if ColorDialog1.Execute then
  begin
    mycol := ColorDialog1.Color;
    EditAlias.Text := ColorToString(mycol);
    EditHex.Text := TColorToHex(mycol);
    EditRGB.Text := TColorToRGB(mycol);
  end;
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
var
  mycol: Tcolor;
begin
  mycol := ColorBox1.Selected;
  EditAlias.Text := ColorToString(mycol);
  EditHex.Text := TColorToHex(mycol);
  EditRGB.Text := TColorToRGB(mycol);
end;

end.
