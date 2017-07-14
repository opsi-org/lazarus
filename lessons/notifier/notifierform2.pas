unit notifierform2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtDlgs, ExtCtrls, notifierform2a;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure Button1Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Form2.Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    Form2.Show;
  end;
end;

end.

