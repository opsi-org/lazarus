unit regexpu1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  regexpr;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  RegexObj: TRegExpr;

begin
  RegexObj := TRegExpr.Create;
  RegexObj.Expression := Edit1.Text;
  CheckBox2.Checked:=false;
  try
  if RegexObj.Exec(Edit2.Text) then CheckBox1.Checked:=true
  else CheckBox1.Checked:=false;
  CheckBox2.Checked:=true;
  except
    CheckBox2.Checked:=false;
  end;
  RegexObj.Free;
end;

end.

