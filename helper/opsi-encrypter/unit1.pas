unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  oscrypt;

const
  MYKEY = '2vafbqpqza2hcimyqn5hswldan27jww7qiwroyv9rcqieglknb6is3jxeitop4wu';

type

  { TForm1 }

  TForm1 = class(TForm)
    EditIn1: TEdit;
    EditIn2: TEdit;
    cryptout: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure EditIn2Change(Sender: TObject);
    procedure EditIn2EditingDone(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.EditIn2Change(Sender: TObject);
begin
  If EditIn2.Text = EditIn1.Text then
  begin
    Label1.Font.Color := clGreen;
    Label1.Caption:= 'Match';
    //RadioButtonMatch.Color:= clgreen
  end
  else
  begin
    //RadioButtonMatch.Color:= clred;
    Label1.Font.Color := clRed;
    Label1.Caption:= 'No Match';
  end;
end;

procedure TForm1.EditIn2EditingDone(Sender: TObject);
begin
  If EditIn2.Text = EditIn1.Text then
  cryptout.Caption:= encryptStringBlow(MYKEY, EditIn2.Text);
end;

end.

