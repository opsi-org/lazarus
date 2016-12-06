unit showkeyu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  oskeyboard;

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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


procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  memo1.Append(shiftStateToString(Shift)+VKtoString(key));

end;

end.

