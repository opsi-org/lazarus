unit notifierform5a;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,contnrs;

type

  { TForm2 }

  TForm2 = class(TForm)
    Image1: TImage;
    //objlist : TObjectList;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    objlist : TObjectList;
  public
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  objlist := TObjectList.Create;
end;

end.

