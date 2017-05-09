unit notifierform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,contnrs;

type

  { TNform }

  TNform = class(TForm)
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
  Nform: TNform;

implementation

{$R *.lfm}

{ TNform }

procedure TNform.FormCreate(Sender: TObject);
begin
  objlist := TObjectList.Create;
end;

end.

