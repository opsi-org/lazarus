unit resultwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TResultForm }

  TResultForm = class(TForm)
    EditQuery: TEdit;
    LabelQueryResult: TLabel;
    LabelQuery: TLabel;
    MemoQueryResult: TMemo;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  ResultForm: TResultForm;

implementation


{$R *.lfm}

{ TResultForm }

procedure TResultForm.FormCreate(Sender: TObject);
begin
  ResultForm.EditQuery.Clear;
  ResultForm.MemoQueryResult.Clear;
end;

end.

