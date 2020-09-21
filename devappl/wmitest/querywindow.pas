unit querywindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TQueryForm }

  TQueryForm = class(TForm)
    EditPropertiesList: TEdit;
    EditNamespace: TEdit;
    EditSelectedClass: TEdit;
    LabelPropertiesList: TLabel;
    LabelNamespace: TLabel;
    LabelSelectedProperties: TLabel;
    LabelSelectedClass: TLabel;
    LabelQueryProperties: TLabel;
    MemoQuerySelectedProperties: TMemo;
    MemoQueryProperties: TMemo;

    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  QueryForm: TQueryForm;

implementation

{$R *.lfm}

{ QueryForm }

procedure TQueryForm.FormCreate(Sender: TObject);
begin
  QueryForm.EditNamespace.Clear;
  QueryForm.EditSelectedClass.Clear;
  QueryForm.EditPropertiesList.Clear;
  QueryForm.MemoQuerySelectedProperties.Clear;
  QueryForm.MemoQueryProperties.Clear;
end;

end.

