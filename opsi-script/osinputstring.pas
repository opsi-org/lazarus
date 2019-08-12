unit osinputstring;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  MaskEdit, StdCtrls, EditBtn;

type

  { TFinputstring }

  TFinputstring = class(TForm)
    BitBtn1: TBitBtn;
    EditButton1: TEditButton;
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    procedure EditButton1ButtonClick(Sender: TObject);
  private

  public

  end;

var
  Finputstring: TFinputstring;

implementation

{$R *.lfm}

{ TFinputstring }



procedure TFinputstring.EditButton1ButtonClick(Sender: TObject);
begin
   if EditButton1.EchoMode = emNormal then EditButton1.EchoMode := emPassword
   else EditButton1.EchoMode := emNormal;
end;

end.

