unit opsi_quick_install_unit_query7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuery7 }

  TQuery7 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnFinish: TButton;
    EditPasswordMasterAdmin: TEdit;
    Label1: TLabel;
    LabelPasswordMasterAdmin: TLabel;
    Panel1: TPanel;
    PanelPasswordMasterAdmin: TPanel;
    RadioBtnNo: TRadioButton;
    RadioBtnYes: TRadioButton;
    procedure BtnFinishClick(Sender: TObject);
  private

  public

  end;

var
  Query7: TQuery7;

implementation

{$R *.lfm}

{ TQuery7 }

procedure TQuery7.BtnFinishClick(Sender: TObject);
begin
  Query7.Close;
end;

end.

