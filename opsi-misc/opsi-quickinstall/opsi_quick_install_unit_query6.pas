unit opsi_quick_install_unit_query6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuery6 }

  TQuery6 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    EditNameAdmin: TEdit;
    EditNameIP: TEdit;
    EditNumberIP: TEdit;
    EditPasswordAdmin: TEdit;
    LabelNameAdmin: TLabel;
    LabelNameIP: TLabel;
    LabelNameIP1: TLabel;
    LabelPasswordAdmin: TLabel;
    PanelNameAdmin: TPanel;
    PanelNameIP: TPanel;
    PanelNumberIP: TPanel;
    PanelPasswordAdmin: TPanel;
    procedure BtnNextClick(Sender: TObject);
  private

  public

  end;

var
  Query6: TQuery6;

implementation

uses
  opsi_quick_install_unit_query7;

{$R *.lfm}

{ TQuery6 }

procedure TQuery6.BtnNextClick(Sender: TObject);
begin
  Query7.ShowModal;
  Query6.Close;
end;

end.

