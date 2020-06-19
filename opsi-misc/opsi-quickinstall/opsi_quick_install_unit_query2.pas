unit opsi_quick_install_unit_query2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TQuery2 }

  TQuery2 = class(TForm)
    BtnBack: TButton;
    BtnNext: TButton;
    BackgrImage: TImage;
    Label1: TLabel;
    LabelRepoKind: TLabel;
    LabelBackend: TLabel;
    PanelUpdate: TPanel;
    PanelRepoKind: TPanel;
    PanelBackend: TPanel;
    RadioBtnFile: TRadioButton;
    RadioBtnMySql: TRadioButton;
    RadioBtnExperimental: TRadioButton;
    RadioBtnStable: TRadioButton;
    RadioBtnTesting: TRadioButton;
    RadioBtnYes: TRadioButton;
    RadioBtnNo: TRadioButton;
    procedure BtnNextClick(Sender: TObject);
  private

  public

  end;

var
  Query2: TQuery2;

implementation

uses
  opsi_quick_install_unit_query3;

{$R *.lfm}

{ TQuery2 }

procedure TQuery2.BtnNextClick(Sender: TObject);
begin
  Query3.ShowModal;
  Query2.Close;
end;

end.

