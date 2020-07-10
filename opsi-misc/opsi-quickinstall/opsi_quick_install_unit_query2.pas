unit opsi_quick_install_unit_query2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TQuery2 }

  TQuery2 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
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
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Query2: TQuery2;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query3;

{$R *.lfm}

{ TQuery2 }

procedure TQuery2.BtnNextClick(Sender: TObject);
begin
  showForm(Query3, self);
  Query3.BtnBack.Left := BtnBack.Left;
  Query3.BtnBack.Top := BtnBack.Top;
  Query3.BtnNext.Left := BtnNext.Left;
  Query3.BtnNext.Top := BtnNext.Top;
end;

procedure TQuery2.FormActivate(Sender: TObject);
var
  compIndex: integer;
begin
  for compIndex := 0 to ComponentCount - 1 do
  begin
    if Components[compIndex].ClassName = 'TPanel' then
    begin
      (Components[compIndex] as TPanel).Left := QuickInstall.panelLeft;
    end;
  end;

  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
end;

procedure TQuery2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query.Close;
end;

procedure TQuery2.BtnBackClick(Sender: TObject);
begin
  showForm(Query, self);
  Query.BtnBack.Left := BtnBack.Left;
  Query.BtnBack.Top := BtnBack.Top;
  Query.BtnNext.Left := BtnNext.Left;
  Query.BtnNext.Top := BtnNext.Top;
end;

end.

