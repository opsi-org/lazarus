unit opsi_quick_install_unit_query3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TQuery3 }

  TQuery3 = class(TForm)
    BackgrImage: TImage;
    BtnBack: TButton;
    BtnNext: TButton;
    LabelDhcp: TLabel;
    LabelReboot: TLabel;
    LabelOpsiProducts: TLabel;
    MemoOpsiProducts: TMemo;
    PanelDhcp: TPanel;
    PanelReboot: TPanel;
    PanelOpsiProducts: TPanel;
    RadioBtnNo: TRadioButton;
    RadioBtnDhcpNo: TRadioButton;
    RadioBtnYes: TRadioButton;
    RadioBtnDhcpYes: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Query3: TQuery3;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query4;

{$R *.lfm}

{ TQuery3 }

procedure TQuery3.BtnNextClick(Sender: TObject);
begin
  Query4.Visible := True;

  Query4.Height := Height;
  Query4.Left := Left;
  Query4.Top := Top;
  Query4.Width := Width;

  Query4.BtnBack.Left := BtnBack.Left;
  Query4.BtnBack.Top := BtnBack.Top;
  Query4.BtnNext.Left := BtnNext.Left;
  Query4.BtnNext.Top := BtnNext.Top;

  Visible := False;
end;

procedure TQuery3.FormActivate(Sender: TObject);
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

procedure TQuery3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query2.Close;
end;

procedure TQuery3.BtnBackClick(Sender: TObject);
begin
  Query2.Visible := True;

  Query2.Height := Height;
  Query2.Left := Left;
  Query2.Top := Top;
  Query2.Width := Width;

  Query2.BtnBack.Left := BtnBack.Left;
  Query2.BtnBack.Top := BtnBack.Top;
  Query2.BtnNext.Left := BtnNext.Left;
  Query2.BtnNext.Top := BtnNext.Top;

  Visible := False;
end;

end.
