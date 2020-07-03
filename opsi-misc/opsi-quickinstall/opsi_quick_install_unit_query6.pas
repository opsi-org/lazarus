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
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Query6: TQuery6;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query7;

{$R *.lfm}

{ TQuery6 }

procedure TQuery6.BtnNextClick(Sender: TObject);
begin
  Query7.Visible := True;

  Query7.Height := Height;
  Query7.Left := Left;
  Query7.Top := Top;
  Query7.Width := Width;

  Query7.BtnBack.Left := BtnBack.Left;
  Query7.BtnBack.Top := BtnBack.Top;
  Query7.BtnFinish.Left := BtnNext.Left;
  Query7.BtnFinish.Top := BtnNext.Top;

  Visible := False;
end;

procedure TQuery6.FormActivate(Sender: TObject);
var
  compIndex: integer;
begin
  for compIndex:=0 to ComponentCount-1 do
  begin
    if Components[compIndex].ClassName = 'TPanel' then
      begin
         (Components[compIndex] as TPanel).Left:= QuickInstall.panelLeft;
      end;
  end;

  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);
end;

procedure TQuery6.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Query5_dhcp.Close;
end;

procedure TQuery6.BtnBackClick(Sender: TObject);
begin
  Query5_dhcp.Visible := True;

  Query5_dhcp.Height := Height;
  Query5_dhcp.Left := Left;
  Query5_dhcp.Top := Top;
  Query5_dhcp.Width := Width;

  Query5_dhcp.BtnBack.Left := BtnBack.Left;
  Query5_dhcp.BtnBack.Top := BtnBack.Top;
  Query5_dhcp.BtnNext.Left := BtnNext.Left;
  Query5_dhcp.BtnNext.Top := BtnNext.Top;

  Visible := False;
end;

end.

