unit opsi_quick_install_unit_distr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TDistribution }

  TDistribution = class(TForm)
    BtnBack: TButton;
    BtnNext: TButton;
    EditDistr: TEdit;
    LabelCorrect: TLabel;
    LabelDistr: TLabel;
    PanelDistr: TPanel;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
  var
    GoOn: boolean;
  end;

var
  Distribution: TDistribution;

implementation

uses
  opsi_quick_install_unit_language;

{$R *.lfm}

{ TDistribution }

procedure TDistribution.BtnNextClick(Sender: TObject);
begin
  GoOn := True;
  Distribution.Close;
  //!!!distribution change
end;

procedure TDistribution.FormCreate(Sender: TObject);
begin
  // centering form Distribution nicely on form QuickInstall
  Distribution.Left := QuickInstall.Left + Round(QuickInstall.Width / 2) -
    Round(Width / 2);
  Distribution.Top := QuickInstall.Top + Round(QuickInstall.Height / 2) -
    Round(Height / 2);

  BtnBack.Left := QuickInstall.BtnBack.Left;
  BtnNext.Left := Width - BtnBack.Left - QuickInstall.BtnNextWidth;

  EditDistr.Text := QuickInstall.distroName + ' ' + QuickInstall.distroRelease;
end;

procedure TDistribution.BtnBackClick(Sender: TObject);
begin
  GoOn := False;
  Distribution.Close;
end;

end.
