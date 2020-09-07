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
    procedure FormActivate(Sender: TObject);
  private

  public
  var
    GoOn: boolean;
  end;

var
  Distribution: TDistribution;

implementation

uses
  opsi_quick_install_unit_language, osDistributionInfo,
  opsi_quick_install_resourcestrings;

{$R *.lfm}

{ TDistribution }

procedure TDistribution.BtnNextClick(Sender: TObject);
begin
  GoOn := True;
  Distribution.Close;
  // if distribution was edited:
  if EditDistr.Text <> QuickInstall.distroName + ' ' + QuickInstall.distroRelease then
  begin
    // set new distribution name and release
    // function Copy is 1-based like Pos
    QuickInstall.distroName := Copy(EditDistr.Text, 1, Pos(' ', EditDistr.Text) - 1);
    QuickInstall.distroRelease :=
      Copy(EditDistr.Text, Pos(' ', EditDistr.Text) + 1, Length(EditDistr.Text) -
      Pos(' ', EditDistr.Text));
  end;
  with QuickInstall do
  begin
    //ShowMessage(distroName);
    //ShowMessage(distroRelease);
    DistrInfo.SetInfo(distroName, distroRelease);
    //ShowMessage(DistrInfo.DistrUrlPart);
  end;
end;

procedure TDistribution.FormActivate(Sender: TObject);
begin
  // centering form Distribution nicely on form QuickInstall
  Distribution.Left := QuickInstall.Left + Round(QuickInstall.Width / 2) -
    Round(Width / 2);
  Distribution.Top := QuickInstall.Top + Round(QuickInstall.Height / 2) -
    Round(Height / 2);

  BtnBack.Left := QuickInstall.BtnBack.Left;
  //ShowMessage(QuickInstall.BtnNextWidth.ToString);
  BtnNext.Left := Width - BtnBack.Left - QuickInstall.BtnNextWidth;

  EditDistr.Text := QuickInstall.distroName + ' ' + QuickInstall.distroRelease;

  // text by resourcestrings
  LabelDistr.Caption := rsDistr;
  LabelCorrect.Caption := rsCorrect;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TDistribution.BtnBackClick(Sender: TObject);
begin
  GoOn := False;
  Distribution.Close;
end;

end.
