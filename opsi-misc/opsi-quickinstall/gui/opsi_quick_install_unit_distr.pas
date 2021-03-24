unit opsi_quick_install_unit_distr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TDistribution }

  TDistribution = class(TForm)
    BtnBack: TButton;
    BtnNext: TButton;
    EditDistr: TEdit;
    InfoDistribution: TImage;
    LabelCorrect: TLabel;
    LabelDistr: TLabel;
    PanelDistr: TPanel;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
  public
  var
    // Distribution.GoOn tells TQuickInstall whether in TDistribution the next or
    // the back button was clicked, i.e. whether to go on to the next form or to
    // stay on TQuickInstall after TDistribution closed.
    GoOn: boolean;
  end;

var
  Distribution: TDistribution;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsi_quick_install_data,
  opsi_quick_install_unit_language,
  osDistributionInfo,
  osLinuxRepository;

{$R *.lfm}

{ TDistribution }

procedure TDistribution.FormActivate(Sender: TObject);
begin
  // centering TDistribution nicely on TQuickInstall
  Distribution.Left := QuickInstall.Left + Round(QuickInstall.Width / 2) -
    Round(Width / 2);
  Distribution.Top := QuickInstall.Top + Round(QuickInstall.Height / 2) -
    Round(Height / 2);

  // position buttons here because of different layout (size of TDistribution)
  BtnBack.Left := QuickInstall.BtnBack.Left;
  BtnNext.Left := Width - BtnBack.Left - QuickInstall.BtnNextWidth;
  // we have one InfoImage
  setInfoBasics(InfoDistribution);
  // show distribution suggestion
  EditDistr.Text := Data.distroName + ' ' + Data.distroRelease;

  // text by resourcestrings
  Caption := rsCapDistr;
  LabelDistr.Caption := rsDistr;
  InfoDistribution.Hint := rsInfoDistribution + #10 + Data.DistrInfo.Distribs;
  LabelCorrect.Caption := rsCorrect;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TDistribution.BtnNextClick(Sender: TObject);
begin
  GoOn := True;
  Distribution.Close;

  // If the distribution was edited:
  if EditDistr.Text <> Data.distroName + ' ' + Data.distroRelease then
  begin
    // set new distribution name and release
    // function Copy is 1-based like Pos
    Data.distroName := Copy(EditDistr.Text, 1, Pos(' ', EditDistr.Text) - 1);
    Data.distroRelease :=
      Copy(EditDistr.Text, Pos(' ', EditDistr.Text) + 1, Length(EditDistr.Text) -
      Pos(' ', EditDistr.Text));
  end;

  // set Data.DistrInfo
  with Data do
  begin
    DistrInfo.SetInfo(distroName, distroRelease);
    //ShowMessage(DistrInfo.DistrUrlPart);
    // If the distribution is not supported, show an information and close QuickInstall:
    if DistrInfo.MyDistr = other then
    begin
      ShowMessage(rsNoSupport + #10 + #10 + DistrInfo.Distribs);
      Close;
    end;
  end;
end;

procedure TDistribution.BtnBackClick(Sender: TObject);
begin
  GoOn := False;
  Distribution.Close;
end;

end.
