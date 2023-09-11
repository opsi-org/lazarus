unit OpsiLinuxInstaller_DistributionForm;

// Parent form to ask for the linux distribution.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TOpsiLinuxInstallerDistributionForm = class(TForm)
    BtnBack: TButton;
    BtnNext: TButton;
    PanelDistr: TPanel;
    EditDistr: TEdit;
    LabelCorrect: TLabel;
    LabelDistr: TLabel;
    InfoDistribution: TImage;
    // TODO in FormActivate: Place form and controls, show detected distribution, set resourcestrings
    procedure FormActivate(Sender: TObject); virtual; abstract;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject); virtual;
  public
  var
    // GoOn tells the WelcomeForm whether the next or the back button was
    // clicked, i.e. whether to go on to the next form or to
    // stay on WelcomeForm after this form closed.
    GoOn: boolean;
  end;


implementation

procedure TOpsiLinuxInstallerDistributionForm.BtnBackClick(Sender: TObject);
begin
  GoOn := False;
  Close;
end;

procedure TOpsiLinuxInstallerDistributionForm.BtnNextClick(Sender: TObject);
begin
  GoOn := True;
  Close;
  // TODO: Check the distribution and set it in TOpsiLinuxInstallerData
end;

end.

