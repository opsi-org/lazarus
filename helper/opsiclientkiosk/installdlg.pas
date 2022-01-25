unit installdlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, ExtCtrls, StdCtrls, Buttons,opsiconnection;

type

  { TFInstalldlg }

  TFInstalldlg= class(TForm)
    BitBtnNow: TBitBtn;
    BitBtnLater: TBitBtn;
    Label1: TLabel;
    Memo1: TMemo;
    PanelDlgTop: TPanel;
    ToolBar1: TToolBar;
    procedure BitBtnLaterClick(Sender: TObject);
    procedure BitBtnNowClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    SoftwareOnDemand : boolean;
    UpdateData: boolean;
  end;

var
  Finstalldlg: TFInstalldlg;

resourcestring
  rsErrorOnDemand = 'opsi is bussy please try it again later. Detailed error message: ';

implementation

{$R *.lfm}

uses
  opsiclientkioskgui;

{ TFInstalldlg }

procedure TFInstalldlg.BitBtnNowClick(Sender: TObject);
var
  i : integer;
  ErrorMessage: string;
begin
  try
    // fire on demand
    //http://wiki.freepascal.org/Cursor#Example_3:_Change_All_Controls_To_An_Hour_Glass.2C_Except_TBitBtn_Controls
    screen.Cursor := crHourGlass;
    OCKOpsiConnection.DoActionsOnDemand(ErrorMessage);
    if ErrorMessage <> '' then ShowMessage(rsErrorOnDemand + ErrorMessage);
  finally
    screen.Cursor := crDefault;
    visible := false;
    UpdateData:= true;
    FormOpsiClientKiosk.Close;
  end;
end;

procedure TFInstalldlg.FormActivate(Sender: TObject);
begin
  if not SoftwareOnDemand then BitBtnNow.Enabled:= False;
end;

procedure TFInstalldlg.Memo1Change(Sender: TObject);
begin

end;

procedure TFInstalldlg.BitBtnLaterClick(Sender: TObject);
begin
  // nothing to do
  visible := false;
end;

end.

