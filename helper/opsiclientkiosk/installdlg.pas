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
  end;

var
  Finstalldlg: TFInstalldlg;

implementation

{$R *.lfm}

{ TFInstalldlg }

procedure TFInstalldlg.BitBtnNowClick(Sender: TObject);
var i : integer;
begin
  try
    // fire on demand
    //http://wiki.freepascal.org/Cursor#Example_3:_Change_All_Controls_To_An_Hour_Glass.2C_Except_TBitBtn_Controls
    screen.Cursor := crHourGlass;
    OCKOpsiConnection.DoActionsOnDemand;
  finally
    screen.Cursor := crDefault;
  end;
  visible := false;
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

