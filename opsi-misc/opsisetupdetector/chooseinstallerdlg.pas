unit ChooseInstallerDlg;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TFChooseInstallerDlg }

  TFChooseInstallerDlg = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ComboBoxChooseInstaller: TComboBox;
    FlowPanel1: TFlowPanel;
    LabelChooseMsg: TLabel;
    LabelSorryUnknown: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FChooseInstallerDlg: TFChooseInstallerDlg;

resourcestring

  rsSorryUnknown = 'Sorry Unknown Installer';
  rsChooseInstallerMsg =
    'Do you want to choose a' + LineEnding +
    'installertype manually' + LineEnding +
     'and go on (ok)' + LineEnding +
     'or do you want to stop (cancel) ?';


implementation

{$R *.lfm}

{ TFChooseInstallerDlg }

procedure TFChooseInstallerDlg.FormCreate(Sender: TObject);
begin
  LabelSorryUnknown.Caption := rsSorryUnknown;
  LabelChooseMsg.Caption := rsChooseInstallerMsg;
end;


end.

