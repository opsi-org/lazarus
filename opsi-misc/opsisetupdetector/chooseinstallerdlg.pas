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
    Label1: TLabel;
    Panel1: TPanel;
  private

  public

  end;

var
  FChooseInstallerDlg: TFChooseInstallerDlg;

implementation

{$R *.lfm}

end.

