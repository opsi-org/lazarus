unit osddlgnewdependency;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  RTTIGrids,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Buttons,
  StdCtrls,
  EditBtn,
  osdbasedata;

type

  { TFNewDepDlg }

  TFNewDepDlg = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ComboBoxActState: TComboBox;
    ComboBoxReqType: TComboBox;
    Editproductid: TEdit;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    FlowPanel3: TFlowPanel;
    FlowPanel4: TFlowPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButtonAction: TRadioButton;
    RadioButtonState: TRadioButton;
    procedure ComboBoxActStateChange(Sender: TObject);
    procedure FlowPanel2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioButtonActionChange(Sender: TObject);
  private

  public

  end;

var
  FNewDepDlg: TFNewDepDlg;
  newdependency: TPDependency;

  resourcestring
  // new for 4.1.0.2 ******************************************************************
    rsDepDlgProductId = 'productId' + LineEnding +
      'of the dependent product';

implementation

{$R *.lfm}

{ TFNewDepDlg }


procedure TFNewDepDlg.RadioButtonActionChange(Sender: TObject);
begin
  if RadioButtonAction.Checked then
  begin
    ComboBoxActState.Items.Clear;
    ComboBoxActState.Items.Add('setup');
    ComboBoxActState.Items.Add('update');
    ComboBoxActState.Items.Add('uninstall');
    ComboBoxActState.Items.Add('once');
  end
  else
  begin
    ComboBoxActState.Items.Clear;
    ComboBoxActState.Items.Add('installed');
    ComboBoxActState.Items.Add('not installed');
    ComboBoxActState.Items.Add('none');
  end;
end;

procedure TFNewDepDlg.FormShow(Sender: TObject);
begin
   label2.Caption:= rsDepDlgProductId;
   ComboBoxReqType.Enabled:= true;
end;

procedure TFNewDepDlg.ComboBoxActStateChange(Sender: TObject);
begin
  if ComboBoxActState.Text = 'uninstall' then
    ComboBoxReqType.Enabled:= false
  else
    ComboBoxReqType.Enabled:= true;
end;

procedure TFNewDepDlg.FlowPanel2Click(Sender: TObject);
begin

end;


end.

