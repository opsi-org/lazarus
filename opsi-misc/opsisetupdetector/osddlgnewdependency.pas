unit osddlgnewdependency;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
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
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButtonAction: TRadioButton;
    RadioButtonState: TRadioButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FNewDepDlg: TFNewDepDlg;
  newdependency : TPDependency;

implementation

{$R *.lfm}

{ TFNewDepDlg }

procedure TFNewDepDlg.FormShow(Sender: TObject);
begin
  // create new dependency
  (*
  newdependency:= TPDependency(aktProduct.dependencies.add);
  newdependency.init;
  TIPropertyGridNewDep.TIObject := newdependency;
  *)

end;

procedure TFNewDepDlg.FormHide(Sender: TObject);
begin
  (*
  TIPropertyGridNewDep.ConsistencyCheck;
  TIPropertyGridNewDep.TIObject := nil;
  *)
end;

procedure TFNewDepDlg.BitBtn1Click(Sender: TObject);
begin

end;

end.

