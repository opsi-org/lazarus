unit osddlgnewproperty;

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
  lcltranslator,
  osdbasedata,
  osddatamod;

type

  { TFNewPropDlg }

  TFNewPropDlg = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtnAddProp: TBitBtn;
    BitBtnDelProp: TBitBtn;
    CheckBoxPropEdit: TCheckBox;
    CheckBoxPropMultiVal: TCheckBox;
    CheckGroupPropBool: TCheckGroup;
    EditPropName: TEdit;
    EditPropNewVal: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListBoxPropPosVal: TListBox;
    ListBoxPropDefVal: TListBox;
    MemoDesc: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel8: TPanel;
    PanelPropPosVal: TPanel;
    Panel7: TPanel;
    RadioButtonPropString: TRadioButton;
    RadioButtonPropBool: TRadioButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtnAddPropClick(Sender: TObject);
    procedure BitBtnDelPropClick(Sender: TObject);
    procedure CheckBoxPropMultiValChange(Sender: TObject);
    procedure EditPropNameEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButtonPropStringChange(Sender: TObject);
  private

  public
    procedure initFields;

  end;

var
  FNewPropDlg: TFNewPropDlg;
  valid: boolean = True;

resourcestring
  // new for 4.1.0.2 ******************************************************************
  rsContainsWhitespaceWarning = ' contains whitespace. Whitespaces are not allowed.';
  rsDuplicateWarning = ' exists. Duplicates not allowed.';


implementation

{$R *.lfm}
uses
  osdform;

{ TFNewPropDlg }

procedure TFNewPropDlg.initFields;
begin
  EditPropName.Text := 'PropertyName';
  MemoDesc.Lines.Clear;
  RadioButtonPropBool.Checked := True;
  CheckBoxPropEdit.Checked := False;
  CheckBoxPropMultiVal.Checked := False;
  EditPropNewVal.Text := 'New Value';
  ListBoxPropPosVal.Clear;
  ListBoxPropDefVal.Clear;
end;

procedure TFNewPropDlg.RadioButtonPropStringChange(Sender: TObject);
begin
  if RadioButtonPropString.Checked then
  begin
    CheckGroupPropBool.Enabled := True;
    ListBoxPropPosVal.Items.Clear;
    ListBoxPropDefVal.Items.Clear;
    PanelPropPosVal.Enabled := True;
  end
  else
  begin
    CheckGroupPropBool.Enabled := True;
    CheckBoxPropMultiVal.Checked := False;
    CheckBoxPropEdit.Checked := False;
    ListBoxPropPosVal.Items.Clear;
    ListBoxPropPosVal.Items.Add('True');
    ListBoxPropPosVal.Items.Add('False');
    PanelPropPosVal.Enabled := False;
    ListBoxPropDefVal.Items.Clear;
    ListBoxPropDefVal.Items.Add('True');
    ListBoxPropDefVal.Items.Add('False');
    ListBoxPropDefVal.MultiSelect := False;
    ListBoxPropDefVal.Selected[0] := True;
  end;
end;

procedure TFNewPropDlg.CheckBoxPropMultiValChange(Sender: TObject);
begin
  if CheckBoxPropMultiVal.Checked then
  begin
    if RadioButtonPropString.Checked then
      ListBoxPropDefVal.MultiSelect := True;
  end
  else
  begin
    if RadioButtonPropString.Checked then
      ListBoxPropDefVal.MultiSelect := False;
  end;

end;

procedure TFNewPropDlg.EditPropNameEditingDone(Sender: TObject);
var
  index, i: integer;
  tmpstr: string;
  exists: boolean;
begin
  if pos(' ', FNewPropDlg.EditPropName.Text) > 0 then
  begin
    MessageDlg('opsi-setup-detector: Property Editor: Error',
      'property Id: ' + FNewPropDlg.EditPropName.Text + rsContainsWhitespaceWarning,
      mtError, [mbOK], '');
    valid := False;
  end;

  index := resultform1.StringGridProp.RowCount;
  tmpstr := lowercase(FNewPropDlg.EditPropName.Text);
  exists := False;
  for i := 0 to index - 1 do
    if lowercase(tmpstr) = lowercase(resultform1.StringGridProp.Cells[1, i]) then
      exists := True;
  if exists then
  begin
    MessageDlg('opsi-setup-detector: Property Editor: Error',
      'property Id: ' + FNewPropDlg.EditPropName.Text + rsDuplicateWarning,
      mtError, [mbOK], '');
    valid := False;
  end;
end;

procedure TFNewPropDlg.FormCreate(Sender: TObject);
begin
  DataModule1.SetFontName(TControl(Sender), myFont);
end;

procedure TFNewPropDlg.BitBtnAddPropClick(Sender: TObject);
begin
  if FNewPropDlg.ListBoxPropDefVal.Items.IndexOf(EditPropNewVal.Text) < 0 then
  begin
    ListBoxPropPosVal.Items.Add(EditPropNewVal.Text);
    ListBoxPropDefVal.Items.Add(EditPropNewVal.Text);
  end
  else
    MessageDlg('opsi-setup-detector: Property Editor: Error',
      'property value: ' + EditPropNewVal.Text + rsDuplicateWarning,
      mtError, [mbOK], '');

end;

procedure TFNewPropDlg.BitBtn2Click(Sender: TObject);
begin

end;

procedure TFNewPropDlg.BitBtn1Click(Sender: TObject);
begin

end;

procedure TFNewPropDlg.BitBtnDelPropClick(Sender: TObject);
begin
  if ListBoxPropPosVal.ItemIndex > -1 then
  begin
    ListBoxPropPosVal.Items.Delete(ListBoxPropPosVal.ItemIndex);
    ListBoxPropDefVal.Items.Delete(ListBoxPropPosVal.ItemIndex);
  end;
end;

end.


