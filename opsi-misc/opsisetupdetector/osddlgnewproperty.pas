unit osddlgnewproperty;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
  osdbasedata;

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
    procedure BitBtnAddPropClick(Sender: TObject);
    procedure BitBtnDelPropClick(Sender: TObject);
    procedure CheckBoxPropMultiValChange(Sender: TObject);
    procedure EditPropNameEditingDone(Sender: TObject);
    procedure RadioButtonPropStringChange(Sender: TObject);
  private

  public

  end;

var
  FNewPropDlg: TFNewPropDlg;
  valid : boolean = true;

implementation

{$R *.lfm}
uses
  osdform;

{ TFNewPropDlg }

procedure TFNewPropDlg.RadioButtonPropStringChange(Sender: TObject);
begin
  if RadioButtonPropString.Checked then
  begin
    CheckGroupPropBool.Enabled:=true;
    ListBoxPropPosVal.Items.Clear;
    ListBoxPropDefVal.Items.Clear;
    PanelPropPosVal.Enabled:=true;
  end
  else
  begin
    CheckGroupPropBool.Enabled:=true;
    CheckBoxPropMultiVal.Checked:=false;
    CheckBoxPropEdit.Checked:=false;
    ListBoxPropPosVal.Items.Clear;
    ListBoxPropPosVal.Items.Add('True');
    ListBoxPropPosVal.Items.Add('False');
    PanelPropPosVal.Enabled:=false;
    ListBoxPropDefVal.Items.Clear;
    ListBoxPropDefVal.Items.Add('True');
    ListBoxPropDefVal.Items.Add('False');
    ListBoxPropDefVal.MultiSelect:=false;
    ListBoxPropDefVal.Selected[0] := true;
  end;
end;

procedure TFNewPropDlg.CheckBoxPropMultiValChange(Sender: TObject);
begin
  if CheckBoxPropMultiVal.Checked then
  begin
    if RadioButtonPropString.Checked then
      ListBoxPropDefVal.MultiSelect:=true;
  end
  else
  begin
     if RadioButtonPropString.Checked then
      ListBoxPropDefVal.MultiSelect:=false;
  end;

end;

procedure TFNewPropDlg.EditPropNameEditingDone(Sender: TObject);
var
  index, i : integer;
  tmpstr : string;
  exists : boolean;
begin
  if pos(' ',FNewPropDlg.EditPropName.Text) > 0 then
  begin
       MessageDlg('opsi-setup-detector: Property Editor: Error',
        'property Id: ' + FNewPropDlg.EditPropName.Text +
        ' contains whitespace. Whitespaces are not allowed.',
        mtError, [mbOK], '');
       valid := false;
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
        'property Id: ' + FNewPropDlg.EditPropName.Text +
        ' exists. Duplicates not allowed.',
        mtError, [mbOK], '');
      valid := false;
    end;
end;

procedure TFNewPropDlg.BitBtnAddPropClick(Sender: TObject);
begin
  if FNewPropDlg.ListBoxPropDefVal.Items.IndexOf(EditPropNewVal.Text) < 0 then
  begin
  ListBoxPropPosVal.Items.Add(EditPropNewVal.Text);
  ListBoxPropDefVal.Items.Add(EditPropNewVal.Text);
  end
  else MessageDlg('opsi-setup-detector: Property Editor: Error',
                  'property value: '+EditPropNewVal.Text+' exists. Duplicates not allowed.',
                   mtError, [mbOK],'');

end;

procedure TFNewPropDlg.BitBtnDelPropClick(Sender: TObject);
begin
  if ListBoxPropPosVal.ItemIndex > - 1 then
  begin
    ListBoxPropPosVal.Items.Delete(ListBoxPropPosVal.ItemIndex);
    ListBoxPropDefVal.Items.Delete(ListBoxPropPosVal.ItemIndex);
  end;
end;

end.

