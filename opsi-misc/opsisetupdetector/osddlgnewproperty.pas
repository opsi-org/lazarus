unit osddlgnewproperty;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
  osdbasedata;

type

  { TFNewProperty }

  TFNewProperty = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtnAddDep: TBitBtn;
    BitBtnDelDep: TBitBtn;
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
    ListBoxPropPosVal: TListBox;
    ListBoxPropDefVal: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelPropPosVal: TPanel;
    Panel7: TPanel;
    RadioButtonPropString: TRadioButton;
    RadioButtonPropBool: TRadioButton;
    procedure BitBtnAddDepClick(Sender: TObject);
    procedure BitBtnDelDepClick(Sender: TObject);
    procedure CheckBoxPropMultiValChange(Sender: TObject);
    procedure RadioButtonPropStringChange(Sender: TObject);
  private

  public

  end;

var
  FNewProperty: TFNewProperty;

implementation

{$R *.lfm}

{ TFNewProperty }

procedure TFNewProperty.RadioButtonPropStringChange(Sender: TObject);
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
    ListBoxPropPosVal.Items.Clear;
    ListBoxPropPosVal.Items.Add('True');
    ListBoxPropPosVal.Items.Add('False');
    PanelPropPosVal.Enabled:=false;
    ListBoxPropDefVal.Items.Clear;
    ListBoxPropDefVal.Items.Add('True');
    ListBoxPropDefVal.Items.Add('False');
    ListBoxPropDefVal.MultiSelect:=false;
  end;
end;

procedure TFNewProperty.CheckBoxPropMultiValChange(Sender: TObject);
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

procedure TFNewProperty.BitBtnAddDepClick(Sender: TObject);
begin
  ListBoxPropPosVal.Items.Add(EditPropNewVal.Text);
  ListBoxPropDefVal.Items.Add(EditPropNewVal.Text);
end;

procedure TFNewProperty.BitBtnDelDepClick(Sender: TObject);
begin
  if ListBoxPropPosVal.ItemIndex > - 1 then
  begin
    ListBoxPropPosVal.Items.Delete(ListBoxPropPosVal.ItemIndex);
    ListBoxPropDefVal.Items.Delete(ListBoxPropPosVal.ItemIndex);
  end;
end;

end.

