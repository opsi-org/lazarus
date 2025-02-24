unit osddlgnewproperty;

{$mode objfpc}{$H+}

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
  LclIntf,
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
    SpeedButtonHelpProperties: TSpeedButton;
    procedure BitBtnAddPropClick(Sender: TObject);
    procedure BitBtnDelPropClick(Sender: TObject);
    procedure CheckBoxPropMultiValChange(Sender: TObject);
    procedure EditPropNameChange(Sender: TObject);
    procedure EditPropNameEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioButtonPropBoolChange(Sender: TObject);
    procedure RadioButtonPropStringChange(Sender: TObject);
    procedure SpeedButtonHelpPropertiesClick(Sender: TObject);
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
  rsPropDefaultVal = 'Default Values' + LineEnding + 'Only the selected' +
    LineEnding + 'Values are the defaults';


implementation

{$R *.lfm}
{$IFDEF OSDGUI}
uses
  osdform;
{$ENDIF OSDGUI}

{ TFNewPropDlg }

procedure TFNewPropDlg.initFields;
begin
  EditPropName.Text := 'propertyname';
  MemoDesc.Lines.Clear;
  RadioButtonPropBool.Checked := True;
  CheckBoxPropEdit.Checked := False;
  CheckBoxPropMultiVal.Checked := False;
  EditPropNewVal.Text := 'new_value';
  ListBoxPropPosVal.Clear;
  ListBoxPropDefVal.Clear;
  Label5.Caption := rsPropDefaultVal;
  RadioButtonPropStringChange(self);
  CheckBoxPropMultiValChange(self);
  RadioButtonPropBoolChange(self);
  ListBoxPropDefVal.Selected[1] := True;
  SetDefaultLang(osdsettings.mylang, osdsettings.mylocaledir);
  Repaint;
  Application.ProcessMessages;
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
    ListBoxPropDefVal.ExtendedSelect := False;
  end;
end;

procedure TFNewPropDlg.SpeedButtonHelpPropertiesClick(Sender: TObject);
var
  myUrl : string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url+'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration-properties-config'
  else
    myUrl := opsidocs_base_url+'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration-properties-config';
  OpenURL(myUrl);
end;

procedure TFNewPropDlg.CheckBoxPropMultiValChange(Sender: TObject);
begin
  if CheckBoxPropMultiVal.Checked then
  begin
    if RadioButtonPropString.Checked then
    begin
      ListBoxPropDefVal.MultiSelect := True;
      ListBoxPropDefVal.ExtendedSelect := True;
    end;
  end
  else
  begin
    if RadioButtonPropString.Checked then
    begin
      ListBoxPropDefVal.MultiSelect := False;
      ListBoxPropDefVal.ExtendedSelect := False;
    end;
  end;
end;

procedure TFNewPropDlg.EditPropNameChange(Sender: TObject);
var
  aktCaretPos: TPoint;
  col: longint;
begin
  aktCaretPos := TEdit(Sender).CaretPos;
  col := aktCaretPos.X;
  // lower case and replace special chars by _
  TEdit(Sender).Caption := cleanOpsiId(TEdit(Sender).Caption);
  // restore the caret position
  TEdit(Sender).SelStart := col;
end;

procedure TFNewPropDlg.EditPropNameEditingDone(Sender: TObject);
var
  index, i: integer;
  exists: boolean;
begin
  if pos(' ', FNewPropDlg.EditPropName.Text) > 0 then
  begin
    MessageDlg('opsi-setup-detector: Property Editor: Error',
      'property Id: ' + FNewPropDlg.EditPropName.Text + rsContainsWhitespaceWarning,
      mtError, [mbOK], '');
    valid := False;
  end;
end;

procedure TFNewPropDlg.FormActivate(Sender: TObject);
begin
  SetDefaultLang(osdsettings.mylang, osdsettings.mylocaledir);
  Repaint;
end;

procedure TFNewPropDlg.FormCreate(Sender: TObject);
var
  resourcedir : string;
  tmpimage: TPicture;
begin
  DataModule1.SetFontName(TControl(Sender), myFont);
  {$IFDEF UNIX}
  tmpimage := TPicture.Create;
  // the first path is in the development environment
  resourcedir := ExtractFileDir(Application.ExeName);
  {$IFDEF DARWIN}
    resourcedir := ExtractFileDir(Application.ExeName) + PathDelim + '../Resources';
  {$ENDIF DARWIN}
  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'help-circle20.png');
  SpeedButtonHelpProperties.Glyph.Assign(tmpimage.Bitmap);
  FreeAndNil(tmpimage);
  {$ENDIF UNIX}
end;

procedure TFNewPropDlg.FormShow(Sender: TObject);
begin
  EditPropName.SetFocus;
end;

procedure TFNewPropDlg.RadioButtonPropBoolChange(Sender: TObject);
begin
  if RadioButtonPropBool.Checked then
  begin
    CheckBoxPropMultiVal.Enabled := False;
    CheckBoxPropEdit.Enabled := False;
    CheckBoxPropMultiVal.Checked := False;
    CheckBoxPropEdit.Checked := False;
  end
  else
  begin
    CheckBoxPropMultiVal.Enabled := True;
    CheckBoxPropEdit.Enabled := True;
  end;
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

procedure TFNewPropDlg.BitBtnDelPropClick(Sender: TObject);
begin
  if ListBoxPropPosVal.ItemIndex > -1 then
  begin
    ListBoxPropPosVal.Items.Delete(ListBoxPropPosVal.ItemIndex);
    ListBoxPropDefVal.Items.Delete(ListBoxPropPosVal.ItemIndex);
  end;
end;

end.
