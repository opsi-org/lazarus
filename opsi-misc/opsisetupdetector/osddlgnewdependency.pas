unit osddlgnewdependency;

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
  lcltranslator,
  LclIntf,
  osdbasedata;

type

  { TFNewDepDlg }

  TFNewDepDlg = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ComboBoxActState: TComboBox;
    ComboBoxDepActionrequest: TComboBox;
    ComboBoxproductIds: TComboBox;
    ComboBoxReqType: TComboBox;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    FlowPanel3: TFlowPanel;
    FlowPanel4: TFlowPanel;
    FlowPanelBaseActionRequest: TFlowPanel;
    FlowPanel6: TFlowPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    LabelDepActionrequest: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelConnect: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButtonAction: TRadioButton;
    RadioButtonState: TRadioButton;
    SpeedButtonHelpDependecies: TSpeedButton;
    procedure ComboBoxActStateChange(Sender: TObject);
    procedure EditproductidChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioButtonActionChange(Sender: TObject);
    procedure SpeedButtonHelpDependeciesClick(Sender: TObject);
  private

  public

  end;

var
  FNewDepDlg: TFNewDepDlg;
  newdependency: TPDependency;

resourcestring
  // new for 4.1.0.2 ******************************************************************
  rsDepDlgProductId = 'productId' + LineEnding + 'of the dependent product';
  rsDepDlgAction = 'create dependency' + LineEnding + 'for which action request';

implementation
uses
  osdform;

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
    if ComboBoxActState.Items.IndexOf(ComboBoxActState.Text) < 0 then
      ComboBoxActState.Text := 'setup';
  end
  else
  begin
    ComboBoxActState.Items.Clear;
    ComboBoxActState.Items.Add('installed');
    ComboBoxActState.Items.Add('not installed');
    ComboBoxActState.Items.Add('none');
    if ComboBoxActState.Items.IndexOf(ComboBoxActState.Text) < 0 then
      ComboBoxActState.Text := 'installed';
  end;
end;

procedure TFNewDepDlg.SpeedButtonHelpDependeciesClick(Sender: TObject);
var
  myUrl : string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url+'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration-dependency-config'
  else
    myUrl := opsidocs_base_url+'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration-dependency-config';
  OpenURL(myUrl);
end;

procedure TFNewDepDlg.FormShow(Sender: TObject);
var
  resourcedir: string;
  tmpimage: TPicture;
begin
  LabelDepActionrequest.Caption := rsDepDlgProductId;
  LabelDepActionrequest.Caption := rsDepDlgAction;
  if myconfiguration.dependencies_for_all_actionrequests and
    (not (osdsettings.runmode = createMeta)) then
  begin
    ComboBoxDepActionrequest.Enabled := True;
    LabelDepActionrequest.Enabled := True;
    FlowPanelBaseActionRequest.Enabled := True;
  end
  else
  begin
    ComboBoxDepActionrequest.Enabled := False;
    LabelDepActionrequest.Enabled := False;
    FlowPanelBaseActionRequest.Enabled := False;
  end;
  //ComboBoxReqType.Enabled := True;
  {$IFDEF UNIX}
  tmpimage := TPicture.Create;
  // the first path is in the development environment
  resourcedir := ExtractFileDir(Application.ExeName);
  {$IFDEF DARWIN}
  resourcedir := ExtractFileDir(Application.ExeName) + PathDelim + '../Resources';
  {$ENDIF DARWIN}
  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'help-circle20.png');
  SpeedButtonHelpDependecies.Glyph.Assign(tmpimage.Bitmap);
  FreeAndNil(tmpimage);
  {$ENDIF UNIX}
end;

procedure TFNewDepDlg.ComboBoxActStateChange(Sender: TObject);
begin
  if ComboBoxActState.Text = 'uninstall' then
    ComboBoxReqType.Enabled := False
  else
    ComboBoxReqType.Enabled := True;
end;

procedure TFNewDepDlg.EditproductidChange(Sender: TObject);
begin
  TEdit(Sender).Caption := cleanOpsiId(TEdit(Sender).Caption);
  TEdit(Sender).SelStart := Length(TEdit(Sender).Caption);
end;

procedure TFNewDepDlg.FormActivate(Sender: TObject);
begin
  //SetDefaultLang(osdsettings.mylang, osdsettings.mylocaledir);
  //Repaint;
end;


end.
