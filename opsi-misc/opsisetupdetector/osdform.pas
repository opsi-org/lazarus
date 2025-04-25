unit osdform;

{$mode objfpc}{$H+}


interface

uses

  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF WINDOWS}
  lazfileutils,
  Classes, SysUtils, FileUtil, RTTICtrls, RTTIGrids, IpHtml,
  Forms, Controls, Graphics,
  LCLType,
  LclIntf,
  Dialogs, ExtCtrls,
  StdCtrls,
  Buttons,
  ComCtrls,
  Menus,
  StrUtils,
  typinfo,
  {$IFNDEF WINDOWS}
  CustApp,
  {$ENDIF WINDOWS}
  osdanalyzewin,
  osdanalyzelin,
  osdanalyzemac,
  lcltranslator,
  EditBtn,
  Grids,
  PairSplitter,
  ColorBox,
  //HtmlView,
  MarkdownUtils,
  MarkdownProcessor,
  //HtmlGlobals,
  //HTMLUn2,
  oslog,
  osdbasedata, osdconfigdlg, osdcreate, fpjsonrtti, osddlgnewdependency,
  osddlgnewproperty, osparserhelper,
  osddatamod,
  osdcheckentriesdlg,
  Contnrs,
  osmessagedialog,
  oswebservice,
  osdmain,
  osd_md_html_dlg, Types;

type
  TIconDisplay = class(TPersistent)
  private
    Panel: TPanel;
    Image: TImage;
    FileName: string;
  public
  end;



  { TResultform1 }

  TResultform1 = class(TForm)
    BitBtnChooseInstDir4: TBitBtn;
    BitBtnChooseTargetProg4: TBitBtn;
    BitBtnChooseUninstFile4: TBitBtn;
    BitBtnImportControl: TBitBtn;
    BitBtnOpenMst4: TBitBtn;
    BitBtnRebuild: TBitBtn;
    BitBtnAddDep: TBitBtn;
    BitBtnChooseInstDir2: TBitBtn;
    BitBtnChooseInstDir3: TBitBtn;
    BitBtnChooseTargetProg2: TBitBtn;
    BitBtnChooseTargetProg3: TBitBtn;
    BitBtnChooseUninstFile1: TBitBtn;
    BitBtnChooseUninstFile2: TBitBtn;
    BitBtnChooseUninstFile3: TBitBtn;
    BitBtnChooseTargetProg1: TBitBtn;
    BitBtnDelDep: TBitBtn;
    BitBtnEditDep: TBitBtn;
    BitBtnOpenMst3: TBitBtn;
    BitBtnChooseInstDir1: TBitBtn;
    BtAnalyzeOnly: TBitBtn;
    BtATwonalyzeAndCreate: TBitBtn;
    BtCreateEmptyTemplateLin: TBitBtn;
    BtCreateEmptyTemplateMac: TBitBtn;
    BtCreateEmptyTemplateMulti: TBitBtn;
    BtCreateEmptyTemplateWin: TBitBtn;
    BtCreateWingetPackage: TBitBtn;
    BtCreateEmptyTemplateWithUserWin: TBitBtn;
    BtCreateMeta: TBitBtn;
    BtnOpenIconFolder: TBitBtn;
    BitBtnAddProp: TBitBtn;
    BitBtnDelProp: TBitBtn;
    BitBtnEditProp: TBitBtn;
    BitBtnWorkBenchPath: TBitBtn;
    BitBtnRecheckWorkbench: TBitBtn;
    BtAnalyzeNextStep: TBitBtn;
    BtCreateProduct: TBitBtn;
    BtProduct1NextStep: TBitBtn;
    BtnIconsNextStep: TBitBtn;
    BtProduct2NextStep: TBitBtn;
    BtSetup1NextStep: TBitBtn;
    BtSetup2NextStep: TBitBtn;
    BitBtnDefault: TBitBtn;
    BitBtnMST1: TBitBtn;
    BitBtnOpenMst1: TBitBtn;
    BitBtnOpenMst2: TBitBtn;
    BtSetup3NextStep: TBitBtn;
    BtWingetNextStep: TBitBtn;
    BtSingleAnalyzeAndCreateLin: TBitBtn;
    BtSingleAnalyzeAndCreateMac: TBitBtn;
    BtSingleAnalyzeAndCreateMulti: TBitBtn;
    BtSingleAnalyzeAndCreateWin: TBitBtn;
    BtSingleAnalyzeAndCreateWithUser: TBitBtn;
    CheckBoxDefaultIcon: TCheckBox;
    CheckBoxNoIcon: TCheckBox;
    EditLogInfo: TEdit;
    FlowPanel1: TFlowPanel;
    FlowPanel10: TFlowPanel;
    FlowPanel11: TFlowPanel;
    FlowPanel12: TFlowPanel;
    FlowPanel13: TFlowPanel;
    FlowPanel14: TFlowPanel;
    FlowPanel15: TFlowPanel;
    FlowPanel16: TFlowPanel;
    FlowPanel17: TFlowPanel;
    FlowPanel18: TFlowPanel;
    FlowPanel19: TFlowPanel;
    FlowPanel2: TFlowPanel;
    FlowPanel20: TFlowPanel;
    FlowPanel21: TFlowPanel;
    FlowPanel22: TFlowPanel;
    FlowPanel23: TFlowPanel;
    FlowPanel24: TFlowPanel;
    FlowPanel25: TFlowPanel;
    FlowPanel26: TFlowPanel;
    FlowPanel27: TFlowPanel;
    FlowPanel28: TFlowPanel;
    FlowPanel29: TFlowPanel;
    FlowPanel3: TFlowPanel;
    FlowPanel30: TFlowPanel;
    FlowPanel31: TFlowPanel;
    FlowPanel4: TFlowPanel;
    FlowPanel6: TFlowPanel;
    FlowPanel8: TFlowPanel;
    FlowPanel9: TFlowPanel;
    FlowPanelCustomDir: TFlowPanel;
    FlowPanelLinuxTitle: TFlowPanel;
    FlowPanelLinuxTitle2: TFlowPanel;
    FlowPanelMacosTitle: TFlowPanel;
    FlowPanelMsiId1: TFlowPanel;
    FlowPanelMsiId2: TFlowPanel;
    FlowPanelMsiId3: TFlowPanel;
    FlowPanelMST1: TFlowPanel;
    FlowPanelMST2: TFlowPanel;
    FlowPanelMST3: TFlowPanel;
    FlowPanelOsIndendentTitle: TFlowPanel;
    FlowPanelSetup32: TFlowPanel;
    FlowPanelMST: TFlowPanel;
    FlowPanel5: TFlowPanel;
    FlowPanelMsiId: TFlowPanel;
    FlowPanel7: TFlowPanel;
    FlowPanelSetup33: TFlowPanel;
    FileHelp: TMenuItem;
    FlowPanelSetup34: TFlowPanel;
    FlowPanelSetup35: TFlowPanel;
    FlowPanelSetup36: TFlowPanel;
    FlowPanelSetup37: TFlowPanel;
    FlowPanelSetup38: TFlowPanel;
    FlowPanelSetup39: TFlowPanel;
    FlowPanelSetup40: TFlowPanel;
    FlowPanelSetup41: TFlowPanel;
    FlowPanelSetup42: TFlowPanel;
    FlowPanelSetup43: TFlowPanel;
    FlowPanelSetup44: TFlowPanel;
    FlowPanelSetup45: TFlowPanel;
    FlowPanelSetup46: TFlowPanel;
    FlowPanelSetup47: TFlowPanel;
    FlowPanelWindowsTitle: TFlowPanel;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    ImageList1: TImageList;
    IpHtmlPanelAdvice: TIpHtmlPanel;
    IpHtmlPanelDesc: TIpHtmlPanel;
    Label1: TLabel;
    Label100: TLabel;
    Label101: TLabel;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Label108: TLabel;
    Label109: TLabel;
    Label110: TLabel;
    Label111: TLabel;
    Label112: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    Label119: TLabel;
    Label120: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label56: TLabel;
    Label6: TLabel;
    Label63: TLabel;
    Label69: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    LabelChannel: TLabel;
    LabelNumber: TLabel;
    LabelNumIcons: TLabel;
    LabelNameSelIcon: TLabel;
    LabelIconName: TLabel;
    LabelIconDir: TLabel;
    LabelIconPreview: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    Label50: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    LabelWorkbenchOK: TLabel;
    LabelWorkbenchNotOK: TLabel;
    MemoDefault: TMemo;
    MenuItemLog: TMenuItem;
    MenuItemOpenControl: TMenuItem;
    MenuItemOpenProj: TMenuItem;
    MenuItemSaveProj: TMenuItem;
    MenuItemLangFr: TMenuItem;
    MenuItemLang: TMenuItem;
    MenuItemLangDe: TMenuItem;
    MenuItemLangEn: TMenuItem;
    MenuItemLangEs: TMenuItem;
    MenuItemStart: TMenuItem;
    MenuItemKnownInstallers: TMenuItem;
    MenuItemConfig: TMenuItem;
    OpenDialogSetupfile: TOpenDialog;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    PanelChannel: TPanel;
    SpeedButtonHelpMain: TSpeedButton;
    SpeedButtonHelpAnalyze: TSpeedButton;
    SpeedButtonHelpSetup1: TSpeedButton;
    SpeedButtonHelpProd1: TSpeedButton;
    SpeedButtonHelpProd2: TSpeedButton;
    SpeedButtonHelpIcon: TSpeedButton;
    SpeedButtonHelpCreate: TSpeedButton;
    SpeedButtonHelpSetup2: TSpeedButton;
    SpeedButtonHelpSetup3: TSpeedButton;
    SpeedButtonHelpSetup4: TSpeedButton;
    SpeedButtonHelpStartOSIndep: TSpeedButton;
    SpeedButtonHelpStartWin: TSpeedButton;
    SpeedButtonHelpStartLin: TSpeedButton;
    SpeedButtonHelpStartMac: TSpeedButton;
    SpeedButtonHelpStartMulti: TSpeedButton;
    TabSheetWinget: TTabSheet;
    TaskPanelMulti: TPanel;
    TaskPanelMac: TPanel;
    PanelDepBtns: TPanel;
    TaskPanelWin: TPanel;
    TaskPanelLinux: TPanel;
    TaskPanelIndep: TPanel;
    PanelNumIcons: TPanel;
    PanelIconPreview: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelProcess: TPanel;
    processing: TLabel;
    processStatement: TLabel;
    ProgressBar1: TProgressBar;
    ProgressBarAnalyze: TProgressBar;
    SaveDialogProj: TSaveDialog;
    SBtnExit: TSpeedButton;
    ScrollBox1: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    TabSheetSetup3: TTabSheet;
    TabSheetIcons: TTabSheet;
    TabSheetProduct2: TTabSheet;
    TabSheetCreate: TTabSheet;
    TabSheetStart: TTabSheet;
    TabSheetSetup2: TTabSheet;
    TabSheetProduct: TTabSheet;
    TabSheetSetup1: TTabSheet;
    FileExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemFile: TMenuItem;
    MainMenu1: TMainMenu;
    MemoAnalyze: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TabSheetAnalyze: TTabSheet;
    TICheckBoxCustomdir: TTICheckBox;
    TICheckBoxCustomizeProfile: TTICheckBox;
    TICheckBoxS4Mst: TTICheckBox;
    TICheckBoxS4Silent: TTICheckBox;
    TICheckBoxUninstallBefore: TTICheckBox;
    TICheckBoxDesktopIcon: TTICheckBox;
    TICheckBoxHandleLiceneKey: TTICheckBox;
    TICheckBoxInstallFromLocal: TTICheckBox;
    TICheckBoxS1Mst: TTICheckBox;
    TICheckBoxS1Silent: TTICheckBox;
    TICheckBoxS3Silent: TTICheckBox;
    TICheckBoxS2Silent: TTICheckBox;
    TICheckBoxS2Mst: TTICheckBox;
    TICheckBoxS3Mst: TTICheckBox;
    TIComboBoxChannel: TTIComboBox;
    TIEditInstallDir2: TTIEdit;
    TIEditInstallDir3: TTIEdit;
    TIEditInstallDir4: TTIEdit;
    TIEditMsiId2: TTIEdit;
    TIEditMsiId3: TTIEdit;
    TIEditMsiId4: TTIEdit;
    TIEditMsiName4: TTIEdit;
    TIEditMsiUpCode1: TTIEdit;
    TIEditMsiName1: TTIEdit;
    TIEditMsiName2: TTIEdit;
    TIEditMsiName3: TTIEdit;
    TIEditMsiUpCode2: TTIEdit;
    TIEditMsiUpCode3: TTIEdit;
    TIEditMsiUpCode4: TTIEdit;
    TIEditMstFile2: TTIEdit;
    TIEditMstFile3: TTIEdit;
    TIEditMstFile4: TTIEdit;
    TIEditRequiredSizeMB4: TTIEdit;
    TIEditSetup1TargetProgram: TTIEdit;
    TIEditSetup4Command: TTIEdit;
    TIEditSetup3TargetProgram: TTIEdit;
    TIEditSetup2TargetProgram: TTIEdit;
    TIEditSetup4TargetProgram: TTIEdit;
    TIEditSetup4UnCommand: TTIEdit;
    TIEditSetup4UnProgram: TTIEdit;
    TIEditSetupfile4: TTIEdit;
    TIEditSetupFileSizeMB4: TTIEdit;
    TIEditSoftVersion1: TTIEdit;
    TIEditMsiId1: TTIEdit;
    TIEditSoftVersion2: TTIEdit;
    TIEditProdVersion3: TTIEdit;
    TIEditProdID: TTIEdit;
    TIEditProdName: TTIEdit;
    TIEditSoftVersion3: TTIEdit;
    TIEditRequiredSizeMB2: TTIEdit;
    TIEditRequiredSizeMB3: TTIEdit;
    TIEditSetup2Command: TTIEdit;
    TIEditSetup1UnCommand: TTIEdit;
    TIEditSetup3Command: TTIEdit;
    TIEditSetup2UnCommand: TTIEdit;
    TIEditSetup1UnProgram: TTIEdit;
    TIEditSetup3UnCommand: TTIEdit;
    TIEditSetup2UnProgram: TTIEdit;
    TIEditSetup3UnProgram: TTIEdit;
    TIEditSetupfile2: TTIEdit;
    TIEditSetup1Command: TTIEdit;
    TIEditSetupfile3: TTIEdit;
    TIEditSetupFileSizeMB3: TTIEdit;
    TIEditSoftVersion4: TTIEdit;
    TIEditworkbenchpath: TTIEdit;
    TIEditSetupFileSizeMB1: TTIEdit;
    TIEditRequiredSizeMB1: TTIEdit;
    TIEditInstallDir1: TTIEdit;
    TIEditSetupfile1: TTIEdit;
    TIEditMstFile1: TTIEdit;
    TIEditSetupFileSizeMB2: TTIEdit;
    TIGridProp: TTIGrid;
    TIGridDep: TTIGrid;
    TIImageIconPreview: TTIImage;
    TILabelDirSelIcon: TTILabel;
    TILabelInstaller2: TTILabel;
    TILabelInstaller3: TTILabel;
    TILabelInstaller4: TTILabel;
    TIMemoAdvice: TTIMemo;
    TIMemoDesc: TTIMemo;
    TimerFirstconfig: TTimer;
    TIRadioGroupCreateMode: TTIRadioGroup;
    TIRadioGroupBuildMode: TTIRadioGroup;
    TILabelInstaller1: TTILabel;
    TIS1Url: TTILabel;
    TIS2Url: TTILabel;
    TIS3Url: TTILabel;
    TIS4Url: TTILabel;
    TISpinEditPrio: TTISpinEdit;
    TISpinEditPackageVers: TTISpinEdit;
    TITrackBarPrio: TTITrackBar;
    ToolBar1: TToolBar;
    mysetup1: TSetupFile;
    TreeView1: TTreeView;

    procedure BitBtnAddDepClick(Sender: TObject);
    procedure BitBtnAddPropClick(Sender: TObject);
    procedure BitBtnChooseInstDir1Click(Sender: TObject);
    procedure BitBtnChooseInstDir2Click(Sender: TObject);
    procedure BitBtnChooseInstDir3Click(Sender: TObject);
    procedure BitBtnChooseTargetProg1Click(Sender: TObject);
    procedure BitBtnChooseTargetProg2Click(Sender: TObject);
    procedure BitBtnChooseTargetProg3Click(Sender: TObject);
    procedure BitBtnChooseUninstFile1Click(Sender: TObject);
    procedure BitBtnChooseUninstFile2Click(Sender: TObject);
    procedure BitBtnChooseUninstFile3Click(Sender: TObject);
    procedure BitBtnDelDepClick(Sender: TObject);
    procedure BitBtnDelPropClick(Sender: TObject);
    procedure BitBtnEditDepClick(Sender: TObject);
    procedure BitBtnEditPropClick(Sender: TObject);
    procedure BitBtnImportControlClick(Sender: TObject);
    procedure BitBtnOpenFile1Click(Sender: TObject);
    procedure BitBtnOpenFileClick(Sender: TObject);
    procedure BitBtnOpenMst1Click(Sender: TObject);
    procedure BitBtnOpenMst2Click(Sender: TObject);
    procedure BitBtnRebuildClick(Sender: TObject);
    procedure BitBtnRecheckWorkbenchClick(Sender: TObject);
    procedure BitBtnWorkBenchPathClick(Sender: TObject);
    procedure BtAnalyzeNextStepClick(Sender: TObject);
    procedure BtATwonalyzeAndCreateClick(Sender: TObject);
    procedure BtCreateEmptyTemplateMultiClick(Sender: TObject);
    procedure BtCreateEmptyTemplateWinClick(Sender: TObject);
    procedure BtCreateEmptyTemplateWithUserWinClick(Sender: TObject);
    procedure BtCreateEmptyTemplateLinClick(Sender: TObject);
    procedure BtCreateEmptyTemplateMacClick(Sender: TObject);
    procedure BtCreateMetaClick(Sender: TObject);
    procedure BtCreateProductClick(Sender: TObject);
    procedure BtCreateWingetPackageClick(Sender: TObject);
    procedure BtnIconsNextStepClick(Sender: TObject);
    procedure BtProduct1NextStepClick(Sender: TObject);
    procedure BtProduct2NextStepClick(Sender: TObject);
    procedure BtSetup1NextStepClick(Sender: TObject);
    procedure BtSetup2NextStepClick(Sender: TObject);
    procedure BtSetup3NextStepClick(Sender: TObject);
    procedure BtSingleAnalyzeAndCreateLinClick(Sender: TObject);
    procedure BtSingleAnalyzeAndCreateMacClick(Sender: TObject);
    procedure BtSingleAnalyzeAndCreateMultiClick(Sender: TObject);
    procedure BtSingleAnalyzeAndCreateWinClick(Sender: TObject);
    procedure BtnOpenIconFolderClick(Sender: TObject);
    procedure BtSingleAnalyzeAndCreateWithUserClick(Sender: TObject);
    procedure BtWingetNextStepClick(Sender: TObject);
    procedure CheckBoxDefaultIconChange(Sender: TObject);
    procedure CheckBoxNoIconChange(Sender: TObject);
    procedure FlowPanel14Click(Sender: TObject);
    procedure FlowPanel18Click(Sender: TObject);
    procedure FlowPanel3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IpHtmlPanelAdviceHotClick(Sender: TObject);
    procedure IpHtmlPanelDescClick(Sender: TObject);
    procedure IpHtmlPanelDescHotClick(Sender: TObject);
    procedure Label88Click(Sender: TObject);
    procedure MenuHelpLogClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItemLogClick(Sender: TObject);
    procedure MenuItemOpenControlClick(Sender: TObject);
    procedure MenuItemOpenProjClick(Sender: TObject);
    procedure MenuItemSaveProjClick(Sender: TObject);
    procedure MenuItemLangClick(Sender: TObject);
    procedure MenuItemLangDeClick(Sender: TObject);
    procedure MenuItemLangEnClick(Sender: TObject);
    procedure MenuItemLangEsClick(Sender: TObject);
    procedure MenuItemLangFrClick(Sender: TObject);
    procedure MenuItemStartClick(Sender: TObject);
    procedure MenuItemConfigClick(Sender: TObject);
    procedure MenuItemKnownInstallersClick(Sender: TObject);
    procedure OpenDialog1CanClose(Sender: TObject; var CanClose: boolean);
    procedure OpenDialog1Close(Sender: TObject);
    procedure OpenDialog1FolderChange(Sender: TObject);
    procedure OpenDialog1SelectionChange(Sender: TObject);
    procedure OpenDialog1Show(Sender: TObject);
    procedure setRunMode;
    procedure BitBtnClose1Click(Sender: TObject);
    procedure BtAnalyzeOnlyClick(Sender: TObject);
    procedure FileHelpClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mst32NameEditChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PanelDefaultClick(Sender: TObject);
    procedure setup32NameEditChange(Sender: TObject);
    procedure FileOpenSetupFileClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure ProductIDChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);

    procedure SBtnExitClick(Sender: TObject);
    procedure SpeedButtonHelpAnalyzeClick(Sender: TObject);
    procedure SpeedButtonHelpCreateClick(Sender: TObject);
    procedure SpeedButtonHelpIconClick(Sender: TObject);
    procedure SpeedButtonHelpMainClick(Sender: TObject);
    procedure SpeedButtonHelpProd1Click(Sender: TObject);
    procedure SpeedButtonHelpProd2Click(Sender: TObject);
    procedure SpeedButtonHelpStartOSIndepClick(Sender: TObject);
    procedure SpeedButtonHelpSetup1Click(Sender: TObject);
    procedure SpeedButtonHelpStartLinClick(Sender: TObject);
    procedure SpeedButtonHelpStartMacClick(Sender: TObject);
    procedure SpeedButtonHelpStartMultiClick(Sender: TObject);
    procedure SpeedButtonHelpStartWinClick(Sender: TObject);
    procedure TabSheetCreateShow(Sender: TObject);
    procedure TabSheetIconsShow(Sender: TObject);
    procedure TabSheetSetup1Enter(Sender: TObject);
    procedure TabSheetSetup2Enter(Sender: TObject);
    procedure TabSheetSetup3Enter(Sender: TObject);
    procedure TabSheetStartExit(Sender: TObject);
    procedure CallMakeProperties(Sender: TObject);
    procedure TabSheetShow(Sender: TObject);
    procedure TabSheetWingetContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TaskPanelWinClick(Sender: TObject);
    procedure TICheckBoxlicenseRequiredChange(Sender: TObject);
    procedure TICheckBoxS1MstChange(Sender: TObject);
    procedure TICheckBoxS1SilentChange(Sender: TObject);
    procedure TICheckBoxS2MstChange(Sender: TObject);
    procedure TIComboBoxChannelChange(Sender: TObject);
    procedure TIComboBoxChannelEditingDone(Sender: TObject);
    procedure WingetInputEditingDone(Sender: TObject);
    procedure TIEditProdIDChange(Sender: TObject);
    procedure TIEditProdIDExit(Sender: TObject);
    procedure TIEditProdIDSizeConstraintsChange(Sender: TObject);
    procedure TIEditProdVersion3Change(Sender: TObject);
    procedure TIEditProdVersion3Exit(Sender: TObject);
    procedure TIEditSetup1UnProgramEditingDone(Sender: TObject);
    procedure TIEditSetup2UnProgramEditingDone(Sender: TObject);
    procedure TIEditSetup3UnProgramEditingDone(Sender: TObject);
    procedure TIMemoAdviceEditingDone(Sender: TObject);
    procedure TIMemoDescEditingDone(Sender: TObject);
    procedure TimerFirstconfigTimer(Sender: TObject);
    procedure TIRadioGroupBuildModeClick(Sender: TObject);
    procedure TIS1UrlClick(Sender: TObject);
    procedure TIS1UrlMouseEnter(Sender: TObject);
    procedure TIS1UrlMouseLeave(Sender: TObject);
    procedure TISpinEditPrioChange(Sender: TObject);
    procedure TITrackBarPrioChange(Sender: TObject);
    procedure ApplicationEventIdle(Sender: TObject; var Done: boolean);
    procedure genRttiEditChange(Sender: TObject);

    procedure IconDisplayOnMouseEnter(Sender: TObject);
    procedure PaintPreview(Image: TImage);
    procedure IconDisplayOnClick(Sender: TObject);
    procedure showCheckEntriesWarning;
    function showCompleteDirDlg: boolean;
    procedure showMacOS2StepSelectionDLG;
    procedure TreeView1Click(Sender: TObject);
    procedure updateGUI;
  private
    { private declarations }
    procedure OpenMSTFile(var mysetup: TSetupFile);
    procedure chooseInstallDir(var mysetup: TSetupFile);
    procedure chooseUninstaller(var mysetup: TSetupFile);

    procedure SetTICheckBoxesMST(Installer: TKnownInstaller);
    procedure chooseTargetProgram(var mysetup: TSetupFile);
  public
    { public declarations }
    // create a FlowPanel dynamically to be able to free it before selecting a
    //new directory for the icon selection
    dynIconFlowPanel: TFlowPanel;
    // TFPObjectList needs Contnrs in uses
    IconList: TFPObjectList;
    SelectedIcon: TIconDisplay;
    iconDirectory: string;
    numberIcons, indexSelectedIcon: integer;
    loadDefaultIcon: boolean;
    procedure memoadd(line: string);
    procedure updateUninstaller(var mysetup: TSetupFile);
  end;


procedure checkWorkbench;
procedure initGUI;
procedure resetGUI;

const
  // base_url while development
  //opsidocs_base_url = 'https://bonifax/opsidoc/antora/';
  opsidocs_base_url = 'https://docs.opsi.org/';


var
  resultForm1: TresultForm1;
  myexitcode: integer;
  Result: integer;
  MSIfilename, MSTfilename, SetupFilename: string;
  configDir: string;
  configFileName: string;
  packetBaseDir: string;
  productid: string;
  fConfig: Text;
  setupTypestr: string;
  markerEmbeddedMSI: boolean = False;
  markerInstallShield: boolean = False;
  opsidir: string;   // opsi.org (set in main)
  opsitmp: string;   // %TEMP%/opsitmp  (set in main)

  //*****************************************
  test: boolean = False;
  overwriteExistingProduct: boolean = False;
  showMSI: boolean = True;
  showInnoSetup: boolean = True;
  showNSIS: boolean = True;
  showInstallShield: boolean = True;
  showInstallShieldMSI: boolean = True;
  //showAdvancedMSI: boolean = True;
  //*****************************************
  firstshowconfigdone: boolean = False;
  productIds: TStringList;
  designPPI: integer;
  screenPPI: integer;


resourcestring

  // old before 4.1.0.2 ******************************************************************
  sErrProductIdEmpty = 'Error: Field opsi Product ID is empty!';
  sErrProductVersionEmpty = 'Error: Field Product Version is empty!';
  sErrOpsiPackageBuilderStart = 'Error invoking opsi package builder';
  sErrPacketBaseDirNotFound =
    'Error: The path to the opsi_workbench is empty or not valid!';
  sInfoFinished = 'Create opsi package finished.';
  sInfoFailedBuild = 'Opsi package files created but build / install package failed';
  sInfoFailedCreate = 'Create opsi package files failed';
  sWarnInstalldirUnknown = 'Error: Field Install Directory is empty or not valid!' +
    Lineending + 'For this Installer the Installdir could not be detected.' +
    Lineending + 'The Installdir is needed for the uninstallation process.' +
    Lineending +
    'Please install this product manually and determine the Installdir. Then add it to the setup and the uninstall script';

  // new for 4.1.0.2 ******************************************************************
  rsNotImplemented = 'Not implemented right now.';
  rsWeNeedConfiguration = 'We need some configurations first !';
  rsTwonalyzeAndCreateMsgHead = 'opsi-setup-detector: Two File (32/64 Bit) Product';
  rsTwonalyzeAndCreateMsgFirstSetup = 'First Select the 32 Bit Setup exe';
  rsTwonalyzeAndCreateMsgSecondSetup = 'Now Select the 64 Bit Setup exe';
  rsThreeAnalyzeAndCreateMsgHead =
    'opsi-setup-detector: Multi Platform three File (Win/Lin/Mac) Product';
  rsThreeAnalyzeAndCreateMsgFirstSetup = 'Do you want to select a Windows Setup file ?';
  rsThreeAnalyzeAndCreateMsgSecondSetup =
    'Do you want to select a Linux installer file ?';
  rsThreeAnalyzeAndCreateMsgThirdSetup =
    'Do you want to select a MacOS installer file ?';

  rsPropEditErrorHead = 'opsi-setup-detector: Property Editor: Error';
  rsPropEditErrorDoubleMsgStart = 'property Id: ';
  rsPropEditErrorDoubleMsgFinish = ' exists. Duplicates not allowed.';
  rsPropEditErrorNoSelect = 'No Property selected.';
  rsDependencyEditErrorHead = 'opsi-setup-detector: Dependency Editor: Error';
  rsDependencyEditErrorNoSelect = 'No Dependency selected.';
  rsDefaultIcon = 'default icon';
  rsNumberIcons = 'Icons to choose from: ';
  rsCopyFileOnlyOrCompleteDirTitle = ' Only File or Directory';
  rsCopyFileOnlyOrCompleteDirText =
    'Should we copy only the installer file or the complete directory ?';
  rsCopyCompleteDirCap = 'Complete directory';
  rsCopyFileOnlyCap = 'Only selected file';
  //    'Should we copy not only the setup file. but the complete directory ?';
  rsSelectAppOrDir =
    'First select MacOS .app directory or a directory that contains the MacOS installer files';
  rsSelectMacFile = 'Now select the MacOS installer file.';
  rsRpmAnalyze = 'Analysis of RPM files';
  rsRPMAnalyzeNotLinux = 'Detailed analysis of rpm files can only be done in linux';
  rsDebAnalyze = 'Analysis of DEB files';
  rsDebAnalyzeNotLinux = 'Detailed analysis of deb files can only be done in linux';
  rscheckEntriesTitle = 'Please keep in mind:';
  rscheckEntriesMsg =
    'The following data fields are automatically detected.' +
    Lineending + 'You have to check every data field to verify the data is correct or plausible.'
    + Lineending + 'For some data fields you may have to install the program once manually'
    + Lineending + 'and then retrieve the needed data from the completed installation.';
  rscheckEntriesRememberMe = 'Do not show this Message again';
  rsMac3stepSelectionText = 'To select a MacOS installer there are two Steps' +
    LineEnding + 'The first step is a dialog to select a directory that contains the installer '
    + LineEnding + 'or the "installer.app" directory' + LineEnding +
    'The second step (if needed) is a dialog to select an installer file';
  rsMac3stepSelectionTitle = 'Attention: Two Step Selection Dialog';
  rsMacSelectionRememberMe = 'Do not show this Message again';
  rsServiceConnectionFailed =
    'Could not connect to the opsi-web-service. Check URL, user and password';
  rsServiceConnected = 'Connected to opsi server';
  rsServiceNotConnected = 'Not connected to opsi server';
  rsCreateWithUserProductAdvice =
    'Use Property "Debug = true" to disable ' + 'mouse and keyboard blocking.' +
    LineEnding +
    'If mouse and keyboard are accidentally blocked after the installation is finished,'
    +
    LineEnding + 'use action request "update" to enable mouse and keyboard again.';
  rsErrorLoadingLogViewer = 'An error occured while loading opsi-logviewer';
  rsErrorFindingLogViewer =
    'Please install the opsi-logviewer product. opsi-logviewer is not installed in ';
  rsRebuildFinished = 'Rebuild opsi package finished.';
  rsRebuildFailedBuild = 'Rebuild opsi package: build / install package failed';


  // Hints
  rsTemlateChannelHint =
    'Choose what kind of templates should be used. If the templates are not found, default is the fallback.';
  rsSupportCustomDirectoryHint = 'Should we add code to support "custom" directories ?';
  rsInstallFromLocalHint =
    'Should we add code to copy the installer locally before the installation ?';
  rsCustomizeProfileHint =
    'Should we add code to customize the installation in user profiles ?';
  rsUninstallBeforeInstallHint =
    'Should we add code to check if we uninstall before the installation ?';
  rsInstalldirHint =
    'The (unquoted) directory path where the software will be installed.' +
    LineEnding +
    'You may also choose the directory via the selection button on the right (if the product is installed).'
    + LineEnding +
    'If you there get a path like "C:\program Files" or "C:\program Files (x86)", ' +
    LineEnding +
    'it will be replaced by the matching opsi-script constant (e.g."%ProgramFiles32Dir%").';
  rsTargetProgHint =
    'The main program of the software that has to be installed.' +
    LineEnding + 'Will be used for creating desktop icons or start menu entries.' +
    LineEnding + 'and to check before installaion if the program is running.' +
    LineEnding +
    'Will be not detected. You have to choose it via the selection button on the right (if the product is installed).'
    + LineEnding + 'Unquoted file name.';
  rsUninstallProgHint =
    'The unqoted path to the detected uninstall program.' + LineEnding +
    'You may also choose the file via the selection button on the right (if the product is installed).';

  // Tabs
  rsTabStart = 'Start';
  rsTabAnalyze = 'Analyze';
  rsTabSetup1 = '1. Setup';
  rsTabSetup2 = '2. Setup';
  rsTabSetup3 = '3. Setup';
  rsTabWinget = 'winget';
  rsTabProduct1 = 'Product Configuration 1';
  rsTabProduct2 = 'Product Configuration 2';
  rsTabProductIcon = 'Product Icon';
  rsTabCreate = 'Create';


implementation

{$R *.lfm}

const
  CSSDecoration = '<style type="text/css">' + 'code{' + '  color: #A00;' +
    '}' + 'pre{' + '  background: #f4f4f4;' + '  border: 1px solid #ddd;' +
    '  border-left: 3px solid #f36d33;' + '  color: #555;' +
    '  overflow: auto;' + '  padding: 1em 1.5em;' + '  display: block;' +
    '}' + 'pre code{' + '  color: inherit;' + '}' + '</style>';

var
  md: TMarkdownProcessor;
  HtmlContent: string;


procedure initGUI;
begin
  LogDatei.log('Start initGUI ... ', LLInfo);
  with resultform1 do
  begin
    with aktProduct do
    begin
      // the links ...
      TILabelInstaller1.Link.SetObjectAndProperty(SetupFiles[0], 'installerid');
      TILabelInstaller2.Link.SetObjectAndProperty(SetupFiles[1], 'installerid');
      TILabelInstaller3.Link.SetObjectAndProperty(SetupFiles[2], 'installerid');
      TILabelInstaller4.Link.SetObjectAndProperty(SetupFiles[0], 'installerid');
      TIEditSetupfile1.Link.SetObjectAndProperty(SetupFiles[0], 'setupFullFileName');
      TIEditSetupFile2.Link.SetObjectAndProperty(SetupFiles[1], 'setupFullFileName');
      TIEditSetupFile3.Link.SetObjectAndProperty(SetupFiles[2], 'setupFullFileName');
      TIEditMstFile1.Link.SetObjectAndProperty(SetupFiles[0], 'mstFullFileName');
      TIEditMstFile2.Link.SetObjectAndProperty(SetupFiles[1], 'mstFullFileName');
      TIEditMstFile3.Link.SetObjectAndProperty(SetupFiles[2], 'mstFullFileName');
      TICheckBoxS1Mst.Link.SetObjectAndProperty(SetupFiles[0], 'mstAllowed');
      TICheckBoxS2Mst.Link.SetObjectAndProperty(SetupFiles[1], 'mstAllowed');
      TICheckBoxS3Mst.Link.SetObjectAndProperty(SetupFiles[2], 'mstAllowed');
      TIEditMsiId1.Link.SetObjectAndProperty(SetupFiles[0], 'msiId');
      TIEditMsiId2.Link.SetObjectAndProperty(SetupFiles[1], 'msiId');
      TIEditMsiId3.Link.SetObjectAndProperty(SetupFiles[2], 'msiId');
      TIEditMsiId4.Link.SetObjectAndProperty(SetupFiles[0], 'wingetId');
      TIEditMsiName1.Link.SetObjectAndProperty(SetupFiles[0], 'msiProductName');
      TIEditMsiName2.Link.SetObjectAndProperty(SetupFiles[1], 'msiProductName');
      TIEditMsiName3.Link.SetObjectAndProperty(SetupFiles[2], 'msiProductName');
      TIEditMsiName4.Link.SetObjectAndProperty(SetupFiles[0], 'wingetSource');
      TIEditMsiUpCode1.Link.SetObjectAndProperty(SetupFiles[0], 'msiUpgradeCode');
      TIEditMsiUpCode2.Link.SetObjectAndProperty(SetupFiles[1], 'msiUpgradeCode');
      TIEditMsiUpCode3.Link.SetObjectAndProperty(SetupFiles[2], 'msiUpgradeCode');
      TIEditSoftVersion1.Link.SetObjectAndProperty(SetupFiles[0], 'SoftwareVersion');
      TIEditSoftVersion2.Link.SetObjectAndProperty(SetupFiles[1], 'SoftwareVersion');
      TIEditSoftVersion3.Link.SetObjectAndProperty(SetupFiles[2], 'SoftwareVersion');
      TIEditSoftVersion4.Link.SetObjectAndProperty(SetupFiles[0], 'SoftwareVersion');
      TIEditSetupFileSizeMB1.Link.SetObjectAndProperty(SetupFiles[0], 'setupFileSize');
      TIEditSetupFileSizeMB2.Link.SetObjectAndProperty(SetupFiles[1], 'setupFileSize');
      TIEditSetupFileSizeMB3.Link.SetObjectAndProperty(SetupFiles[2], 'setupFileSize');
      TIEditSetupFileSizeMB4.Link.SetObjectAndProperty(SetupFiles[0], 'setupFileSize');
      TIEditRequiredSizeMB1.Link.SetObjectAndProperty(SetupFiles[0], 'requiredSpace');
      TIEditRequiredSizeMB2.Link.SetObjectAndProperty(SetupFiles[1], 'requiredSpace');
      TIEditRequiredSizeMB3.Link.SetObjectAndProperty(SetupFiles[2], 'requiredSpace');
      TIEditRequiredSizeMB4.Link.SetObjectAndProperty(SetupFiles[0], 'requiredSpace');
      TIEditInstallDir1.Link.SetObjectAndProperty(SetupFiles[0], 'installDirectory');
      TIEditInstallDir2.Link.SetObjectAndProperty(SetupFiles[1], 'installDirectory');
      TIEditInstallDir3.Link.SetObjectAndProperty(SetupFiles[2], 'installDirectory');
      TIEditInstallDir4.Link.SetObjectAndProperty(SetupFiles[0], 'installDirectory');
      TIEditSetup1Command.Link.SetObjectAndProperty(SetupFiles[0], 'installCommandLine');
      TIEditSetup2Command.Link.SetObjectAndProperty(SetupFiles[1], 'installCommandLine');
      TIEditSetup3Command.Link.SetObjectAndProperty(SetupFiles[2], 'installCommandLine');
      TIEditSetup4Command.Link.SetObjectAndProperty(SetupFiles[0], 'installCommandLine');
      TIEditSetup1UnCommand.Link.SetObjectAndProperty(SetupFiles[0],
        'uninstallCommandLine');
      TIEditSetup2UnCommand.Link.SetObjectAndProperty(SetupFiles[1],
        'uninstallCommandLine');
      TIEditSetup3UnCommand.Link.SetObjectAndProperty(SetupFiles[2],
        'uninstallCommandLine');
      TIEditSetup4UnCommand.Link.SetObjectAndProperty(SetupFiles[0],
        'uninstallCommandLine');
      TIEditSetup1UnProgram.Link.SetObjectAndProperty(SetupFiles[0], 'uninstallProg');
      TIEditSetup2UnProgram.Link.SetObjectAndProperty(SetupFiles[1], 'uninstallProg');
      TIEditSetup3UnProgram.Link.SetObjectAndProperty(SetupFiles[2], 'uninstallProg');
      TIEditSetup1TargetProgram.Link.SetObjectAndProperty(SetupFiles[0], 'targetProg');
      TIEditSetup2TargetProgram.Link.SetObjectAndProperty(SetupFiles[1], 'targetProg');
      TIEditSetup3TargetProgram.Link.SetObjectAndProperty(SetupFiles[2], 'targetProg');
      TIEditSetup4TargetProgram.Link.SetObjectAndProperty(SetupFiles[0], 'targetProg');
      TIS1Url.Link.SetObjectAndProperty(SetupFiles[0], 'link');
      TIS2Url.Link.SetObjectAndProperty(SetupFiles[1], 'link');
      TIS3Url.Link.SetObjectAndProperty(SetupFiles[2], 'link');
      TIS4Url.Link.SetObjectAndProperty(SetupFiles[0], 'link');
      TICheckBoxS1Silent.Link.SetObjectAndProperty(SetupFiles[0], 'preferSilent');
      TICheckBoxS2Silent.Link.SetObjectAndProperty(SetupFiles[1], 'preferSilent');
      TICheckBoxS3Silent.Link.SetObjectAndProperty(SetupFiles[2], 'preferSilent');
      TICheckBoxS4Silent.Link.SetObjectAndProperty(SetupFiles[0], 'preferSilent');
      // product
      TIEditProdVersion3.Link.SetObjectAndProperty(productdata, 'productVersion');
      TISpinEditPackageVers.Link.SetObjectAndProperty(productdata, 'packageVersion');
      TIEditProdID.Link.SetObjectAndProperty(productdata, 'productId');
      TIEditProdName.Link.SetObjectAndProperty(productdata, 'productName');
      TIMemoAdvice.Link.SetObjectAndProperty(productdata, 'advice');
      TIMemoDesc.Link.SetObjectAndProperty(productdata, 'description');
      TITrackBarPrio.Link.SetObjectAndProperty(productdata, 'priority');
      TISpinEditPrio.Link.SetObjectAndProperty(productdata, 'priority');
      // definition of class TProductData in unit osdbasedata line ~256
      TILabelDirSelIcon.Link.SetObjectAndProperty(osdbasedata.aktproduct.productdata,
        'productImageFullFileName');
      TIGridDep.ListObject := osdbasedata.aktproduct.dependencies;
      TIGridProp.ListObject := osdbasedata.aktproduct.properties;
      TICheckBoxCustomdir.Link.SetObjectAndProperty(productdata, 'useCustomDir');
      TICheckBoxInstallFromLocal.Link.SetObjectAndProperty(productdata,
        'installFromLocal');
      TICheckBoxHandleLiceneKey.Link.SetObjectAndProperty(productdata,
        'handleLicensekey');
      TICheckBoxDesktopIcon.Link.SetObjectAndProperty(productdata,
        'desktopicon');
      TICheckBoxCustomizeProfile.Link.SetObjectAndProperty(productdata,
        'customizeProfile');
      TICheckBoxUninstallBefore.Link.SetObjectAndProperty(productdata,
        'uninstallBeforeInstall');
      TIComboBoxChannel.Link.SetObjectAndProperty(productdata, 'channelDir');
      // initialize drop down
      TIComboBoxChannel.Items.Text := templateChannelList.Text;

      // reinit osd settings (changed language ?)
      osdsettings.BuildMode.Clear;
      osdsettings.BuildMode.Add(rsBuildRadioBuild);
      osdsettings.BuildMode.Add(rsBuildRadioBuildInstall);
      osdsettings.CreateMode.Clear;
      osdsettings.CreateMode.Add(rsCreateRadioFiles);
      osdsettings.CreateMode.Add(rsCreateRadioFilesBuild);
      osdsettings.CreateMode.Add(rsCreateRadioFilesPackageBuilder);

      TIRadioGroupCreateMode.Caption := rsCreateRadioGroupTitle;
      TIRadioGroupCreateMode.Items.Text := osdsettings.CreateMode.Text;
      TIRadioGroupCreateMode.Link.SetObjectAndProperty(osdsettings, 'CreateModeValue');

      TIRadioGroupBuildMode.Caption := rsBuildRadioGroupTitle;
      TIRadioGroupBuildMode.Items.Text := osdsettings.BuildMode.Text;
      TIRadioGroupBuildMode.Link.SetObjectAndProperty(osdsettings, 'BuildModeValue');

      // the hints ...
      TIComboBoxChannel.Hint := rsTemlateChannelHint;
      TICheckBoxHandleLiceneKey.Hint := rsUsePropLicenseOrPool;
      TICheckBoxInstallFromLocal.Hint := rsInstallFromLocalHint;
      TICheckBoxCustomdir.Hint := rsSupportCustomDirectoryHint;
      TICheckBoxDesktopIcon.Hint := rsUsePropDesktopicon;
      TICheckBoxCustomizeProfile.Hint := rsCustomizeProfileHint;
      TICheckBoxUninstallBefore.Hint := rsUninstallBeforeInstallHint;
      TIEditInstallDir1.Hint := rsInstalldirHint;
      TIEditInstallDir2.Hint := rsInstalldirHint;
      TIEditInstallDir3.Hint := rsInstalldirHint;
      TIEditSetup1UnProgram.Hint := rsUninstallProgHint;
      TIEditSetup2UnProgram.Hint := rsUninstallProgHint;
      TIEditSetup3UnProgram.Hint := rsUninstallProgHint;
      TIEditSetup1TargetProgram.Hint := rsTargetProgHint;
      TIEditSetup2TargetProgram.Hint := rsTargetProgHint;
      TIEditSetup3TargetProgram.Hint := rsTargetProgHint;
      TICheckBoxS1Silent.Hint := rsPreferSilent;
      TICheckBoxS2Silent.Hint := rsPreferSilent;
      TICheckBoxS3Silent.Hint := rsPreferSilent;


      TIEditInstallDir1.ShowHint := True;
      TIEditInstallDir2.ShowHint := True;
      TIEditInstallDir3.ShowHint := True;
      TIEditSetup1UnProgram.ShowHint := True;
      TIEditSetup2UnProgram.ShowHint := True;
      TIEditSetup3UnProgram.ShowHint := True;
      TIEditSetup1TargetProgram.ShowHint := True;
      TIEditSetup2TargetProgram.ShowHint := True;
      TIEditSetup3TargetProgram.ShowHint := True;
      TICheckBoxS1Silent.ShowHint := True;
      TICheckBoxS2Silent.ShowHint := True;
      TICheckBoxS3Silent.ShowHint := True;

    end;
    TIEditworkbenchpath.Link.SetObjectAndProperty(myconfiguration, 'workbench_path');
    (*
    case myconfiguration.CreateRadioIndex of
      0: RadioButtonCreateOnly.Checked := True;
      1: RadioButtonBuildPackage.Checked := True;
      2: RadioButtonPackageBuilder.Checked := True;
    end;
    case myconfiguration.BuildRadioIndex of
      0: radioBuildModebuildOnly.Checked := True;
      1: radioBuildModebuildInstall.Checked := True;
    end;
    *)
    // copy create and build radio button settings from configuration to
    // osdseetings and so also to GUI
    osdsettings.CreateModeIndex := myconfiguration.CreateRadioIndex;
    osdsettings.BuildModeIndex := myconfiguration.BuildRadioIndex;

    Visible := True;
    // Tabsheet icons
    TabSheetStart.ImageIndex := 0;
    TabSheetAnalyze.ImageIndex := 1;
    TabSheetSetup1.ImageIndex := 2;
    TabSheetSetup2.ImageIndex := 2;
    TabSheetSetup3.ImageIndex := 2;
    TabSheetProduct.ImageIndex := 3;
    TabSheetProduct2.ImageIndex := 3;
    TabSheetIcons.ImageIndex := 3;
    TabSheetCreate.ImageIndex := 4;
    // Tabsheet all enabled since 4.3.4
    TabSheetStart.Enabled := True;
    TabSheetAnalyze.Enabled := True;
    TabSheetSetup1.Enabled := True;
    TabSheetSetup2.Enabled := True;
    TabSheetSetup3.Enabled := True;
    TabSheetWinget.Enabled := True;
    TabSheetProduct.Enabled := True;
    TabSheetProduct2.Enabled := True;
    TabSheetIcons.Enabled := True;
    TabSheetCreate.Enabled := True;

    TimerFirstconfig.Enabled := True;
    // check if we may call package builder
    if fileexists(myconfiguration.PathToOpsiPackageBuilder) then
    begin
      //RadioButtonPackageBuilder.Enabled := True;
      // is not possible via rtti
    end
    else
    begin
      //RadioButtonPackageBuilder.Enabled := False;
      // is not possible via rtti
    end;
    // check if we may build and install
    if fileexists(myconfiguration.PathToOpsiPackageBuilder) or
      ((myconfiguration.Service_URL <> '') and (myconfiguration.Service_user <> '')) then
    begin
      //RadioButtonBuildPackage.Enabled := True;
      // is not possible via rtti
      //CheckGroupBuildMode.Enabled := True;
      TIRadioGroupBuildMode.Enabled := True;
    end
    else
    begin
      TIRadioGroupCreateMode.ItemIndex := 0;
      TIRadioGroupCreateMode.Enabled := False;
      TIRadioGroupBuildMode.Enabled := False;
      //RadioButtonBuildPackage.Enabled := False;
      //CheckGroupBuildMode.Enabled := True;
    end;

    //Treeview colors
    {$IFDEF WINDOWS}
    TreeView1.BackgroundColor := clBtnFace;
    TreeView1.SelectionColor := clWhite;
    TreeView1.SelectionFontColor:= clNavy;
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
    TreeView1.BackgroundColor := clWhite;
    TreeView1.SelectionColor := clBtnFace;
    TreeView1.SelectionFontColor:= clNavy;
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
    TreeView1.BackgroundColor := clWhite;
    TreeView1.SelectionColor := clMenuBar;
    TreeView1.SelectionFontColor:= clNavy;
    {$ENDIF DARWIN}
    //Treeview items enabled
    TreeView1.Items[0].Enabled:= true;  // start
    TreeView1.Items[1].Enabled:= true;  // analyze
    TreeView1.Items[2].Enabled:= true;  // setup1
    TreeView1.Items[3].Enabled:= false;  // setup2
    TreeView1.Items[4].Enabled:= false;  // setup3
    TreeView1.Items[5].Enabled:= false;  // winget
    TreeView1.Items[6].Enabled:= true;  // product1
    TreeView1.Items[7].Enabled:= true;  // product2
    TreeView1.Items[8].Enabled:= true;  // product3
    TreeView1.Items[9].Enabled:= true;  // create
    //Treeview items names
    TreeView1.Items[0].Text := rsTabStart;
    TreeView1.Items[1].Text := rsTabAnalyze;
    TreeView1.Items[2].Text := rsTabSetup1;
    TreeView1.Items[3].Text := rsTabSetup2;
    TreeView1.Items[4].Text := rsTabSetup3;
    TreeView1.Items[5].Text := rsTabWinget;
    TreeView1.Items[6].Text := rsTabProduct1;
    TreeView1.Items[7].Text := rsTabProduct2;
    TreeView1.Items[8].Text := rsTabProductIcon;
    TreeView1.Items[9].Text := rsTabCreate;


    EditLogInfo.Caption := 'More info in Log file: ' + LogDatei.FileName;
    Application.ProcessMessages;
  end;
  reload_installer_info_messages;
  LogDatei.log('Finished initGUI ... ', LLInfo);
end;

procedure resetGUI;
begin
  // set the default values that may changed by different runModes
  LogDatei.log('Start resetGUI ... ', LLInfo);
  with resultform1 do
  begin
    // the checkboxes
    TICheckBoxCustomdir.Enabled := True;
    TICheckBoxInstallFromLocal.Enabled := True;
    TICheckBoxHandleLiceneKey.Enabled := True;
    TICheckBoxDesktopIcon.Enabled := True;
    TICheckBoxCustomizeProfile.Enabled := True;
    TICheckBoxUninstallBefore.Enabled := True;
    (* disabled  for 4.3.4 with treeview control
    // the tabsheets
    TabSheetStart.Enabled := True;
    TabSheetAnalyze.Enabled := True;
    TabSheetSetup1.Enabled := True;
    TabSheetSetup2.Enabled := True;
    TabSheetProduct.Enabled := True;
    TabSheetProduct2.Enabled := True;
    TabSheetCreate.Enabled := True;
    *)
    //Treeview items
    TreeView1.Items[0].Enabled:= true;  // start
    TreeView1.Items[1].Enabled:= true;  // analyze
    TreeView1.Items[2].Enabled:= true;  // setup1
    TreeView1.Items[3].Enabled:= false;  // setup2
    TreeView1.Items[4].Enabled:= false;  // setup3
    TreeView1.Items[5].Enabled:= false;  // winget
    TreeView1.Items[6].Enabled:= true;  // product1
    TreeView1.Items[7].Enabled:= true;  // product2
    TreeView1.Items[8].Enabled:= true;  // product3
    TreeView1.Items[9].Enabled:= true;  // create

    // the buttons
    BtSetup1NextStep.Enabled := True;
    // treeview
    TreeView1.Select(TreeView1.Items[0]);
    Application.ProcessMessages;
  end;
  LogDatei.log('Finished resetGUI ... ', LLInfo);
end;

procedure TResultform1.updateGUI;
begin
  TIGridDep.ListObject := osdbasedata.aktproduct.dependencies;
  TIGridDep.ReloadTIList;
  TIGridDep.Update;
  TIGridProp.ListObject := nil;
  TIGridProp.ReloadTIList;
  TIGridProp.Update;
  Application.ProcessMessages;
  TIGridProp.ListObject := osdbasedata.aktproduct.properties;
  TIGridProp.ReloadTIList;
  TIGridProp.Update;
end;

procedure TResultform1.FormDestroy(Sender: TObject);
begin
  TIEditSetupfile1.Link.TIObject := nil;
  TIEditSetupFile2.Link.TIObject := nil;
  TILabelInstaller1.Link.TIObject := nil;
  TILabelInstaller2.Link.TIObject := nil;
  TIEditMstFile1.Link.TIObject := nil;
  TIEditMstFile2.Link.TIObject := nil;
  TIEditMsiId1.Link.TIObject := nil;
  TIEditMsiId2.Link.TIObject := nil;
  TIEditSoftVersion1.Link.TIObject := nil;
  TIEditSoftVersion2.Link.TIObject := nil;
  TIEditSetupFileSizeMB1.Link.TIObject := nil;
  TIEditSetupFileSizeMB2.Link.TIObject := nil;
  TIEditRequiredSizeMB1.Link.TIObject := nil;
  TIEditRequiredSizeMB2.Link.TIObject := nil;
  TIEditInstallDir1.Link.TIObject := nil;
  TIEditInstallDir1.Link.TIObject := nil;
  TIEditworkbenchpath.Link.TIObject := nil;
  TIEditSetup1Command.Link.TIObject := nil;
  TITrackBarPrio.Link.TIObject := nil;
  TISpinEditPrio.Link.TIObject := nil;
  TIS1Url.Link.TIObject := nil;
  TIS2Url.Link.TIObject := nil;
  TICheckBoxS1Mst.Link.TIObject := nil;
  TICheckBoxS2Mst.Link.TIObject := nil;
  OSD_info.Destroy;
  FreeAndNil(md);
end;


{ TResultform1 }

procedure TResultform1.setRunMode;
begin
  case osdsettings.runmode of

    analyzeOnly:
    begin
      (*
      TabSheetSetup2.Enabled := False;
      TabSheetProduct.Enabled := False;
      TabSheetProduct2.Enabled := False;
      TabSheetCreate.Enabled := False;
      *)
      BtSetup1NextStep.Enabled := False;
      (* lookup comment:
      //Treeview items in  resetgui
      TreeView1.Items[0].Enabled:= true;  // start
      TreeView1.Items[1].Enabled:= true;  // analyze
      TreeView1.Items[2].Enabled:= true;  // setup1
      TreeView1.Items[3].Enabled:= false;  // setup2
      TreeView1.Items[4].Enabled:= false;  // setup3
      TreeView1.Items[5].Enabled:= false;  // winget
      TreeView1.Items[6].Enabled:= true;  // product1
      TreeView1.Items[7].Enabled:= true;  // product2
      TreeView1.Items[8].Enabled:= true;  // product3
      TreeView1.Items[9].Enabled:= true;  // create
      *)
      //Treeview items in difference to default from resetgui
      TreeView1.Items[6].Enabled:= false;  // product1
      TreeView1.Items[7].Enabled:= false;  // product2
      TreeView1.Items[8].Enabled:= false;  // product3
      TreeView1.Items[9].Enabled:= false;  // create
    end;
    singleAnalyzeCreate, analyzeCreateWithUser:
    begin
      //Treeview items  in difference to default from resetgui
      // Nothing todo
    end;
    twoAnalyzeCreate_1, twoAnalyzeCreate_2:
    begin
      //Treeview items  in difference to default from resetgui
      TreeView1.Items[3].Enabled:= true;  // setup2
    end;
     threeAnalyzeCreate_1, threeAnalyzeCreate_2, threeAnalyzeCreate_3:
    begin
      //Treeview items  in difference to default from resetgui
      TreeView1.Items[3].Enabled:= true;  // setup2
      TreeView1.Items[4].Enabled:= true;  // setup3
    end;
    createTemplate, createTemplateWithUser:
    begin
      (*
      TabSheetAnalyze.Enabled := False;
      TabSheetSetup1.Enabled := False;
      TabSheetSetup2.Enabled := False;
      *)
      //Treeview items  in difference to default from resetgui
      TreeView1.Items[1].Enabled:= false;  // analyze
      TreeView1.Items[2].Enabled:= false;  // setup1
    end;
    createMeta:
    begin
      (*
      TabSheetAnalyze.Enabled := False;
      TabSheetSetup1.Enabled := False;
      TabSheetSetup2.Enabled := False;
      *)
      //Treeview items  in difference to default from resetgui
      TreeView1.Items[1].Enabled:= false;  // analyze
      TreeView1.Items[2].Enabled:= false;  // setup1
    end;
  end;

  case osdsettings.runmode of
    analyzeCreateWithUser, createTemplateWithUser:
    begin
      // disable some checkboxes for 'with user'
      TICheckBoxCustomdir.Enabled := False;
      TICheckBoxInstallFromLocal.Enabled := False;
      TICheckBoxDesktopIcon.Enabled := False;
      TICheckBoxUninstallBefore.Enabled := False;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TResultform1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TResultform1.FormDeactivate(Sender: TObject);
begin
  ActiveControl.EditingDone;
end;

procedure TResultform1.FormMouseLeave(Sender: TObject);
begin
  ActiveControl.EditingDone;
end;

procedure TResultform1.FormShow(Sender: TObject);
begin
  if not osdsettings.startupfinished then
    main;
end;

procedure TResultform1.IpHtmlPanelAdviceHotClick(Sender: TObject);
var
  //NodeA: TIpHtmlNodeA;
  URL: string;
begin
  (*
  if IpHtmlPanelAdvice.HotNode is TIpHtmlNodeA then
  begin
    NodeA := TIpHtmlNodeA(IpHtmlPanelAdvice.HotNode);
    URL := NodeA.HRef;
    OpenUrl(URL);
  end;
  *)
end;

procedure TResultform1.IpHtmlPanelDescClick(Sender: TObject);
begin
  // nothing
end;

procedure TResultform1.IpHtmlPanelDescHotClick(Sender: TObject);
var
  //NodeA: TIpHtmlNodeA;
  URL: string;
begin
(*
  if IpHtmlPanelDesc.HotNode is TIpHtmlNodeA then
  begin
    NodeA := TIpHtmlNodeA(IpHtmlPanelDesc.HotNode);
    URL := NodeA.HRef;
    OpenUrl(URL);
  end;
  *)
end;

procedure TResultform1.Label88Click(Sender: TObject);
begin

end;

procedure TResultform1.MenuHelpLogClick(Sender: TObject);
var
  ErrorMessage: string;
  PathOpsiLogViewer: string;
  paramstring: string;
  basepath: string;
begin
  {$IFDEF WINDOWS}
  basepath := 'C:\Program Files (x86)';
  if not DirectoryExists(basepath) then basepath := 'C:\Program Files';
  PathOpsiLogViewer :=
    basepath + '\opsi.org\configed\opsi-logviewer.exe';
  if not FileExists(PathOpsiLogViewer) then
    PathOpsiLogViewer :=
      basepath + '\opsi.org\opsi-logviewer\opsi-logviewer.exe';
  paramstring := LogDatei.FileName;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  // call of logviewer at linux (N.Otto 3.7.2023):
  // /usr/share/opsi-configed/java/jre/bin/java -jar "/usr/share/opsi-configed/configed.jar" --logviewer
  PathOpsiLogViewer := '/usr/share/opsi-configed/java/jre/bin/java';
  paramstring := ' -jar "/usr/share/opsi-configed/configed.jar" --logviewer -f ' +
    LogDatei.FileName;
  (*
  PathOpsiLogViewer := '/usr/share/opsi-logviewer/logviewer'; // '/usr/bin/logviewer'
  paramstring := ' -f '+LogDatei.FileName;
  *)
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  PathOpsiLogViewer := '/Applications/opsi-logviewer.app/Contents/MacOS/opsi-logviewer';
  paramstring := ' -f ' + LogDatei.FileName;
  //ShowMessage('Logview is temporary not working. Please use the opsi-logviewer product.');
  {$ENDIF DARWIN}
  if FileExists(PathOpsiLogViewer) then
  begin
    if ExecuteProcess(PathOpsiLogViewer, paramstring) <> 0 then
    begin
      ErrorMessage := rsErrorLoadingLogViewer;
      LogDatei.log(ErrorMessage, LLInfo);
      ShowMessage(ErrorMessage);
    end;
  end
  else
  begin
    ErrorMessage := rsErrorFindingLogViewer + PathOpsiLogViewer;
    LogDatei.log(ErrorMessage, LLInfo);
    ShowMessage(ErrorMessage);
  end;
end;

procedure TResultform1.MenuItem1Click(Sender: TObject);
begin
  //OSD_info := TOSD_info.Create(resultForm1);
  OSD_info.ShowModal;
  //OSD_info.Destroy;
end;

procedure TResultform1.MenuItemLogClick(Sender: TObject);
var
  ErrorMessage: string;
  PathOpsiLogViewer: string;
  paramstring: string;
  basepath: string;
begin
  {$IFDEF WINDOWS}
  basepath := 'C:\Program Files (x86)';
  if not DirectoryExists(basepath) then basepath := 'C:\Program Files';
  PathOpsiLogViewer :=
    basepath + '\opsi.org\configed\opsi-logviewer.exe';
  if not FileExists(PathOpsiLogViewer) then
    PathOpsiLogViewer :=
      basepath + '\opsi.org\opsi-logviewer\opsi-logviewer.exe';
  paramstring := LogDatei.FileName;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  // call of logviewer at linux (N.Otto 3.7.2023):
  // /usr/share/opsi-configed/java/jre/bin/java -jar "/usr/share/opsi-configed/configed.jar" --logviewer
  PathOpsiLogViewer := '/usr/share/opsi-configed/java/jre/bin/java';
  paramstring := ' -jar "/usr/share/opsi-configed/configed.jar" --logviewer -f ' +
    LogDatei.FileName;
  (*
  PathOpsiLogViewer := '/usr/share/opsi-logviewer/logviewer'; // '/usr/bin/logviewer'
  paramstring := ' -f '+LogDatei.FileName;
  *)
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  PathOpsiLogViewer := '/Applications/opsi-logviewer.app/Contents/MacOS/opsi-logviewer';
  paramstring := ' -f ' + LogDatei.FileName;
  //ShowMessage('Logview is temporary not working. Please use the opsi-logviewer product.');
  {$ENDIF DARWIN}
  if FileExists(PathOpsiLogViewer) then
  begin
    if ExecuteProcess(PathOpsiLogViewer, paramstring) <> 0 then
    begin
      ErrorMessage := rsErrorLoadingLogViewer;
      LogDatei.log(ErrorMessage, LLInfo);
      ShowMessage(ErrorMessage);
    end;
  end
  else
  begin
    ErrorMessage := rsErrorFindingLogViewer + PathOpsiLogViewer;
    LogDatei.log(ErrorMessage, LLInfo);
    ShowMessage(ErrorMessage);
  end;
end;

procedure TResultform1.MenuItemOpenControlClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 9;   // control file
  if DirectoryExists(myconfiguration.LastProjectFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastProjectFileDir
  else if DirectoryExists(myconfiguration.workbench_Path) then
    OpenDialog1.InitialDir := myconfiguration.workbench_Path;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastProjectFileDir := ExtractFileDir(OpenDialog1.FileName);
    LogDatei.log('Start open control file as new project from: ' +
      OpenDialog1.FileName, LLnotice);
    initaktproduct;
    resultform1.updateGUI;
    aktProduct.readControlFile(OpenDialog1.FileName, False);
    TIGridDep.ListObject := osdbasedata.aktproduct.dependencies;
    TIGridDep.ReloadTIList;
    TIGridDep.Update;
    TIGridProp.ListObject := osdbasedata.aktproduct.properties;
    TIGridProp.ReloadTIList;
    TIGridProp.Update;
    LogDatei.log('Finished open control file as new project from: ' +
      OpenDialog1.FileName, LLnotice);
  end;
end;

procedure TResultform1.MenuItemOpenProjClick(Sender: TObject);
begin
  resetGUI;
  OpenDialog1.FilterIndex := 8;   // project file
  if DirectoryExists(myconfiguration.LastProjectFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastProjectFileDir
  else if DirectoryExists(myconfiguration.workbench_Path) then
    OpenDialog1.InitialDir := myconfiguration.workbench_Path;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastProjectFileDir := ExtractFileDir(OpenDialog1.FileName);
    LogDatei.log('Start import Project file from: ' + OpenDialog1.FileName, LLnotice);
    initaktproduct;
    resultform1.updateGUI;
    aktProduct.readProjectFile(OpenDialog1.FileName);
    TIGridDep.ListObject := osdbasedata.aktproduct.dependencies;
    TIGridDep.ReloadTIList;
    TIGridDep.Update;
    TIGridProp.ListObject := osdbasedata.aktproduct.properties;
    TIGridProp.ReloadTIList;
    TIGridProp.Update;
    // process osdsettings.runmode
    setRunMode;
    LogDatei.log('Finished import Project file from: ' + OpenDialog1.FileName, LLnotice);
  end;
end;

procedure TResultform1.MenuItemSaveProjClick(Sender: TObject);
begin
  if SaveDialogProj.Execute then
  begin
    aktProduct.writeProjectFileToFile(SaveDialogProj.FileName);
    LogDatei.log('Write Project file to: ' + SaveDialogProj.FileName, LLnotice);
  end;
end;

procedure TResultform1.MenuItemLangClick(Sender: TObject);
begin

end;

procedure TResultform1.MenuItemLangDeClick(Sender: TObject);
begin
  osdsettings.mylang := 'de';
  SetDefaultLang('de', osdsettings.mylocaledir);
  initGUI;
end;

procedure TResultform1.MenuItemLangEnClick(Sender: TObject);
begin
  osdsettings.mylang := 'en';
  SetDefaultLang('en', osdsettings.mylocaledir);
  initGUI;
end;

procedure TResultform1.MenuItemLangEsClick(Sender: TObject);
begin
  osdsettings.mylang := 'en';
  SetDefaultLang('es', osdsettings.mylocaledir);
  initGUI;
end;

procedure TResultform1.MenuItemLangFrClick(Sender: TObject);
begin
  osdsettings.mylang := 'fr';
  SetDefaultLang('fr', osdsettings.mylocaledir);
  initGUI;
end;

procedure TResultform1.MenuItemStartClick(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheetStart;
end;

procedure TResultform1.MenuItemConfigClick(Sender: TObject);
var
  Streamer: TJSONStreamer;
  JSONString: string;
begin
  logdatei.log('Start MenuItemConfigClick', LLDebug2);
  FOSDConfigdlg.ShowModal;
  logdatei.log('After configdialog: create jsonstr', LLDebug2);
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
    JSONString := Streamer.ObjectToJSONString(myconfiguration);
    logdatei.log('After configdialog: ' + JSONString, LLDebug2);
    myconfiguration.writeconfig;
  finally
    Streamer.Destroy;
  end;
  if fileexists(myconfiguration.PathToOpsiPackageBuilder) then
  begin
    logdatei.log('After configdialog: packagebuilder exists', LLDebug2);
    //RadioButtonBuildPackage.Enabled := True;
    // is not pössible via rtti
    //RadioButtonPackageBuilder.Enabled := True;
    // is not pössible via rtti
    //CheckGroupBuildMode.Enabled := True;

    // enable build
    TIRadioGroupBuildMode.Enabled := True;
  end
  else
  begin
    logdatei.log('After configdialog: packagebuilder not found', LLDebug2);
    //RadioButtonBuildPackage.Enabled := False;
    // is not possible via rtti
    //RadioButtonPackageBuilder.Enabled := False;
    // is not pössible via rtti
    //CheckGroupBuildMode.Enabled := False;

    // disable build if also no service data
    if ((myconfiguration.Service_URL = '') or (myconfiguration.Service_user = '')) then
      TIRadioGroupBuildMode.Enabled := False;
  end;
  logdatei.log('Finished MenuItemConfigClick', LLDebug2);
end;

procedure TResultform1.MenuItemKnownInstallersClick(Sender: TObject);
var
  installerstr: string;
  installer: TKnownInstaller;
  installerlist: TStringList;
begin
  installerlist := TStringList.Create;
  for installer := Low(TKnownInstaller) to High(TKnownInstaller) do
    if not (installer = stUnknown) or (installer = stDetectedUnknown) then
      installerlist.Add(installerToInstallerstr(installer));
  //installerstr := installerstr + installerToInstallerstr(installer) + LineEnding;
  installerlist.Sort;
  installerstr := installerlist.Text;
  ShowMessage(installerstr);
  installerlist.Free;
end;

procedure TResultform1.OpenDialog1CanClose(Sender: TObject; var CanClose: boolean);
begin
  Logdatei.log('opendialog ccl folder: ' + TOpenDialog(Sender).FileName +
    '  -  ' + BoolToStr(canclose), LLInfo);
end;

procedure TResultform1.OpenDialog1Close(Sender: TObject);
begin
  Logdatei.log('opendialog cl folder: ' + TOpenDialog(Sender).FileName, LLInfo);
end;

procedure TResultform1.OpenDialog1FolderChange(Sender: TObject);
begin
  Logdatei.log('opendialog fc folder: ' + TOpenDialog(Sender).FileName, LLInfo);

end;

procedure TResultform1.OpenDialog1SelectionChange(Sender: TObject);
var
  ext: string;
begin
  Logdatei.log('opendialog sc folder: ' + TOpenDialog(Sender).FileName, LLInfo);
  ext := ExtractFileExt(TOpenDialog(Sender).FileName);
  if ext = '.app' then
    OpenDialog1.Options := [ofAllowMultiSelect, ofNoChangeDir,
      ofNoValidate, ofEnableSizing, ofViewDetail]
  else
    OpenDialog1.Options := [ofEnableSizing, ofViewDetail];

end;

procedure TResultform1.OpenDialog1Show(Sender: TObject);
begin
  // the case insensitive trick implemented in the changeFilenameFilter
  // does not work with macos
  // but macos is case sensitive - so we choose alway 'any file (*.*)'
  {$IFDEF DARWIN}
  OpenDialog1.FilterIndex := 7;
  {$ENDIF DARWIN}
end;


procedure TResultform1.BtSingleAnalyzeAndCreateWinClick(Sender: TObject);
var
  localTOSset: TTargetOSset;
begin
  resetGUI;
  openDialog1.FilterIndex := 1;   // setup
  if DirectoryExists(myconfiguration.LastSetupFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastSetupFileDir;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastSetupFileDir := ExtractFileDir(OpenDialog1.FileName);
    osdsettings.runmode := singleAnalyzeCreate;
    setRunMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    localTOSset := aktProduct.productdata.targetOSset;
    Include(localTOSset, osWin);
    aktProduct.productdata.targetOSset := localTOSset;
    aktProduct.SetupFiles[0].targetOS := osWin;
    MemoAnalyze.Clear;
    aktProduct.SetupFiles[0].copyCompleteDir := showCompleteDirDlg;
    makeProperties;
    resultform1.updateGUI;
    Application.ProcessMessages;
    aktProduct.SetupFiles[0].active := True;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;


procedure TResultform1.IconDisplayOnMouseEnter(Sender: TObject);
var
  index: integer;
begin
  for index := 0 to (numberIcons - 1) do
  begin
    TIconDisplay(IconList.Items[index]).Panel.Color := clDefault;
  end;
  // set background color of icon display on mouse entering
  if Sender.ClassName = 'TImage' then
    (Sender as TImage).Parent.Color := clSkyBlue;
  if Sender.ClassName = 'TPanel' then
    (Sender as TPanel).Color := clSkyBlue;
end;


// paint icon preview with selected background
procedure TResultform1.PaintPreview(Image: TImage);
var
  RectBackgr: TRect;
  row, col: integer;
  squaresize: integer;
  // chess background as no background
  ChessColors: array[0..1] of TColor = (clMedGray, clSilver);
  picturesize: integer;
  scalefactor: double;
  TempImage: TImage;
begin
  scalefactor := screenPPI / designPPI;
  LogDatei.log('scalefactor: ' + FloatToStr(scalefactor), LLnotice);
  // scale TIImageIconPreview only the first call
  if TIImageIconPreview.Width = 160 then
    with TIImageIconPreview do
    begin
      Top := round(top * scalefactor);
      Left := round(left * scalefactor);
      Width := round(Width * scalefactor);
      Height := round(Height * scalefactor);
    end;
  LogDatei.log('TIImageIconPreview.width : ' + IntToStr(
    TIImageIconPreview.Width), LLnotice);
  with TIImageIconPreview.Canvas do
  begin
    // paint chess background
    squaresize := 22;
    // scale background
    squaresize := round(squaresize * scalefactor);
    for row := 0 to 9 do
    begin
      for col := 0 to 9 do
      begin
        // paint chess squares
        Brush.Color := ChessColors[(row + col) mod 2];
        FillRect(Rect(squaresize * row, squaresize * col, squaresize *
          row + squaresize, squaresize * col + squaresize));
      end;
    end;
    // paint chess board

    // get scaled picturesize
    picturesize := TIImageIconPreview.Width;

    {$IFDEF LINUX}
    // scale rect
    picturesize := round(picturesize * (screenPPI / designPPI));
    {$ENDIF LINUX}
    // rect we want to paint to (with border of 2 pixel)
    RectBackgr := Rect(2, 2, picturesize - 2, picturesize - 2);
    // paint icon on chess board
    // copy the image
    TempImage := TImage.Create(nil);
    //TempImage.Picture.Bitmap := Image.Picture.Bitmap;
    TempImage.Picture.Assign(Image.Picture);
    // paint stretched:
    TIImageIconPreview.Proportional := True;
    TIImageIconPreview.Center := True;
    TIImageIconPreview.Canvas.StretchDraw(RectBackgr, TempImage.Picture.Bitmap);

  (*
  // original size:
    Draw(round((picturesize - Image.Picture.Width) / 2), round(
      (picturesize - Image.Picture.Height) / 2), Image.Picture.Bitmap);
    //end;
  *)

  end;
end;

procedure TResultform1.IconDisplayOnClick(Sender: TObject);
var
  index: integer;
  IconFileName: string;
begin
  // search in IconList for index of selected icon...
  // if image clicked
  if Sender.ClassName = 'TImage' then
  begin
    for index := 0 to (numberIcons - 1) do
    begin
      if TIconDisplay(IconList.Items[index]).Image = (Sender as TImage) then
      begin
        indexSelectedIcon := index;
        break;
      end;
    end;
    // if panel clicked
  end;
  if Sender.ClassName = 'TPanel' then
  begin
    for index := 0 to (numberIcons - 1) do
    begin
      if TIconDisplay(IconList.Items[index]).Panel = (Sender as TPanel) then
      begin
        indexSelectedIcon := index;
        break;
      end;
    end;
  end;
  // get selected icon from IconList
  SelectedIcon := TIconDisplay(IconList.Items[indexSelectedIcon]);
  // get file name without file extension
  IconFileName := SelectedIcon.FileName;
  Delete(IconFileName, Pos('.', IconFileName), IconFileName.Length - 1);
  // show name and directory of selected icon
  LabelIconName.Visible := True;
  LabelNameSelIcon.Caption := IconFileName;
  LabelIconDir.Visible := True;
  TILabelDirSelIcon.Visible := True;
  // save icon directory in productdata.productImageFullFileName
  // and therefore show it in TILabelDirSelIcon
  // (definition of class TProductData in unit osdbasedata line ~256)
  osdbasedata.aktProduct.productdata.productImageFullFileName :=
    iconDirectory + SelectedIcon.FileName;
  //ShowMessage(osdbasedata.aktProduct.productdata.productImageFullFileName);
  // paint icon in preview
  TIImageIconPreview.Visible := True;
  PaintPreview(SelectedIcon.Image);
  // adjust checkboxes
  CheckBoxNoIcon.Checked := False;
  CheckBoxDefaultIcon.Checked := False;
end;

procedure TResultform1.BtnOpenIconFolderClick(Sender: TObject);
var
  selectDirectory, loadFromFile: boolean;
  Background: TPanel;
  Image: TImage;
  IconDisplay: TIconDisplay;
  IconSearch: TSearchRec;
begin
  // select new directory the icons shall come from
  try
    {$IFDEF UNIX}
    SelectDirectoryDialog1.InitialDir := '/usr/share/opsi-setup-detector/icons';
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    SelectDirectoryDialog1.InitialDir :=
      ExtractFileDir(Application.Params[0]) + PathDelim + 'icons';
    {$ENDIF WINDOWS}
    if DirectoryExists(myconfiguration.LasticonFileDir) then
      SelectDirectoryDialog1.InitialDir := myconfiguration.LasticonFileDir;
    selectDirectory := SelectDirectoryDialog1.Execute;
  except
    on E: Exception do
    begin
      ShowMessage('Exception while preview directory: ' + E.Message);
      selectDirectory := False;
    end;
  end;
  // if directory selected
  // fill dynIconFlowPanel and store the icon display infos in IconList
  if selectDirectory then
  begin
    myconfiguration.LasticonFileDir := ExtractFileDir(SelectDirectoryDialog1.FileName);
    iconDirectory := SelectDirectoryDialog1.FileName + PathDelim;
    LogDatei.log('Open Icon dir: ' + iconDirectory, LLnotice);
    // get all files from the selected directory
    if FindFirst(iconDirectory + '*', faAnyFile, IconSearch) = 0 then
    begin
      // clear IconList and FlowPanel for new directory
      IconList.Free;
      dynIconFlowPanel.Free;
      // create new dynIconFlowPanel
      dynIconFlowPanel := TFlowPanel.Create(TabSheetIcons);
      dynIconFlowPanel.Parent := ScrollBox1;
      dynIconFlowPanel.Align := TAlign.alClient;
      // create new IconList
      IconList := TFPObjectList.Create;
      numberIcons := 0;
      repeat
        Image := TImage.Create(TabSheetIcons);
        // now load icon into Image
        loadFromFile := True;
        // easiest way to get all files with supported image formats:
        // first do FindFirst(directory + '*', faAnyFile, IconSearch) then try
        // Image.Picture.LoadFromFile(directory + IconSearch.Name) but
        // raise no exceptions because that's disturbing and annoying,
        // so the not-images are sorted out
      try
        // IconSearch.Name = name of icon file as string
        Image.Picture.LoadFromFile(iconDirectory + IconSearch.Name);
      except
        on E: Exception do
        begin
          //ShowMessage('Exception while load from file: ' + E.Message);
          loadFromFile := False;
          Image.Free;
        end;
      end;
        if loadFromFile then
        begin
          // create images (name: Image) on panels (name: Background) in FlowPanel
          // panel properties
          Background := TPanel.Create(TabSheetIcons);
          Background.Parent := dynIconFlowPanel;
          Background.AutoSize := True;
          Background.BorderSpacing.Around := 5;
          Background.Height := 50;
          Background.Width := 50;
          Background.BevelOuter := bvNone;
          // panel procedures
          Background.OnMouseEnter := @IconDisplayOnMouseEnter;
          Background.OnClick := @IconDisplayOnClick;
          // image properties
          Image.Parent := Background;
          Image.Stretch := True;
          Image.BorderSpacing.Around := 5;
          Image.Height := 50;
          Image.Width := 50;
          // image procedures
          Image.OnMouseEnter := @IconDisplayOnMouseEnter;
          Image.OnClick := @IconDisplayOnClick;

          // store panel, image and file name as one object IconDisplay in
          // IconList for later use
          IconDisplay := TIconDisplay.Create;
          IconDisplay.Panel := Background;
          IconDisplay.Image := Image;
          IconDisplay.FileName := IconSearch.Name;
          IconList.Add(IconDisplay);
          Image.Update;
          Inc(numberIcons);
        end;
        if numberIcons mod 10 = 0 then
        begin
          LabelNumber.Caption := IntToStr(numberIcons);
          Application.ProcessMessages;
        end;
      until FindNext(IconSearch) <> 0; // I have no idea how they are ordered
      FindClose(IconSearch);
    end;
    LabelNumIcons.Caption := rsNumberIcons;
    LogDatei.log('Finished Icon dir: ' + iconDirectory +
      ' number of icons: ' + IntToStr(numberIcons), LLnotice);
    // part of LabelNumber.Caption in po-files deleted so that the caption does
    // not change to 0 when changing the language
    LabelNumber.Caption := IntToStr(numberIcons);
  end;
end;

procedure TResultform1.BtSingleAnalyzeAndCreateWithUserClick(Sender: TObject);
var
  localTOSset: TTargetOSset;
begin
  resetGUI;
  openDialog1.FilterIndex := 1;   // setup
  if DirectoryExists(myconfiguration.LastSetupFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastSetupFileDir;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastSetupFileDir := ExtractFileDir(OpenDialog1.FileName);
    osdsettings.runmode := analyzeCreateWithUser;
    setRunMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    localTOSset := aktProduct.productdata.targetOSset;
    Include(localTOSset, osWin);
    aktProduct.productdata.targetOSset := localTOSset;
    aktProduct.SetupFiles[0].targetOS := osWin;
    MemoAnalyze.Clear;
    aktProduct.SetupFiles[0].copyCompleteDir := showCompleteDirDlg;
    makeProperties;
    resultform1.updateGUI;
    aktProduct.productdata.setupscript := 'setup.opsiscript';
    aktProduct.productdata.uninstallscript := 'localsetup\uninstall-local.opsiscript';
    aktProduct.productdata.updatescript := 'localsetup\update-local.opsiscript';
    aktProduct.productdata.priority := -20;
    aktProduct.productdata.advice := rsCreateWithUserProductAdvice;
    resultform1.updateGUI;
    Application.ProcessMessages;
    aktProduct.SetupFiles[0].active := True;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;

procedure TResultform1.BtWingetNextStepClick(Sender: TObject);
var
  checkok: boolean = True;
begin
  if ((aktProduct.SetupFiles[0].installDirectory = '') or
    (aktProduct.SetupFiles[0].installDirectory = 'unknown')) and
    (aktProduct.SetupFiles[0].installerId <> stMsi) and
    (aktProduct.SetupFiles[0].targetOS = osWin) then
  begin
    // checkok := False;
    // we warn here only
    ShowMessage(sWarnInstalldirUnknown);
  end;
  if checkok then
  begin

    case osdsettings.runmode of
      analyzeOnly, singleAnalyzeCreate, analyzeCreateWithUser,
      twoAnalyzeCreate_2,
      createTemplate,
      createTemplateWithUser,
      createMeta, gmUnknown,
      threeAnalyzeCreate_3:
      begin
        // we should never be here
        logdatei.log('Error: in BtWingetNextStepClick RunMode:' +
          GetEnumName(TypeInfo(TRunMode), Ord(osdsettings.runmode)), LLError);
      end;
      createWingetProd:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct;
        Application.ProcessMessages;
      end;
    end;
  end;
end;

procedure TResultform1.CheckBoxDefaultIconChange(Sender: TObject);
var
  DefaultIcon: TImage;
begin
  if CheckBoxDefaultIcon.Checked = True then
  begin
    { show name 'default icon' but no directory }
    LabelIconName.Visible := True;
    LabelNameSelIcon.Caption := rsDefaultIcon;
    LabelIconDir.Visible := False;
    TILabelDirSelIcon.Visible := False;

    osdbasedata.aktProduct.productdata.productImageFullFileName :=
      defaultIconFullFileName;
    CheckBoxNoIcon.Checked := False;
    TIImageIconPreview.Visible := True;
    // paint icon preview
    DefaultIcon := TImage.Create(TabSheetIcons);
    {$IFDEF LINUX}
    // scale image
    DefaultIcon.AutoAdjustLayout(lapAutoAdjustForDPI, 91, screen.PixelsPerInch, 0, 0);
    {$ENDIF LINUX}
    DefaultIcon.Picture.LoadFromFile(
      osdbasedata.aktProduct.productdata.productImageFullFileName);
    PaintPreview(DefaultIcon);
  end;
  // if CheckBoxDefaultIcon is unchecked then check CheckBoxNoIcon if no custom icon is selected
  // LabelIconDir.Visible = False is equivalent to no custom icon is selected
  if (CheckBoxDefaultIcon.Checked = False) and (LabelIconDir.Visible = False) then
    CheckBoxNoIcon.Checked := True;
end;

procedure TResultform1.CheckBoxNoIconChange(Sender: TObject);
begin
  if CheckBoxNoIcon.Checked = True then
  begin
    LabelIconName.Visible := False;
    LabelIconDir.Visible := False;
    LabelNameSelIcon.Caption := '';
    osdbasedata.aktProduct.productdata.productImageFullFileName := '';
    CheckBoxDefaultIcon.Checked := False;
    TIImageIconPreview.Visible := False;
  end;
  // if CheckBoxNoIcon is unchecked then check CheckBoxDefaultIcon if no custom icon is selected
  // LabelIconDir.Visible = False is equivalent to no custom icon is selected
  if (CheckBoxNoIcon.Checked = False) and (LabelIconDir.Visible = False) then
    CheckBoxDefaultIcon.Checked := True;
end;


procedure TResultform1.FlowPanel14Click(Sender: TObject);
begin

end;

procedure TResultform1.FlowPanel18Click(Sender: TObject);
begin

end;

procedure TResultform1.FlowPanel3Click(Sender: TObject);
begin

end;

procedure TResultform1.BtATwonalyzeAndCreateClick(Sender: TObject);
var
  localTOSset: TTargetOSset;
begin
  resetGUI;
  MessageDlg(rsTwonalyzeAndCreateMsgHead, rsTwonalyzeAndCreateMsgFirstSetup,
    mtInformation, [mbOK], '');
  OpenDialog1.FilterIndex := 1;   // setup
  if DirectoryExists(myconfiguration.LastSetupFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastSetupFileDir;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastSetupFileDir := ExtractFileDir(OpenDialog1.FileName);
    osdsettings.runmode := twoAnalyzeCreate_1;
    setRunMode;
    aktProduct.SetupFiles[0].copyCompleteDir := showCompleteDirDlg;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    MemoAnalyze.Clear;
    Application.ProcessMessages;
    initaktproduct;
    localTOSset := aktProduct.productdata.targetOSset;
    Include(localTOSset, osWin);
    aktProduct.productdata.targetOSset := localTOSset;
    aktProduct.SetupFiles[0].targetOS := osWin;
    // start add property
    makeProperties;
    resultform1.updateGUI;
    aktProduct.SetupFiles[0].active := True;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;

procedure TResultform1.BtCreateEmptyTemplateMultiClick(Sender: TObject);
begin
  resetGUI;
  osdsettings.runmode := createMultiTemplate;
  setRunMode;
  MemoAnalyze.Clear;
  PageControl1.ActivePage := resultForm1.TabSheetProduct;
  Application.ProcessMessages;
  initaktproduct;
  makeProperties;
  resultform1.updateGUI;
  aktProduct.productdata.targetOSset := [osWin, osLin, osMac, osMulti];
  aktProduct.productdata.productId := 'opsi-template';
  aktProduct.productdata.productName := 'opsi template for multi platform';
  aktProduct.productdata.productversion := '1.0.0';
  aktProduct.productdata.packageversion := 1;
  aktProduct.productdata.description :=
    'A template for opsi products for Win, Lin, Mac';
end;

procedure TResultform1.showCheckEntriesWarning;
var
  tmpbool: boolean;
begin
  if myconfiguration.ShowCheckEntryWarning then
  begin
    // https://specials.rejbrand.se/TTaskDialog/
    with TTaskDialog.Create(self) do
    try
      Caption := 'opsi-setup-detector';
      Title := rscheckEntriesTitle;
      Text := rscheckEntriesMsg;
      CommonButtons := [tcbOk];
      MainIcon := tdiInformation;
      VerificationText := rscheckEntriesRememberMe;
      Execute;
      tmpbool := tfVerificationFlagChecked in Flags;
      myconfiguration.ShowCheckEntryWarning := not (tmpbool);
      tmpbool := myconfiguration.ShowCheckEntryWarning;
      myconfiguration.writeconfig;
    finally
      Free;
    end;
  end;
end;

procedure TResultform1.BtAnalyzeNextStepClick(Sender: TObject);
begin
  showCheckEntriesWarning;
  case osdsettings.runmode of
    analyzeOnly:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      Application.ProcessMessages;
    end;
    singleAnalyzeCreate, analyzeCreateWithUser:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      Application.ProcessMessages;
    end;
    twoAnalyzeCreate_1:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      Application.ProcessMessages;
    end;
    twoAnalyzeCreate_2:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup2;
      Application.ProcessMessages;
    end;
    threeAnalyzeCreate_1:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      Application.ProcessMessages;
    end;
    threeAnalyzeCreate_2:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup2;
      Application.ProcessMessages;
    end;
    threeAnalyzeCreate_3:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup3;
      Application.ProcessMessages;
    end;

    createTemplate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      Application.ProcessMessages;
    end;
    createMeta:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetProduct;
      Application.ProcessMessages;
    end;
    gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup1NextStepClick osdsettings.runmode: ' +
        GetEnumName(TypeInfo(TRunMode), Ord(osdsettings.runmode)), LLError);
    end;
  end;
end;

procedure TResultform1.BitBtnWorkBenchPathClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    myconfiguration.workbench_Path := SelectDirectoryDialog1.FileName;
  checkWorkbench;
end;

procedure TResultform1.BitBtnRecheckWorkbenchClick(Sender: TObject);
begin
  checkWorkbench;
end;

procedure TResultform1.BitBtnAddDepClick(Sender: TObject);
var
  mydep: TPDependency;
begin
  startOpsiServiceConnection;
  FNewDepDlg.ComboBoxActState.Text := '';
  FNewDepDlg.RadioButtonState.Checked := True;
  FNewDepDlg.RadioButtonActionChange(Sender);

  // avoid nonsense dependencies in meta product
  if osdsettings.runmode = createMeta then
  begin
    // set required mode to 'state'
    FNewDepDlg.RadioButtonState.Checked := True;
    FNewDepDlg.RadioButtonAction.Checked := False;
    // fill combobox 'Action or State'
    FNewDepDlg.RadioButtonActionChange(Sender);
    // set Requirement type to empty
    FNewDepDlg.ComboBoxReqType.Text := '';
    // deny requirement types before / after
    FNewDepDlg.ComboBoxReqType.Enabled := False;
    // allow  action setup for nested meta products
    // (std is state installed)
    FNewDepDlg.ComboBoxActState.Enabled := False;
  end
  else
  begin
    FNewDepDlg.ComboBoxActState.Enabled := True;
    FNewDepDlg.ComboBoxReqType.Enabled := True;
  end;
  // needed for macos:
  FNewDepDlg.Repaint;
  procmess;
  if FNewDepDlg.ShowModal = mrOk then
  begin
    //add
    mydep := TPDependency(osdbasedata.aktProduct.dependencies.add);
    mydep.init;
    // Action request
    mydep.action := FNewDepDlg.ComboBoxDepActionrequest.Text;
    // required productId
    mydep.Required_ProductId := FNewDepDlg.ComboBoxproductIds.Text;
    // required State & action
    if FNewDepDlg.RadioButtonAction.Checked then
    begin
      mydep.Required_State := noState;
      case FNewDepDlg.ComboBoxActState.Text of
        '': mydep.Required_Action := noRequest;
        'setup': mydep.Required_Action := setup;
        'uninstall': mydep.Required_Action := uninstall;
        'update': mydep.Required_Action := TPActionRequest.update;
      end;
    end
    else
    begin
      case FNewDepDlg.ComboBoxActState.Text of
        '': mydep.Required_State := noState;
        'installed': mydep.Required_State := installed;
        'not installed': mydep.Required_State := not_installed;
        'unknown': mydep.Required_State := unknown;
      end;
      mydep.Required_Action := noRequest;
    end;

    // requirement Type
    case FNewDepDlg.ComboBoxReqType.Text of
      '': mydep.Required_Type := doNotMatter;
      'before': mydep.Required_Type := before;
      'after': mydep.Required_Type := after;
    end;
    TIGridDep.ListObject := osdbasedata.aktproduct.dependencies;
    TIGridDep.ReloadTIList;
    TIGridDep.Update;
  end
  else
  begin
    // cancel add
  end;
end;

procedure TResultform1.BitBtnAddPropClick(Sender: TObject);
// add property
var
  myprop: TPProperty;
  i: integer;
  tmpstrlist: TStringList;
  exists: boolean;
begin
  FNewPropDlg.initFields;
  FNewPropDlg.RadioButtonPropBool.Checked := True;
  FNewPropDlg.RadioButtonPropStringChange(Sender);
  procmess;
  if FNewPropDlg.ShowModal = mrOk then
  begin
    // add
    with osdbasedata.aktProduct do
    begin
      exists := properties.propExists(FNewPropDlg.EditPropName.Text);
      if exists then
        MessageDlg(rsPropEditErrorHead,
          rsPropEditErrorDoubleMsgStart + FNewPropDlg.EditPropName.Text +
          ' ' + rsPropEditErrorDoubleMsgFinish,
          mtError, [mbOK], '')
      else
      begin
        myprop := TPProperty(properties.add);
        myprop.init;
        myprop.Property_Name := FNewPropDlg.EditPropName.Text;
        myprop.description := FNewPropDlg.MemoDesc.Text;
        if FNewPropDlg.RadioButtonPropBool.Checked then
        begin
          myprop.Property_Type := bool;
          myprop.boolDefault := False;
          for i := 0 to FNewPropDlg.ListBoxPropDefVal.Count - 1 do
            if FNewPropDlg.ListBoxPropDefVal.Selected[i] then
              if lowercase(FNewPropDlg.ListBoxPropDefVal.Items[i]) = 'true' then
                myprop.boolDefault := False;
          tmpstrlist := TStringList.Create;
          myprop.SetDefaultLines(TStrings(tmpstrlist));
          myprop.SetValueLines(TStrings(tmpstrlist));
          FreeAndNil(tmpstrlist);
        end
        else
        begin
          myprop.Property_Type := unicode;  //type
          myprop.SetValueLines(FNewPropDlg.ListBoxPropPosVal.Items);
          tmpstrlist := TStringList.Create;
          for i := 0 to FNewPropDlg.ListBoxPropDefVal.Count - 1 do
            if FNewPropDlg.ListBoxPropDefVal.Selected[i] then
              tmpstrlist.Add(FNewPropDlg.ListBoxPropDefVal.Items[i]);
          myprop.SetDefaultLines(TStrings(tmpstrlist));
          FreeAndNil(tmpstrlist);
        end;
        myprop.multivalue := FNewPropDlg.CheckBoxPropMultiVal.Checked; //multivalue
        myprop.editable := FNewPropDlg.CheckBoxPropEdit.Checked;  //editable
      end;
    end;
    TIGridProp.ListObject := osdbasedata.aktproduct.properties;
    TIGridProp.ReloadTIList;
    TIGridProp.Update;
  end
  else
  begin
    // cancel add
  end;
end;

procedure TResultform1.BitBtnChooseInstDir1Click(Sender: TObject);
begin
  chooseInstallDir(aktProduct.SetupFiles[0]);
end;

procedure TResultform1.BitBtnChooseInstDir2Click(Sender: TObject);
begin
  chooseInstallDir(aktProduct.SetupFiles[1]);
end;

procedure TResultform1.BitBtnChooseInstDir3Click(Sender: TObject);
begin
  chooseInstallDir(aktProduct.SetupFiles[2]);
end;

procedure TResultform1.BitBtnChooseTargetProg1Click(Sender: TObject);
begin
  chooseTargetProgram(aktProduct.SetupFiles[0]);
end;

procedure TResultform1.BitBtnChooseTargetProg2Click(Sender: TObject);
begin
  chooseTargetProgram(aktProduct.SetupFiles[1]);
end;

procedure TResultform1.BitBtnChooseTargetProg3Click(Sender: TObject);
begin
  chooseTargetProgram(aktProduct.SetupFiles[2]);
end;

procedure TResultform1.BitBtnChooseUninstFile1Click(Sender: TObject);
begin
  chooseUninstaller(aktProduct.SetupFiles[0]);
end;

procedure TResultform1.BitBtnChooseUninstFile2Click(Sender: TObject);
begin
  chooseUninstaller(aktProduct.SetupFiles[1]);
end;

procedure TResultform1.BitBtnChooseUninstFile3Click(Sender: TObject);
begin
  chooseUninstaller(aktProduct.SetupFiles[2]);
end;

procedure TResultform1.BitBtnDelDepClick(Sender: TObject);
var
  index, Count: integer;
begin
  // delete dependency
  if TIGridDep.SelectedRangeCount > 0 then
  begin
    index := TIGridDep.SelectedRange[0].Top;
    Count := aktProduct.dependencies.Count;
    // deleting the first entry leads to an access violation
    // to avoid this we move the item to delete to the end of the collection
    aktProduct.dependencies.Exchange(index - 1, Count - 1);
    // then we have to sync with the grid
    TIGridDep.ReloadTIList;
    // now we delete the last element
    aktProduct.dependencies.Delete(Count - 1);
    // and now we can resync without access violation
    TIGridDep.ReloadTIList;
    TIGridDep.Update;
  end
  else
    // nothing yet
  ;
end;

procedure TResultform1.BitBtnDelPropClick(Sender: TObject);
var
  index, Count: integer;
begin
  // delete property
  if TIGridProp.SelectedRangeCount > 0 then
  begin
    index := TIGridProp.SelectedRange[0].Top;
    Count := aktProduct.properties.Count;
    // deleting the first entry leads to an access violation
    // to avoid this we move the item to delete to the end of the collection
    aktProduct.properties.Exchange(index - 1, Count - 1);
    // then we have to sync with the grid
    TIGridProp.ReloadTIList;
    // now we delete the last element
    aktProduct.properties.Delete(Count - 1);
    index := aktProduct.properties.Count;
    // and now we can resync without access violation
    TIGridProp.ReloadTIList;
    TIGridProp.Update;
  end
  else
    // nothing yet
  ;
end;

procedure TResultform1.BitBtnEditDepClick(Sender: TObject);
var
  //mydep: TStringList;
  y: integer;
  mydep: TPDependency;
  tmpstr: string;

  //aPoint: TPoint;
begin
  if TIGridDep.SelectedRangeCount > 0 then
  begin
    y := TIGridDep.SelectedRange[0].Top - 1;
    if y > -1 then
    begin
      mydep := TPDependency(aktProduct.dependencies.Items[y]);
      FNewDepDlg.ComboBoxproductIds.Text := mydep.Required_ProductId;
      if mydep.Required_State = noState then
      begin
        FNewDepDlg.RadioButtonState.Checked := False;
        FNewDepDlg.RadioButtonAction.Checked := True;
        tmpstr := GetEnumName(TypeInfo(TPActionRequest), Ord(mydep.Required_Action));
        if tmpstr = 'noRequest' then
          tmpstr := '';
        FNewDepDlg.ComboBoxActState.Text := tmpstr;
      end
      else
      begin
        FNewDepDlg.RadioButtonState.Checked := True;
        FNewDepDlg.RadioButtonAction.Checked := False;
        tmpstr := GetEnumName(TypeInfo(TPInstallationState), Ord(mydep.Required_State));
        if tmpstr = 'noState' then
          tmpstr := '';
        FNewDepDlg.ComboBoxActState.Text := tmpstr;
      end;
      FNewDepDlg.RadioButtonActionChange(Sender);
      procmess;

      tmpstr := GetEnumName(TypeInfo(TPDtype), Ord(mydep.Required_Type));
      if tmpstr = 'doNotMatter' then
        tmpstr := '';
      FNewDepDlg.ComboBoxReqType.Text := tmpstr;
      FNewDepDlg.ComboBoxReqType.Refresh;
      procmess;
      if FNewDepDlg.ShowModal = mrOk then
      begin
        // required productId
        mydep.Required_ProductId := FNewDepDlg.ComboBoxproductIds.Text;
        // required State & action
        if FNewDepDlg.RadioButtonAction.Checked then
        begin
          mydep.Required_State := noState;
          //mydep.Add(FNewDepDlg.ComboBoxActState.Text);
          case FNewDepDlg.ComboBoxActState.Text of
            '': mydep.Required_Action := noRequest;
            'setup': mydep.Required_Action := setup;
            'uninstall': mydep.Required_Action := uninstall;
            'update': mydep.Required_Action := TPActionRequest.update;
            'once': mydep.Required_Action := once;
          end;
        end
        else
        begin
          //mydep.Add(FNewDepDlg.ComboBoxActState.Text);
          case FNewDepDlg.ComboBoxActState.Text of
            '': mydep.Required_State := noState;
            'none': mydep.Required_State := noState;
            'installed': mydep.Required_State := installed;
            'not installed': mydep.Required_State := not_installed;
            'unknown': mydep.Required_State := unknown;
          end;
          mydep.Required_Action := noRequest;
        end;

        // requirement Type
        case FNewDepDlg.ComboBoxReqType.Text of
          '': mydep.Required_Type := doNotMatter;
          'before': mydep.Required_Type := before;
          'after': mydep.Required_Type := after;
        end;
        TIGridDep.ListObject := osdbasedata.aktproduct.dependencies;
        TIGridDep.ReloadTIList;
        TIGridDep.Update;
      end
      else
      begin
        // cancel add
      end;
    end;
  end
  else
  begin
    MessageDlg('opsi-setup-detector: Dependency Editor: Error',
      rsDependencyEditErrorNoSelect,
      mtError, [mbOK], '');
  end;

end;

procedure TResultform1.BitBtnEditPropClick(Sender: TObject);
// edit property
var
  i, k: integer;
  tmpstr: string;
  y: integer;
  myprop: TPProperty;
  tmpstrlist: TStringList;
begin
  if TIGridProp.SelectedRangeCount > 0 then
  begin
    y := TIGridProp.SelectedRange[0].Top - 1;
    if y > -1 then
    begin
      myprop := TPProperty(aktProduct.properties.Items[y]);

      FNewPropDlg.ListBoxPropPosVal.Clear;
      FNewPropDlg.ListBoxPropDefVal.Clear;
      FNewPropDlg.EditPropName.Text := myprop.Property_Name;
      FNewPropDlg.MemoDesc.Text := myprop.description;
      FNewPropDlg.CheckBoxPropMultiVal.Checked := myprop.multivalue;
      FNewPropDlg.CheckBoxPropEdit.Checked := myprop.editable;
      FNewPropDlg.CheckBoxPropMultiValChange(Sender);
      procmess;
      if myprop.Property_Type = bool then
      begin
        FNewPropDlg.RadioButtonPropString.Checked := False;
        FNewPropDlg.RadioButtonPropBool.Checked := True;
        FNewPropDlg.RadioButtonPropStringChange(Sender);
        procmess;
        if myprop.boolDefault = True then
        begin
          FNewPropDlg.ListBoxPropDefVal.Selected[1] := False;
          FNewPropDlg.ListBoxPropDefVal.Selected[0] := True;
        end
        else
        begin
          FNewPropDlg.ListBoxPropDefVal.Selected[0] := False;
          FNewPropDlg.ListBoxPropDefVal.Selected[1] := True;
        end;
        procmess;
      end
      else  // unicode
      begin
        FNewPropDlg.RadioButtonPropString.Checked := True;
        FNewPropDlg.RadioButtonPropBool.Checked := False;
        FNewPropDlg.RadioButtonPropStringChange(Sender);
        procmess;
        FNewPropDlg.ListBoxPropPosVal.Items.SetStrings(myprop.GetValueLines);
        // add possible value here - default is the selcted one
        FNewPropDlg.ListBoxPropDefVal.Items.SetStrings(myprop.GetValueLines);
        for i := 0 to myprop.GetDefaultLines.Count - 1 do
        begin
          k := FNewPropDlg.ListBoxPropDefVal.Items.IndexOf(
            myprop.GetDefaultLines.Strings[i]);
          if k > -1 then
            FNewPropDlg.ListBoxPropDefVal.Selected[k] := True;
        end;
      end;


      if FNewPropDlg.ShowModal = mrOk then
      begin
        // modify
        myprop.Property_Name := FNewPropDlg.EditPropName.Text;
        myprop.description := FNewPropDlg.MemoDesc.Text;
        if FNewPropDlg.RadioButtonPropBool.Checked then
        begin
          myprop.Property_Type := bool;
          myprop.boolDefault := False;
          for i := 0 to FNewPropDlg.ListBoxPropDefVal.Count - 1 do
            if FNewPropDlg.ListBoxPropDefVal.Selected[i] then
            begin
              tmpstr := lowercase(FNewPropDlg.ListBoxPropDefVal.Items[i]);
              if tmpstr = 'true' then
                myprop.boolDefault := True;
            end;
          tmpstrlist := TStringList.Create;
          myprop.SetDefaultLines(TStrings(tmpstrlist));
          myprop.SetValueLines(TStrings(tmpstrlist));
          FreeAndNil(tmpstrlist);
        end
        else
        begin
          myprop.Property_Type := unicode;  //type
          myprop.SetValueLines(FNewPropDlg.ListBoxPropPosVal.Items);
          tmpstrlist := TStringList.Create;
          for i := 0 to FNewPropDlg.ListBoxPropDefVal.Count - 1 do
            if FNewPropDlg.ListBoxPropDefVal.Selected[i] then
              tmpstrlist.Add(FNewPropDlg.ListBoxPropDefVal.Items[i]);
          myprop.SetDefaultLines(TStrings(tmpstrlist));
          FreeAndNil(tmpstrlist);
        end;
        myprop.multivalue := FNewPropDlg.CheckBoxPropMultiVal.Checked; //multivalue
        myprop.editable := FNewPropDlg.CheckBoxPropEdit.Checked;  //editable
        TIGridProp.ListObject := osdbasedata.aktproduct.properties;
        TIGridProp.ReloadTIList;
        TIGridProp.Update;
      end
      else
      begin
        // cancel add
      end;
    end
    else
    begin
      MessageDlg(rsPropEditErrorHead,
        rsPropEditErrorNoSelect,
        mtError, [mbOK], '');
    end;

  end
  else
  begin
    MessageDlg(rsPropEditErrorHead,
      rsPropEditErrorNoSelect,
      mtError, [mbOK], '');
  end;
end;

procedure TResultform1.BitBtnImportControlClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 9;   // control file
  if DirectoryExists(myconfiguration.LastProjectFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastProjectFileDir
  else if DirectoryExists(myconfiguration.workbench_Path) then
    OpenDialog1.InitialDir := myconfiguration.workbench_Path;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastProjectFileDir := ExtractFileDir(OpenDialog1.FileName);
    LogDatei.log('Start import control file to existing data from: ' +
      OpenDialog1.FileName, LLnotice);
    // do not init product data structure here
    // we just want to add the data to the existing data structure
    //initaktproduct;
    resultform1.updateGUI;
    // we do not want to import everything : filter = true
    aktProduct.readControlFile(OpenDialog1.FileName, True);
    TIGridDep.ListObject := osdbasedata.aktproduct.dependencies;
    TIGridDep.ReloadTIList;
    TIGridDep.Update;
    TIGridProp.ListObject := osdbasedata.aktproduct.properties;
    TIGridProp.ReloadTIList;
    TIGridProp.Update;
    LogDatei.log('Finished import control file to existing data from: ' +
      OpenDialog1.FileName, LLnotice);
  end;
end;

procedure TResultform1.BitBtnOpenFile1Click(Sender: TObject);
begin

end;

procedure TResultform1.BitBtnOpenFileClick(Sender: TObject);
begin

end;

procedure TResultform1.OpenMSTFile(var mysetup: TSetupFile);
var
  str: string;
begin
  OpenDialog1.FilterIndex := 4;
  if DirectoryExists(myconfiguration.LastSetupFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastSetupFileDir;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastSetupFileDir := ExtractFileDir(OpenDialog1.FileName);
    mysetup.mstFullFileName := OpenDialog1.FileName;
    str := ' TRANSFORMS="$installerSourceDir$\' + mysetup.mstFileName + '"';
    mysetup.installCommandLine := mysetup.installCommandLine + str;
  end;
end;

{$IFDEF WINDOWS}
// from https://wiki.freepascal.org/Detect_Windows_x32-x64_example
// modified to work with auto format (Ctrl-D) (d.oertel 05.09.2022)
//function IsWindows64: boolean;
  {
  Detect if we are running on 64 bit Windows or 32 bit Windows,
  independently of bitness of this program.
  Original source:
  http://www.delphipraxis.net/118485-ermitteln-ob-32-bit-oder-64-bit-betriebssystem.html
  modified for FreePascal in German Lazarus forum:
  http://www.lazarusforum.de/viewtopic.php?f=55&t=5287
  }
{$ifdef WIN32}//Modified KpjComp for 64bit compile mode
function IsWindows64: boolean;
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
      Handle: Windows.THandle; var Res: Windows.BOOL): Windows.BOOL; stdcall;
var
  IsWow64Result: Windows.BOOL; // Result from IsWow64Process
  IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := TIsWow64Process(Windows.GetProcAddress(
    Windows.GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(Windows.GetCurrentProcess, IsWow64Result) then
      raise SysUtils.Exception.Create('IsWindows64: bad process handle');
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
end;

{$else}//if were running 64bit code, OS must be 64bit :)
function IsWindows64: boolean;
begin
  Result := True;
end;

{$endif}
{$ENDIF WINDOWS}

procedure TResultform1.chooseInstallDir(var mysetup: TSetupFile);
var
  installdir: string;
begin
  {$IFDEF WINDOWS}
  SelectDirectoryDialog1.InitialDir := 'c:\';
  {$ELSE WINDOWS}
  SelectDirectoryDialog1.InitialDir := '/';
  {$ENDIF WINDOWS}
  if SelectDirectoryDialog1.Execute then
  begin
    installdir := SelectDirectoryDialog1.FileName;
    {$IFDEF WINDOWS}
    // use constants if possible
    installdir := ReplaceText(installdir, 'c:\program files (x86)',
      '%ProgramFiles32Dir%');
    if IsWindows64 then
      installdir := ReplaceText(installdir, 'c:\program files', '%ProgramFiles64Dir%')
    else
      installdir := ReplaceText(installdir, 'c:\program files', '%ProgramFiles32Dir%');
    {$ENDIF WINDOWS}
    mysetup.installDirectory := installdir;
  end;
end;

procedure TResultform1.updateUninstaller(var mysetup: TSetupFile);
var
  uninstcheckstr: string;
begin
  // update uninstall program relevant data
  if mysetup.uninstallProg <> '' then
  begin

    mysetup.uninstallCheck.Clear;
    uninstcheckstr := mysetup.uninstallProg;
    // the use of the  $installdir$ variable for the primary section function fileexists
    // will for example result to:
    // if fileexists(""+$installdir$+"\uninst.exe")
    uninstcheckstr := StringReplace(uninstcheckstr, '$installdir$',
      '"+$installdir$+"', [rfIgnoreCase]);

    // the use of the  $installerSourceDir$ variable for the primary section function fileexists
    // will for example result to:
    // if fileexists($installerSourceDir$+"\uninst.exe")
    uninstcheckstr := StringReplace(uninstcheckstr, '$installerSourceDir$',
      '"+$installerSourceDir$+"', [rfIgnoreCase]);


    mysetup.uninstallCheck.Add('if fileexists("' + uninstcheckstr + '")');
    mysetup.uninstallCheck.Add('	set $oldProgFound$ = "true"');
    mysetup.uninstallCheck.Add('endif');
    if mysetup.preferSilent then
      mysetup.uninstallCommandLine :=
        '"' + mysetup.uninstallProg + '" ' +
        installerArray[integer(mysetup.installerId)].silentuninstall
    else
      mysetup.uninstallCommandLine :=
        '"' + mysetup.uninstallProg + '" ' +
        installerArray[integer(mysetup.installerId)].unattendeduninstall;
  end
  else
  begin
    // no known uninstall program
    mysetup.uninstallCheck.Add('set $oldProgFound$ = "false"');
  end;
end;

procedure TResultform1.chooseUninstaller(var mysetup: TSetupFile);
var
  uninstfile, uninstdir: string;
begin
  OpenDialog1.FilterIndex := 1;
  if DirectoryExists('C:\Program Files') then
    OpenDialog1.InitialDir := 'C:\Program Files';
  if OpenDialog1.Execute then
  begin
    uninstfile := ExtractFileName(OpenDialog1.FileName);
    uninstdir := ExtractFileDir(OpenDialog1.FileName);
    {$IFDEF WINDOWS}
    // use constants if possible
    uninstdir := ReplaceText(uninstdir, 'c:\program files (x86)',
      '%ProgramFiles32Dir%');
    if IsWindows64 then
      uninstdir := ReplaceText(uninstdir, 'c:\program files', '%ProgramFiles64Dir%')
    else
      uninstdir := ReplaceText(uninstdir, 'c:\program files', '%ProgramFiles32Dir%');
    {$ENDIF WINDOWS}
    // use $installdir$ if possible
    uninstdir := ReplaceText(uninstdir, mysetup.installDirectory, '$installdir$');
    mysetup.uninstallProg := uninstdir + '\' + uninstfile;
  end;
  updateUninstaller(mysetup);
end;

procedure TResultform1.chooseTargetProgram(var mysetup: TSetupFile);
begin
  OpenDialog1.FilterIndex := 1;
  if DirectoryExists('C:\Program Files') then
    OpenDialog1.InitialDir := 'C:\Program Files';
  if OpenDialog1.Execute then
  begin
    mysetup.targetProg := ExtractFileName(OpenDialog1.FileName);
  end;
end;


procedure TResultform1.SetTICheckBoxesMST(Installer: TKnownInstaller);
begin
  case Installer of
    stMsi:
    begin
      TICheckBoxS1MSt.Enabled := True;
      TICheckBoxS2MSt.Enabled := True;
    end
    else
    begin
      TICheckBoxS1MSt.Enabled := False;
      TICheckBoxS2MSt.Enabled := False;
    end;
  end;
end;

procedure TResultform1.BitBtnOpenMst1Click(Sender: TObject);
begin
  OpenMSTFile(aktProduct.SetupFiles[0]);
end;

procedure TResultform1.BitBtnOpenMst2Click(Sender: TObject);
begin
  OpenMSTFile(aktProduct.SetupFiles[1]);
end;

procedure TResultform1.BitBtnRebuildClick(Sender: TObject);
var
  buildSuccess: boolean = False;
begin
  // reset exitcode before (repeated) build:
  system.ExitCode := 0;
  logdatei.log('Start BtRebuildClick', LLDebug2);
  OSD_info.Height := 150 * round(screen.PixelsPerInch / DesignTimePPI);
  OSD_info.Width := 400 * round(screen.PixelsPerInch / DesignTimePPI);
  if not DirectoryExists(myconfiguration.workbench_Path) then
  begin
    //ShowMessage(sErrPacketBaseDirNotFound);
    OSD_info.mdContent := '<span style="color:red"> **Failed**  ' +
      sErrPacketBaseDirNotFound + '</span>';
    OSD_info.ShowModal;
  end;
  try
    PanelProcess.Visible := True;
    procmess;
    buildSuccess := callServiceOrPackageBuilder;
    procmess;
    PanelProcess.Visible := False;
    if (system.ExitCode = 0) and buildSuccess then
      //ShowMessage(rsRebuildFinished)
      OSD_info.mdContent := '<span style="color:green"> **OK**  ' +
        rsRebuildFinished + '</span>'
    else
      OSD_info.mdContent := '<span style="color:red"> **Failed**  ' +
        rsRebuildFailedBuild + '</span>';
    //ShowMessage(rsRebuildFailedBuild);

    OSD_info.ShowModal;
  finally
    PanelProcess.Visible := False;
    OSD_info.FormSizeReset;
    procmess;
  end;
  logdatei.log('Finished BtRebuildClick', LLDebug2);
end;


procedure TResultform1.BtCreateEmptyTemplateWinClick(Sender: TObject);
begin
  resetGUI;
  osdsettings.runmode := createTemplate;
  setRunMode;
  MemoAnalyze.Clear;
  PageControl1.ActivePage := resultForm1.TabSheetProduct;
  Application.ProcessMessages;
  initaktproduct;
  makeProperties;
  resultform1.updateGUI;
  aktProduct.productdata.targetOSset := [osWin];
  aktProduct.productdata.productId := 'opsi-template';
  aktProduct.productdata.productName := 'opsi template for Windows';
  aktProduct.productdata.productversion := '1.0.0';
  aktProduct.productdata.packageversion := 1;
  aktProduct.productdata.description := 'A template for opsi products for Windows';
end;

procedure TResultform1.BtCreateEmptyTemplateWithUserWinClick(Sender: TObject);
begin
  resetGUI;
  osdsettings.runmode := createTemplateWithUser;
  setRunMode;
  MemoAnalyze.Clear;
  PageControl1.ActivePage := resultForm1.TabSheetProduct;
  Application.ProcessMessages;
  initaktproduct;
  makeProperties;
  resultform1.updateGUI;
  aktProduct.productdata.targetOSset := [osWin];
  aktProduct.productdata.productId := 'opsi-template-with-user';
  aktProduct.productdata.productName := 'opsi template for Windows - Install with user';
  aktProduct.productdata.productversion := '1.0.0';
  aktProduct.productdata.packageversion := 1;
  aktProduct.productdata.description :=
    'A template for opsi products for Windows - Install with user';
  aktProduct.productdata.setupscript := 'setup.opsiscript';
  aktProduct.productdata.uninstallscript := 'localsetup\uninstall-local.opsiscript';
  aktProduct.productdata.updatescript := 'localsetup\update-local.opsiscript';
  aktProduct.productdata.priority := -20;
  aktProduct.productdata.advice := rsCreateWithUserProductAdvice;
  resultform1.updateGUI;
  Application.ProcessMessages;
end;


procedure TResultform1.BtCreateEmptyTemplateMacClick(Sender: TObject);
begin
  begin
    resetGUI;
    osdsettings.runmode := createTemplate;
    setRunMode;
    MemoAnalyze.Clear;
    PageControl1.ActivePage := resultForm1.TabSheetProduct;
    Application.ProcessMessages;
    initaktproduct;
    makeProperties;
    resultform1.updateGUI;
    aktProduct.productdata.targetOSset := [osMac];
    aktProduct.productdata.productId := 'm-opsi-template';
    aktProduct.productdata.productName := 'opsi template for macos';
    aktProduct.productdata.productversion := '1.0.0';
    aktProduct.productdata.packageversion := 1;
    aktProduct.productdata.description := 'A template for opsi products for MacOS';
  end;
end;

procedure TResultform1.BtCreateMetaClick(Sender: TObject);
begin
  resetGUI;
  osdsettings.runmode := createMeta;
  setRunMode;
  MemoAnalyze.Clear;
  PageControl1.ActivePage := resultForm1.TabSheetProduct;
  Application.ProcessMessages;
  initaktproduct;
  // no properties at meta product
  //makeProperties;
  resultform1.updateGUI;
  aktProduct.productdata.targetOSset := [osMulti];
  aktProduct.productdata.productId := 'opsi-meta-template';
  aktProduct.productdata.productName := 'opsi template for a Meta Product';
  aktProduct.productdata.productversion := '1.0.0';
  aktProduct.productdata.packageversion := 1;
  aktProduct.productdata.description :=
    'A opsi product that contains dependencies but installs nothing';
end;

procedure TResultform1.BtCreateEmptyTemplateLinClick(Sender: TObject);
begin
  resetGUI;
  osdsettings.runmode := createTemplate;
  setRunMode;
  MemoAnalyze.Clear;
  PageControl1.ActivePage := resultForm1.TabSheetProduct;
  Application.ProcessMessages;
  initaktproduct;
  makeProperties;
  resultform1.updateGUI;
  aktProduct.productdata.targetOSset := [osLin];
  aktProduct.productdata.productId := 'l-opsi-template';
  aktProduct.productdata.productName := 'opsi template for Linux';
  aktProduct.productdata.productversion := '1.0.0';
  aktProduct.productdata.packageversion := 1;
  aktProduct.productdata.description := 'A template for opsi products for Linux';
end;


procedure TResultform1.BtCreateProductClick(Sender: TObject);
var
  radioindex: integer;
  //checkok: boolean = True;
  done: boolean = False;
begin
  // reset exitcode before (repeated) build:
  system.ExitCode := 0;
  logdatei.log('Start BtCreateProductClick', LLDebug2);
  OSD_info.Height := 150 * round(screen.PixelsPerInch / DesignTimePPI);
  OSD_info.Width := 400 * round(screen.PixelsPerInch / DesignTimePPI);
  if not DirectoryExists(myconfiguration.workbench_Path) then
  begin
    //checkok := False;
    //ShowMessage(sErrPacketBaseDirNotFound);
    OSD_info.mdContent := '<span style="color:red"> **Failed**  ' +
      sErrPacketBaseDirNotFound + '</span>';
    OSD_info.ShowModal;
  end;
  try
    PanelProcess.Visible := True;
    procmess;
    done := createProductStructure;
    procmess;
    case TIRadioGroupCreateMode.ItemIndex of
      0: begin
        logdatei.log('Create Mode is files only - so we finished', LLnotice);
      end; // do nothing else
      1: begin
        logdatei.log('Create Mode is files and build - so we try to call the service', LLnotice);
        callServiceOrPackageBuilder;
      end;
      2: begin
        logdatei.log('Create Mode is files and interactive packageBuilder - so we try to call the opsiPackageBuilder', LLnotice);
        callOpsiPackageBuilder;

      end;
    end;
    (*
    if RadioButtonCreateOnly.Checked then
    ; // do nothing else
    if RadioButtonBuildPackage.Checked then
      callServiceOrPackageBuilder;
    if RadioButtonPackageBuilder.Checked then
      callOpsiPackageBuilder;
    *)
    procmess;
    PanelProcess.Visible := False;
    if done and (system.ExitCode = 0) then
      OSD_info.mdContent := '<span style="color:green"> **OK**  ' + sInfoFinished + '</span>'
    //ShowMessage(sInfoFinished)
    else if done and (system.ExitCode = 1) then
      //ShowMessage(sInfoFailedBuild)
      OSD_info.mdContent := '<span style="color:red"> **Failed**  ' +
        sInfoFailedBuild + '</span>'
    else
      OSD_info.mdContent := '<span style="color:red"> **Failed**  ' +
        sInfoFailedCreate + '</span>';
    //ShowMessage(sInfoFailedCreate);
    OSD_info.ShowModal;
  finally
    PanelProcess.Visible := False;
    OSD_info.FormSizeReset;
    procmess;
  end;
  // we do not want to sav the actual readio button selection in the configuration
  //myconfiguration.CreateRadioIndex := TIRadioGroupCreateMode.ItemIndex;
  //myconfiguration.BuildRadioIndex := TIRadioGroupBuildMode.ItemIndex;
  (*
  if RadioButtonCreateOnly.Checked then
    radioindex := 0;
  if RadioButtonBuildPackage.Checked then
    radioindex := 1;
  if RadioButtonPackageBuilder.Checked then
    radioindex := 2;
  myconfiguration.CreateRadioIndex := radioindex;
  if radioBuildModebuildOnly.Checked then
    radioindex := 0;
  if radioBuildModebuildInstall.Checked then
    radioindex := 1;
  myconfiguration.BuildRadioIndex := radioindex;
  *)
  logdatei.log('Finished BtCreateProductClick', LLDebug2);
end;

procedure TResultform1.BtCreateWingetPackageClick(Sender: TObject);
var
  tmpstr : string;
begin
  resetGUI;
  osdsettings.runmode := createWingetProd;
  setRunMode;
  MemoAnalyze.Clear;
  PageControl1.ActivePage := resultForm1.TabSheetWinget;
  Application.ProcessMessages;
  initaktproduct;
  makeProperties;
  resultform1.updateGUI;
  aktProduct.SetupFiles[0].active := True;
  aktProduct.SetupFiles[0].installerId := stWinget;
  aktProduct.productdata.targetOSset := [osWin];
  aktProduct.productdata.productId := '';
  aktProduct.productdata.productName := '';
  aktProduct.productdata.productversion := '1.0.0';
  aktProduct.productdata.packageversion := 1;
  aktProduct.productdata.description := 'winget';
  {$IFDEF OSDGUI}
  TreeView1.Items[1].Enabled:= false;  // analyze
  TreeView1.Items[2].Enabled:= false;  // setup1
  TreeView1.Items[3].Enabled:= false;  // setup2
  TreeView1.Items[4].Enabled:= false;  // setup3
  TreeView1.Items[5].Enabled:= true;  // winget
  tmpstr := installerArray[integer(stWinget)].info_message_html.Text;
  if tmpstr <> '' then
  begin
    OSD_info.mdContent := tmpstr;
    if osdsettings.showgui then
      OSD_info.ShowModal;
  end;
  {$ENDIF OSDGUI}
end;
procedure TResultform1.BtnIconsNextStepClick(Sender: TObject);
begin
  case osdsettings.runmode of
    analyzeOnly:
    begin
      //we should never be here
      logdatei.log(
        'Error: in BtProductNextStepClick RunMode: analyzeOnly', LLError);
    end;
    createTemplate,
    createTemplateWithUser,
    createMultiTemplate,
    singleAnalyzeCreate,
    analyzeCreateWithUser,
    twoAnalyzeCreate_1,
    twoAnalyzeCreate_2,
    threeAnalyzeCreate_1,
    threeAnalyzeCreate_2,
    threeAnalyzeCreate_3,
    createWingetProd:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    createMeta, gmUnknown:
    begin
      // we should never be here
      logdatei.log(
        'Error: in BtProductNextStepClick RunMode: ' + GetEnumName(
        TypeInfo(TRunMode), Ord(osdsettings.runmode)), LLError);
    end;
  end;
end;

procedure TResultform1.BtProduct1NextStepClick(Sender: TObject);
var
  checkok: boolean = True;
begin
  if (aktProduct.productdata.productId = '') then
  begin
    checkok := False;
    ShowMessage(sErrProductIdEmpty);
  end;
  if (aktProduct.productdata.productversion = '') then
  begin
    checkok := False;
    ShowMessage(sErrProductVersionEmpty);
  end;
  if checkok then
  begin
    case osdsettings.runmode of
      analyzeOnly:
      begin
        //we should never be here
        logdatei.log(
          'Error: in BtProductNextStepClick RunMode: analyzeOnly', LLError);
      end;
      analyzeCreateWithUser,
      createMeta,
      createTemplate,
      createTemplateWithUser,
      createMultiTemplate,
      singleAnalyzeCreate,
      twoAnalyzeCreate_1,
      twoAnalyzeCreate_2,
      threeAnalyzeCreate_1,
      threeAnalyzeCreate_2,
      threeAnalyzeCreate_3,
      createWingetProd:
      begin
        updateGUI;
        PageControl1.ActivePage := resultForm1.TabSheetProduct2;
        Application.ProcessMessages;
      end;
      gmUnknown:
      begin
        // we should never be here
        logdatei.log('Error: in BtProductNextStepClick RunMode: ' +
          GetEnumName(TypeInfo(TRunMode), Ord(osdsettings.runmode)),
          LLError);
      end;
    end;
  end;
end;

procedure TResultform1.BtProduct2NextStepClick(Sender: TObject);
begin
  case osdsettings.runmode of
    analyzeOnly, gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtProductNextStepClick RunMode: ' +
        GetEnumName(TypeInfo(TRunMode), Ord(osdsettings.runmode)), LLError);
    end;
    analyzeCreateWithUser,
    createTemplate,
    createTemplateWithUser,
    createMultiTemplate,
    singleAnalyzeCreate,
    twoAnalyzeCreate_1,
    twoAnalyzeCreate_2,
    threeAnalyzeCreate_1,
    threeAnalyzeCreate_2,
    threeAnalyzeCreate_3,
    createWingetProd:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetIcons;
      Application.ProcessMessages;
    end;
    // no icons for meta product
    createMeta:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
  end;
end;

procedure TResultform1.BtSetup1NextStepClick(Sender: TObject);
var
  checkok: boolean = True;
  localTOSset: TTargetOSset;
  installerselected: boolean = False;
begin
  if ((aktProduct.SetupFiles[0].installDirectory = '') or
    (aktProduct.SetupFiles[0].installDirectory = 'unknown')) and
    (aktProduct.SetupFiles[0].installerId <> stMsi) and
    (aktProduct.SetupFiles[0].targetOS = osWin) then
  begin
    //checkok := False;
    // we warn here only
    ShowMessage(sWarnInstalldirUnknown);
  end;
  if checkok then
  begin
    case osdsettings.runmode of
      analyzeOnly:
      begin
        Application.Terminate;
      end;
      singleAnalyzeCreate, analyzeCreateWithUser:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct;
        Application.ProcessMessages;
      end;
      twoAnalyzeCreate_1:
      begin
        osdsettings.runmode := twoAnalyzeCreate_2;
        MessageDlg(rsTwonalyzeAndCreateMsgHead,
          rsTwonalyzeAndCreateMsgSecondSetup,
          mtInformation, [mbOK], '');
        OpenDialog1.FilterIndex := 1;   // setup
        if OpenDialog1.Execute then
        begin
          aktProduct.SetupFiles[1].copyCompleteDir := showCompleteDirDlg;
          PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
          MemoAnalyze.Clear;
          Application.ProcessMessages;
          aktProduct.SetupFiles[1].active := True;
          Analyze(OpenDialog1.FileName,
            aktProduct.SetupFiles[1], True);
          SetTICheckBoxesMST(aktProduct.SetupFiles[1].installerId);
        end;
      end;
      threeAnalyzeCreate_1:
      begin
        osdsettings.runmode := threeAnalyzeCreate_2;
        //aktProduct.targetOS := osLin;
        localTOSset := aktProduct.productdata.targetOSset;
        Include(localTOSset, osLin);
        if MessageDlg(rsThreeAnalyzeAndCreateMsgHead,
          rsThreeAnalyzeAndCreateMsgSecondSetup, mtConfirmation,
          [mbYes, mbNo], '') = mrYes then
        begin
          OpenDialog1.FilterIndex := 6;   // linux
          if OpenDialog1.Execute then
          begin
            aktProduct.SetupFiles[1].copyCompleteDir := showCompleteDirDlg;
            PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
            MemoAnalyze.Clear;
            Application.ProcessMessages;
            aktProduct.SetupFiles[1].active := True;
            aktProduct.productdata.targetOSset := localTOSset;
            //aktProduct.SetupFiles[1] := osLin;
            aktProduct.SetupFiles[1].targetOS := osLin;
            AnalyzeLin(OpenDialog1.FileName,
              aktProduct.SetupFiles[1], True);
            SetTICheckBoxesMST(aktProduct.SetupFiles[1].installerId);
            installerselected := True;
          end;
        end;
        if not installerselected then
          BtSetup2NextStepClick(Sender);
      end;
      createTemplate, createTemplateWithUser, createMeta, gmUnknown:
      begin
        // we should never be here
        logdatei.log(
          'Error: in BtSetup1NextStepClick RunMode: ' +
          GetEnumName(TypeInfo(TRunMode), Ord(osdsettings.runmode)), LLError);
      end;
    end;
  end;
end;

procedure TResultform1.BtSetup2NextStepClick(Sender: TObject);
var
  checkok, isapp, goon: boolean;
  filename: string;
  localTOSset: TTargetOSset;
begin
  checkok := True;
  if ((aktProduct.SetupFiles[1].installDirectory = '') or
    (aktProduct.SetupFiles[1].installDirectory = 'unknown')) and
    (aktProduct.SetupFiles[1].installerId <> stMsi) and
    (aktProduct.SetupFiles[1].targetOS = osWin) then
  begin
    // checkok := False;
    // we warn here only
    ShowMessage(sWarnInstalldirUnknown);
  end;
  if checkok then
  begin
    case osdsettings.runmode of
      analyzeOnly, singleAnalyzeCreate, analyzeCreateWithUser,
      createTemplate,
      createTemplateWithUser,
      createMeta, gmUnknown:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode: ' +
          GetEnumName(TypeInfo(TRunMode), Ord(osdsettings.runmode)), LLError);
      end;
      twoAnalyzeCreate_2:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct;
        Application.ProcessMessages;
      end;
      threeAnalyzeCreate_2:
      begin
        osdsettings.runmode := threeAnalyzeCreate_3;
        //aktProduct.targetOS := osMac;
        localTOSset := aktProduct.productdata.targetOSset;
        Include(localTOSset, osMac);
        aktProduct.productdata.targetOSset := localTOSset;
        if MessageDlg(rsThreeAnalyzeAndCreateMsgHead,
          rsThreeAnalyzeAndCreateMsgThirdSetup, mtConfirmation,
          [mbYes, mbNo], '') = mrYes then
        begin
          goon := False;
          isapp := False;
          MessageDlg(rsThreeAnalyzeAndCreateMsgHead,
            rsSelectAppOrDir, mtInformation,
            [mbOK], '');
          SelectDirectoryDialog1.Title := rsSelectAppOrDir;

          if SelectDirectoryDialog1.Execute then
          begin
            goon := True;
            filename := SelectDirectoryDialog1.FileName;
            if ExtractFileExt(filename) = '.app' then
            begin
              isapp := True;
            end;
          end;
          if goon and not isapp then
          begin
            MessageDlg(rsThreeAnalyzeAndCreateMsgHead,
              rsSelectMacFile, mtInformation,
              [mbOK], '');
            OpenDialog1.InitialDir := filename;
            ;
            OpenDialog1.FilterIndex := 5;   // macos
            if OpenDialog1.Execute then
            begin
              filename := OpenDialog1.FileName;
              goon := True;
            end
            else
              goon := False;
          end;
          if goon then
          begin
            if not isapp then
              // no showCompleteDirDlg for macos
              aktProduct.SetupFiles[2].copyCompleteDir := False;
            PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
            MemoAnalyze.Clear;
            Application.ProcessMessages;
            aktProduct.SetupFiles[2].targetOS := osMac;
            aktProduct.SetupFiles[2].active := True;
            AnalyzeMac(filename,
              aktProduct.SetupFiles[2], True);
            SetTICheckBoxesMST(aktProduct.SetupFiles[1].installerId);
          end;
        end
        else
          BtSetup3NextStepClick(Sender);
      end;
    end;
  end;
end;

procedure TResultform1.BtSetup3NextStepClick(Sender: TObject);
var
  checkok: boolean = True;
begin
  if ((aktProduct.SetupFiles[2].installDirectory = '') or
    (aktProduct.SetupFiles[2].installDirectory = 'unknown')) and
    (aktProduct.SetupFiles[2].installerId <> stMsi) and
    (aktProduct.SetupFiles[2].targetOS = osWin) then
  begin
    // checkok := False;
    // we warn here only
    ShowMessage(sWarnInstalldirUnknown);
  end;
  if checkok then
  begin

    case osdsettings.runmode of
      analyzeOnly, singleAnalyzeCreate, analyzeCreateWithUser,
      twoAnalyzeCreate_2,
      createTemplate,
      createTemplateWithUser,
      createMeta, gmUnknown:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode:' +
          GetEnumName(TypeInfo(TRunMode), Ord(osdsettings.runmode)), LLError);
      end;
      threeAnalyzeCreate_3:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct;
        Application.ProcessMessages;
      end;
    end;
  end;
end;

procedure TResultform1.BtSingleAnalyzeAndCreateLinClick(Sender: TObject);
var
  filename: string = '';
  goon: boolean;
  isapp: boolean;
  localTOSset: TTargetOSset;
begin
  goon := False;
  isapp := False;
  OpenDialog1.FilterIndex := 6;   // linux
  if DirectoryExists(myconfiguration.LastSetupFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastSetupFileDir;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastSetupFileDir := ExtractFileDir(OpenDialog1.FileName);
    filename := OpenDialog1.FileName;
    goon := True;
  end
  else
    goon := False;

  if goon then
  begin
    resetGUI;
    osdsettings.runmode := singleAnalyzeCreate;
    setRunMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    //aktProduct.targetOS := osMac;
    localTOSset := aktProduct.productdata.targetOSset;
    Include(localTOSset, osLin);
    aktProduct.productdata.targetOSset := localTOSset;
    aktProduct.SetupFiles[0].targetOS := osLin;
    MemoAnalyze.Clear;
    // we expect deb, rpm so we do not ask for complete directories
    makeProperties;
    resultform1.updateGUI;
    Application.ProcessMessages;
    aktProduct.SetupFiles[0].active := True;
    AnalyzeLin(FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;




procedure TResultform1.BtSingleAnalyzeAndCreateMacClick(Sender: TObject);
var
  filename: string;
  goon: boolean;
  isapp: boolean;
  localTOSset: TTargetOSset;
begin
  goon := False;
  isapp := False;
  MessageDlg('',
            rsSelectAppOrDir, mtInformation,
            [mbOK], '');
  SelectDirectoryDialog1.Title := rsSelectAppOrDir;
  if DirectoryExists(myconfiguration.LastSetupFileDir) then
    SelectDirectoryDialog1.InitialDir := myconfiguration.LastSetupFileDir;
  if SelectDirectoryDialog1.Execute then
  begin
    goon := True;
    filename := SelectDirectoryDialog1.FileName;
    if ExtractFileExt(filename) = '.app' then
    begin
      isapp := True;
    end;
  end;
  if goon and not isapp then
  begin
    MessageDlg('',
              rsSelectMacFile, mtInformation,
              [mbOK], '');
    OpenDialog1.InitialDir := filename;
    ;
    OpenDialog1.FilterIndex := 5;   // macos
    if OpenDialog1.Execute then
    begin
      filename := OpenDialog1.FileName;
      goon := True;
    end
    else
      goon := False;
  end;
  if goon then
  begin
    resetGUI;
    osdsettings.runmode := singleAnalyzeCreate;
    setRunMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    localTOSset := aktProduct.productdata.targetOSset;
    Include(localTOSset, osMac);
    aktProduct.productdata.targetOSset := localTOSset;
    aktProduct.SetupFiles[0].targetOS := osMac;
    MemoAnalyze.Clear;
    // no showCompleteDirDlg for macos
    aktProduct.SetupFiles[0].copyCompleteDir := False;
    makeProperties;
    resultform1.updateGUI;
    Application.ProcessMessages;
    aktProduct.SetupFiles[0].active := True;
    AnalyzeMac(FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;

procedure TResultform1.BtSingleAnalyzeAndCreateMultiClick(Sender: TObject);
var
  localTOSset: TTargetOSset;
  installerselected: boolean = False;
begin
  resetGUI;
  osdsettings.runmode := threeAnalyzeCreate_1;
  setRunMode;
  initaktproduct;
  aktProduct.productdata.targetOSset := [osMulti];
  if MessageDlg(rsThreeAnalyzeAndCreateMsgHead, rsThreeAnalyzeAndCreateMsgFirstSetup,
    mtConfirmation, [mbYes, mbNo], '') = mrYes then
  begin
    OpenDialog1.FilterIndex := 1;   // setup
    if DirectoryExists(myconfiguration.LastSetupFileDir) then
      OpenDialog1.InitialDir := myconfiguration.LastSetupFileDir;
    if OpenDialog1.Execute then
    begin
      myconfiguration.LastSetupFileDir := ExtractFileDir(OpenDialog1.FileName);
      PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
      Application.ProcessMessages;
      localTOSset := aktProduct.productdata.targetOSset;
      Include(localTOSset, osWin);
      aktProduct.productdata.targetOSset := localTOSset;
      aktProduct.SetupFiles[0].targetOS := osWin;
      MemoAnalyze.Clear;
      aktProduct.SetupFiles[0].copyCompleteDir := showCompleteDirDlg;
      makeProperties;
      resultform1.updateGUI;
      Application.ProcessMessages;
      aktProduct.SetupFiles[0].active := True;
      Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
      SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
      installerselected := True;
    end;
  end;
  if not installerselected then
  begin
    BtSetup1NextStepClick(Sender);
  end;
end;

procedure TResultform1.BitBtnClose1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TResultform1.BtAnalyzeOnlyClick(Sender: TObject);
begin
  resetGUI;
  OpenDialog1.FilterIndex := 1;   // setup
  if DirectoryExists(myconfiguration.LastSetupFileDir) then
    OpenDialog1.InitialDir := myconfiguration.LastSetupFileDir;
  if OpenDialog1.Execute then
  begin
    myconfiguration.LastSetupFileDir := ExtractFileDir(OpenDialog1.FileName);
    osdsettings.runmode := analyzeOnly;
    setRunMode;
    MemoAnalyze.Clear;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    makeProperties;
    resultform1.updateGUI;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;


procedure TResultform1.FileHelpClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url + 'opsi-docs-de/4.3/opsi-modules/setup-detector.html'
  else
    myUrl := opsidocs_base_url + 'opsi-docs-en/4.3/opsi-modules/setup-detector.html';
  OpenURL(myUrl);
end;


procedure TResultform1.mst32NameEditChange(Sender: TObject);
begin
end;

procedure TResultform1.PageControl1Change(Sender: TObject);
var
  myimage: TImage;
  pageIndex : integer;
begin
  if FileExists(aktProduct.productdata.productImageFullFileName) then
  begin
    myimage := TImage.Create(self);
    myimage.Picture.LoadFromFile(aktProduct.productdata.productImageFullFileName);
    PaintPreview(myimage);
    FreeAndNil(myimage);
  end;
  pageIndex := PageControl1.ActivePageIndex;
  TreeView1.Select(TreeView1.Items[pageIndex]);
end;

procedure TResultform1.PanelDefaultClick(Sender: TObject);
begin

end;

procedure TResultform1.setup32NameEditChange(Sender: TObject);
begin

end;

procedure TResultform1.FileOpenSetupFileClick(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheetAnalyze;
  BtSingleAnalyzeAndCreateWin.Click;
end;

procedure TResultform1.FileExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TResultform1.MenuItemAboutClick(Sender: TObject);
var
  progname: string;
  msg: string;
  list: TStringList;
begin
  list := TStringList.Create;
  progname := ExtractFileName(ParamStr(0));
  msg := progname + ' Version: ' + myVersion + '      ('+{$i %DATE%} + ', ' +{$i %TIME%}+')';
  //msg := progname + ' Version: ' + myVersion;
  list.Add(msg);
  list.Add('(c) uib gmbh under AGPLv3');
  list.Add('This is a part of the opsi.org project: https://opsi.org');
  list.Add('');
  list.add('Icons from Iconic (https://useiconic.com/) under MIT License.');
  list.add('Icons from feathericons (https://feathericons.com/) under MIT License.');
  //list.add('https://github.com/iconic/open-iconic/blob/master/ICON-LICENSE');
  list.Add('');
  list.Add('Configuration: ');
  list.Add(aktconfigfile);
  list.Add('Log: ');
  list.Add(logdatei.FileName);
  MyMessageDlg.ShowMessage('opsi-setup-detector', list.Text, [mrOk]);
  //ShowMessage(list.Text);
  list.Free;
end;


procedure TResultform1.Panel1Click(Sender: TObject);
begin

end;

procedure TResultform1.ProductIDChange(Sender: TObject);
var
  CurPos: longint;
  inputText, checkedText: string;
  myEdit: TEdit;
begin
  myEdit := Sender as TEdit;
  CurPos := myEdit.SelStart;
  inputText := myEdit.Text;
  checkedText := getPacketIDfromFilename(inputText);

  myEdit.Text := checkedText;
  myEdit.SelStart := CurPos;
end;

function TResultform1.showCompleteDirDlg: boolean;
begin
  Result := False;
  // https://specials.rejbrand.se/TTaskDialog/
  with TTaskDialog.Create(self) do
  try
    Title := rsCopyFileOnlyOrCompleteDirTitle;
    Caption := 'opsi-setup-detector';
    Text := rsCopyFileOnlyOrCompleteDirText;
    CommonButtons := [];
    with TTaskDialogButtonItem(Buttons.Add) do
    begin
      Caption := rsCopyFileOnlyCap;
      ModalResult := mrNo;
    end;
    with TTaskDialogButtonItem(Buttons.Add) do
    begin
      Caption := rsCopyCompleteDirCap;
      ModalResult := mrYes;
    end;
    MainIcon := tdiQuestion;
    if Execute then
    begin
      if ModalResult = mrYes then
        Result := True;
      if ModalResult = mrNo then
        Result := False;
    end;
  finally
    Free;
  end;
end;

procedure TResultform1.showMacOS2StepSelectionDLG;
var
  tmpbool: boolean;
begin
  if myconfiguration.Show2StepMacSeletionWarn then
  begin
    // https://specials.rejbrand.se/TTaskDialog/
    with TTaskDialog.Create(self) do
    try
      Title := rsMac3stepSelectionTitle;
      Caption := 'opsi-setup-detector';
      Text := rsMac3stepSelectionText;
      CommonButtons := [tcbOk];
      MainIcon := tdiInformation;
      VerificationText := rsMacSelectionRememberMe;
      Execute;
      tmpbool := tfVerificationFlagChecked in Flags;
      myconfiguration.Show2StepMacSeletionWarn := not (tmpbool);
      tmpbool := myconfiguration.Show2StepMacSeletionWarn;
      myconfiguration.writeconfig;
    finally
      Free;
    end;
  end;
end;

procedure TResultform1.TreeView1Click(Sender: TObject);
var
  index : integer;
begin
  index := TTreeview(Sender).Selected.Index;
  PageControl1.ActivePageIndex:= index;
end;

// https://www.lazarusforum.de/viewtopic.php?p=116368#p116368
function ChangeFileNameFilter(sFilter: string): string;
  // Filter-String für Linux die Groß/Kleinschreibung als Filter setzen
  {$ifdef LINUX}
var
  sl: TStringList;
  s, s2: string;
  i, k: integer;
  {$endif}
begin
  {$ifdef LINUX}
  // macht aus "*.jpg" ein "*.[jJ][pP][gG]"
  sl := TStringList.Create;
  sl.Delimiter := '|';
  sl.StrictDelimiter := True;
  sl.DelimitedText := sFilter;
  for i := 0 to sl.Count - 1 do
  begin
    if (i mod 2) = 1 then
    begin
      s := sl[i];
      for k := Length(s) downto 1 do
      begin
        s2 := LowerCase(Copy(s, k, 1));
        if s2[1] in ['a'..'z'] then
        begin
          s2 := '[' + s2 + UpperCase(s2) + ']';
          Delete(s, k, 1);
          Insert(s2, s, k);
        end;
      end;
      sl[i] := s;
    end;
  end;
  Result := sl.DelimitedText;
  sl.Free;
  {$else}// Windows / macOs
  Result := sFilter;
  {$endif}
end;

procedure TResultform1.FormCreate(Sender: TObject);
var
  DefaultIcon: TImage;
  tmpimage: TPicture;
  filename: string;
  resourcedir, templatePath: string;
begin
  loadDefaultIcon := True;
  Application.OnIdle := @ApplicationEventIdle;
  // TabSheetIcons presets
  BtnOpenIconFolder.Font.Size := 12;
  designPPI := DesignTimePPI;
  screenPPI := screen.PixelsPerInch;
  DefaultIcon := TImage.Create(TabSheetIcons);
  {$IFDEF LINUX}
  // scale form
  AutoAdjustLayout(lapAutoAdjustForDPI, 91, screen.PixelsPerInch, 0, 0);
  DefaultIcon.AutoAdjustLayout(lapAutoAdjustForDPI, 91, screen.PixelsPerInch, 0, 0);
  {$ENDIF LINUX}
  LogDatei.log('design ppi: 91 , screen: ' + IntToStr(Screen.PixelsPerInch),
    LLessential);
  ;
  {$IFDEF WINDOWS}
  DefaultIcon.Picture.LoadFromFile(ExtractFileDir(Application.Params[0]) +
    PathDelim + 'template-files' + PathDelim + 'default' + PathDelim +
    'images' + PathDelim + 'template.png');
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  // the first path is in the development environment
  resourcedir := ExtractFileDir(Application.ExeName);
  templatePath := resourcedir + PathDelim + 'template-files';
  if not DirectoryExists(templatePath) then
    resourcedir := '/usr/share/opsi-setup-detector';
  {$IFDEF DARWIN}
  // the first path is in the development environment
  resourcedir := ExtractFileDir(Application.ExeName) + PathDelim + '../../..';
  templatePath := resourcedir + PathDelim + 'template-files' + PathDelim + 'default';
  if not DirectoryExists(templatePath) then
    //templatePath := '/usr/local/share/opsi-setup-detector/template-files';
    resourcedir := ExtractFileDir(Application.ExeName) + PathDelim + '../Resources';
  {$ENDIF DARWIN}
  filename := resourcedir + PathDelim + 'template-files' + PathDelim +
    'default' + PathDelim + 'images' + PathDelim + 'template.png';
  if fileexists(filename) then
    DefaultIcon.Picture.LoadFromFile(filename)
  else
  begin
    filename := ExtractFileDir(Application.Params[0]) + PathDelim +
      'template-files' + PathDelim + 'default' + PathDelim + 'images' +
      PathDelim + 'template.png';
    if fileexists(filename) then
      DefaultIcon.Picture.LoadFromFile(filename)
    else
      LogDatei.log('Could not find template.png ', LLError);
  end;
  tmpimage := TPicture.Create;

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateWin.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'analyzepack4.xpm');
  BtATwonalyzeAndCreate.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateLin.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateMac.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateMulti.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateWithUser.Glyph.Assign(tmpimage.Bitmap);

  // Help icons at linux has to be loaded as xpm file
  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' + PathDelim +
    'help-circle20.png');
  SpeedButtonHelpMain.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpStartOSIndep.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpStartWin.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpStartLin.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpStartMac.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpStartMulti.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpAnalyze.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpSetup2.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpSetup2.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpSetup3.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpProd1.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpProd2.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpIcon.Glyph.Assign(tmpimage.Bitmap);
  SpeedButtonHelpCreate.Glyph.Assign(tmpimage.Bitmap);


  FreeAndNil(tmpimage);
  {$ENDIF UNIX}
  PaintPreview(DefaultIcon);
  DataModule1.SetFontName(TControl(Sender), myFont);
  OSD_info := TOSD_info.Create(resultForm1);
  md := TMarkdownProcessor.createDialect(mdDaringFireball);
  md.UnSafe := True;
  //HtmlViewerDesc.LoadFromString(CSSDecoration + '');
  //HtmlViewerAdvice.LoadFromString('');
  //{$ifdef UNIX}
  // make opendialog filter case insensitive at Unix
  // https://www.lazarusforum.de/viewtopic.php?p=116368#p116368
  OpenDialogSetupfile.Filter := ChangeFileNameFilter(OpenDialogSetupfile.Filter);
  OpenDialog1.Filter := ChangeFileNameFilter(OpenDialog1.Filter);
  //{$endif}
  TreeView1.Select(TreeView1.Items[0]);
  PageControl1.ActivePageIndex:=0;
  LogDatei.log('Finished FormCreate ', LLInfo);
end;

procedure TResultform1.memoadd(line: string);
begin
  MemoAnalyze.append(line);
end;

procedure TResultform1.ApplicationEventIdle(Sender: TObject; var Done: boolean);
begin
  Application.ProcessMessages;
  sleep(100);
end;



(*
procedure TResultform1.RadioButtonBuildModeChange(Sender: TObject);
var
  RadioButtonName: string;
begin
  RadioButtonName := (Sender as TRadioButton).Name;
  if RadioButtonName = 'RadioButtonCreateOnly' then
  begin
  end
  else if RadioButtonName = 'RadioButtonPackageBuilder' then
  begin
  end
  else if RadioButtonName = 'RadioButtonBuildPackage' then
  begin
  end;
end;
*)

procedure TResultform1.SBtnExitClick(Sender: TObject);
begin
  LogDatei.log('Choosed exit Button - Terminate Program', LLnotice);
  resultForm1.Close;
  resultForm1.Destroy;
  freebasedata;
  Application.Terminate;
end;

procedure TResultform1.SpeedButtonHelpAnalyzeClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-use-single-analyze'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-use-single-analyze';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpCreateClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-create'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-create';
  OpenURL(myUrl);
  //ShowMessage(rsNotImplemented);
end;

procedure TResultform1.SpeedButtonHelpIconClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration-icon'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration-icon';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpMainClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url + 'opsi-docs-de/4.3/opsi-modules/setup-detector.html'
  else
    myUrl := opsidocs_base_url + 'opsi-docs-en/4.3/opsi-modules/setup-detector.html';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpProd1Click(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration1'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration1';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpProd2Click(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration-priority_dependency'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-product-configuration-priority_dependency';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpStartOSIndepClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-os-independent'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-os-independent';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpSetup1Click(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-use-single-setup-data'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-use-single-setup-data';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpStartLinClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-linux'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-linux';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpStartMacClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-macos'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-macos';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpStartMultiClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-multiplatform'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-multiplatform';
  OpenURL(myUrl);
end;

procedure TResultform1.SpeedButtonHelpStartWinClick(Sender: TObject);
var
  myUrl: string;
begin
  if LowerCase(osdsettings.mylang) = 'de' then
    myUrl := opsidocs_base_url +
      'opsi-docs-de/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-windows'
  else
    myUrl := opsidocs_base_url +
      'opsi-docs-en/4.3/opsi-modules/setup-detector.html#opsi-setup-detector-tasks-windows';
  OpenURL(myUrl);
end;

procedure checkWorkbench;
begin
  if DirectoryExists(myconfiguration.workbench_Path) then
  begin
    resultForm1.LabelWorkbenchOK.Visible := True;
    resultForm1.LabelWorkbenchNotOK.Visible := False;
  end
  else
  begin
    resultForm1.LabelWorkbenchOK.Visible := False;
    resultForm1.LabelWorkbenchNotOK.Visible := True;
  end;
end;

procedure TResultform1.TabSheetCreateShow(Sender: TObject);
begin
  checkWorkbench;
  TabSheetShow(Sender);
end;

procedure TResultform1.TabSheetIconsShow(Sender: TObject);
begin

end;

procedure TResultform1.TabSheetSetup1Enter(Sender: TObject);
begin
  // force focus - avoid cursor in one of the edit boxes
  TICheckBoxS1Silent.SetFocus;
end;

procedure TResultform1.TabSheetSetup2Enter(Sender: TObject);
begin
  // force focus - avoid cursor in one of the edit boxes
  TICheckBoxS2Silent.SetFocus;
end;

procedure TResultform1.TabSheetSetup3Enter(Sender: TObject);
begin
  // force focus - avoid cursor in one of the edit boxes
  TICheckBoxS3Silent.SetFocus;
end;

procedure TResultform1.TabSheetStartExit(Sender: TObject);
begin

end;

procedure TResultform1.CallMakeProperties(Sender: TObject);
begin
  makeProperties;
  resultform1.updateGUI;
end;

procedure TResultform1.TabSheetShow(Sender: TObject);
var
  pageindex : integer;
begin
  pageIndex := PageControl1.ActivePageIndex;
  TreeView1.Select(TreeView1.Items[pageIndex]);
end;

procedure TResultform1.TabSheetWingetContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TResultform1.TaskPanelWinClick(Sender: TObject);
begin

end;

procedure TResultform1.TICheckBoxlicenseRequiredChange(Sender: TObject);
begin
  makeProperties;
  resultform1.updateGUI;
end;

procedure TResultform1.TICheckBoxS1MstChange(Sender: TObject);
begin
  if (Sender as TTICheckBox).State = cbChecked then
  begin
    FlowPanelMst.Enabled := True;
  end
  else
  begin
    FlowPanelMst.Enabled := False;
  end;
end;

procedure TResultform1.TICheckBoxS1SilentChange(Sender: TObject);
var
  setupindex: integer = 0;
  cmdstr, fromstr, tostr: string;
begin
  if (Sender as TTICheckBox).Name = 'TICheckBoxS1Silent' then setupindex := 0
  else if (Sender as TTICheckBox).Name = 'TICheckBoxS2Silent' then setupindex := 1
  else if (Sender as TTICheckBox).Name = 'TICheckBoxS3Silent' then setupindex := 2;

  if (Sender as TTICheckBox).State = cbChecked then
  begin
    { install }
    { get the command line }
    cmdstr := aktProduct.SetupFiles[setupindex].installCommandLine;
    { get the string to replace }
    fromstr := installerArray[integer(aktProduct.SetupFiles[setupindex].installerId)].unattendedsetup;
    { get the string to replace with}
    tostr := installerArray[integer(aktProduct.SetupFiles[setupindex].installerId)].silentsetup;
    cmdstr := ReplaceText(cmdstr, fromstr, tostr);
    { set the command line }
    aktProduct.SetupFiles[setupindex].installCommandLine := cmdstr;
    { uninstall }
    { get the command line }
    cmdstr := aktProduct.SetupFiles[setupindex].uninstallCommandLine;
    { get the string to replace }
    fromstr := installerArray[integer(aktProduct.SetupFiles[setupindex].installerId)].unattendeduninstall;
    { get the string to replace with}
    tostr := installerArray[integer(aktProduct.SetupFiles[setupindex].installerId)].silentuninstall;
    cmdstr := ReplaceText(cmdstr, fromstr, tostr);
    { set the command line }
    aktProduct.SetupFiles[setupindex].uninstallCommandLine := cmdstr;
  end
  else
  begin
    { install }
    { get the command line }
    cmdstr := aktProduct.SetupFiles[setupindex].installCommandLine;
    { get the string to replace }
    fromstr := installerArray[integer(aktProduct.SetupFiles[setupindex].installerId)].silentsetup;
    { get the string to replace with}
    tostr := installerArray[integer(aktProduct.SetupFiles[setupindex].installerId)].unattendedsetup;
    cmdstr := ReplaceText(cmdstr, fromstr, tostr);
    { set the command line }
    aktProduct.SetupFiles[setupindex].installCommandLine := cmdstr;
    { uninstall }
    { get the command line }
    cmdstr := aktProduct.SetupFiles[setupindex].uninstallCommandLine;
    { get the string to replace }
    fromstr := installerArray[integer(aktProduct.SetupFiles[setupindex].installerId)].silentuninstall;
    { get the string to replace with}
    tostr := installerArray[integer(aktProduct.SetupFiles[setupindex].installerId)].unattendeduninstall;
    cmdstr := ReplaceText(cmdstr, fromstr, tostr);
    { set the command line }
    aktProduct.SetupFiles[setupindex].uninstallCommandLine := cmdstr;
  end;
end;

procedure TResultform1.TICheckBoxS2MstChange(Sender: TObject);
begin
  if (Sender as TTICheckBox).State = cbChecked then
  begin
    FlowPanelMst1.Enabled := True;
  end
  else
  begin
    FlowPanelMst1.Enabled := False;
  end;
end;

procedure TResultform1.TIComboBoxChannelChange(Sender: TObject);
begin
  Application.ProcessMessages;
  makeProperties;
  resultform1.updateGUI;
  TIGridProp.Refresh;
end;

procedure TResultform1.TIComboBoxChannelEditingDone(Sender: TObject);
begin

end;

procedure TResultform1.WingetInputEditingDone(Sender: TObject);
begin
  osdanalyzewin.get_winget_info(aktProduct.SetupFiles[0]);
  TIEditSetup4Command.Refresh;
  TIEditSetup4UnCommand.Refresh;
end;


procedure TResultform1.TIEditProdIDChange(Sender: TObject);
begin

end;

procedure TResultform1.TIEditProdIDExit(Sender: TObject);
begin
  {$IFDEF DARWIN}
  // lower case and replace special chars by _
  TTIEdit(Sender).Caption := cleanOpsiId(TTIEdit(Sender).Caption);
  {$ENDIF DARWIN}
end;

procedure TResultform1.TIEditProdIDSizeConstraintsChange(Sender: TObject);
begin

end;

procedure TResultform1.TIEditProdVersion3Change(Sender: TObject);
begin

end;

procedure TResultform1.TIEditProdVersion3Exit(Sender: TObject);
begin

end;

procedure TResultform1.TIEditSetup1UnProgramEditingDone(Sender: TObject);
begin
  updateUninstaller(aktProduct.SetupFiles[0]);
  TTIEdit(Sender).Refresh;
end;

procedure TResultform1.TIEditSetup2UnProgramEditingDone(Sender: TObject);
begin
  updateUninstaller(aktProduct.SetupFiles[1]);
end;

procedure TResultform1.TIEditSetup3UnProgramEditingDone(Sender: TObject);
begin
  updateUninstaller(aktProduct.SetupFiles[2]);
end;


procedure TResultform1.TIMemoAdviceEditingDone(Sender: TObject);
var
  fs: TStringStream;
  pHTML: TIpHtml;
begin
  HtmlContent := md.process(TTIMemo(Sender).Text);
  try
    fs := TStringStream.Create(CSSDecoration + HtmlContent);
    try
      pHTML := TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanelDesc
      pHTML.LoadFromStream(fs);
    finally
      fs.Free;
    end;
    IpHtmlPanelAdvice.SetHtml(pHTML);
    //Caption := IpHtmlPanelAdvice.Title;
  except
    on E: Exception do
    begin
      MessageDlg('Error: ' + E.Message, mtError, [mbCancel], 0);
    end;
  end;
end;

procedure TResultform1.TIMemoDescEditingDone(Sender: TObject);
var
  fs: TStringStream;
  pHTML: TIpHtml;
begin
  HtmlContent := md.process(TTIMemo(Sender).Text);
  //HtmlViewerDesc.LoadFromString(CSSDecoration + HtmlContent);
  //HtmlViewerDesc.LoadFromString(HtmlContent);
  try
    fs := TStringStream.Create(CSSDecoration + HtmlContent);
    try
      pHTML := TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanelDesc
      pHTML.LoadFromStream(fs);
    finally
      fs.Free;
    end;
    IpHtmlPanelDesc.SetHtml(pHTML);
    //Caption := IpHtmlPanelDesc.Title;
  except
    on E: Exception do
    begin
      MessageDlg('Error: ' + E.Message, mtError, [mbCancel], 0);
    end;
  end;
end;


procedure TResultform1.genRttiEditChange(Sender: TObject);
var
  aktCaretPos: TPoint;
  col: longint;
begin
  if Sender.ClassType = TTIEdit then
    if TTIEdit(Sender).Name = 'TIEditProdID' then
    begin
      {$IFNDEF DARWIN}
      // seems not to work at macOS: we get a last-to-first sequence of chars
      aktCaretPos := TTIEdit(Sender).CaretPos;
      col := aktCaretPos.X;
      // lower case and replace special chars by _
      TTIEdit(Sender).Caption := cleanOpsiId(TTIEdit(Sender).Caption);
      // restore the caret position
      TTIEdit(Sender).SelStart := col;
      {$ENDIF DARWIN}
    end;
  TControl(Sender).EditingDone;
end;



procedure TResultform1.TimerFirstconfigTimer(Sender: TObject);
begin
  TimerFirstconfig.Enabled := False;
  if not myconfiguration.config_filled then
  begin
    ShowMessage(rsWeNeedConfiguration);
    MenuItemConfigClick(Sender);
    logdatei.log('Missing configs found- config dialog forced', LLinfo);
  end;
end;

procedure TResultform1.TIRadioGroupBuildModeClick(Sender: TObject);
begin

end;

procedure TResultform1.TIS1UrlClick(Sender: TObject);
var
  link: string;
  PropInfo: PPropInfo;
begin
  link := TTILabel(Sender).Caption;
  OpenURL(link);
end;

procedure TResultform1.TIS1UrlMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TResultform1.TIS1UrlMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;



procedure TResultform1.TISpinEditPrioChange(Sender: TObject);
begin
  TITrackBarPrio.Loaded;
  TITrackBarPrio.Refresh;
  procmess;
end;

procedure TResultform1.TITrackBarPrioChange(Sender: TObject);
begin
  TISpinEditPrio.Loaded;
  TISpinEditPrio.Refresh;
  procmess;
end;


end.
