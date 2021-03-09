unit osdform;

 {$mode objfpc}{$H+}
//{$MODE DELPHI}{$H+}


interface

uses

  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF WINDOWS}
  lazfileutils,
  Classes, SysUtils, FileUtil, RTTICtrls, RTTIGrids,
  Forms, Controls, Graphics,
  LCLType,
  LclIntf,
  Dialogs, ExtCtrls,
  StdCtrls,
  Buttons,
  ComCtrls,
  Menus,
  //Registry,
  //Strings,
  StrUtils,
  //Process,
  typinfo,
  CustApp,
  //fileinfo,
  //osdhelper,
  osdanalyzewin,
  osdanalyzelin,
  osdanalyzemac,
  //winpeimagereader,
  lcltranslator,
  //defaulttranslator,
  EditBtn,
  //Spin,
  //JSONPropStorage,
  Grids,
  PairSplitter, ColorBox,
  oslog,
  osdbasedata, osdconfigdlg, osdcreate, fpjsonrtti, osddlgnewdependency,
  osddlgnewproperty, osparserhelper,
  osddatamod,
  osdcheckentriesdlg,
  Contnrs;
//openfiledirdlg;

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
    BitBtnOpenMst3: TBitBtn;
    BtAnalyzeOnly: TBitBtn;
    BtATwonalyzeAndCreate: TBitBtn;
    BtCreateEmptyTemplateWin: TBitBtn;
    BtCreateEmptyTemplateMulti: TBitBtn;
    BtCreateEmptyTemplateLin: TBitBtn;
    BtCreateEmptyTemplateMac: TBitBtn;
    BtnOpenIconFolder: TBitBtn;
    BitBtnAddDep: TBitBtn;
    BitBtnAddProp: TBitBtn;
    BitBtnDelDep: TBitBtn;
    BitBtnDelProp: TBitBtn;
    BitBtnEditProp: TBitBtn;
    BitBtnEditDep: TBitBtn;
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
    BtSingleAnalyzeAndCreateWin: TBitBtn;
    BtSingleAnalyzeAndCreateMulti: TBitBtn;
    BtSingleAnalyzeAndCreateLin: TBitBtn;
    BtSingleAnalyzeAndCreateMac: TBitBtn;
    CheckBoxDefaultIcon: TCheckBox;
    CheckBoxNoIcon: TCheckBox;
    CheckGroupBuildMode: TCheckGroup;
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
    FlowPanel3: TFlowPanel;
    FlowPanel4: TFlowPanel;
    FlowPanel6: TFlowPanel;
    FlowPanel8: TFlowPanel;
    FlowPanelMsiId1: TFlowPanel;
    FlowPanelMsiId2: TFlowPanel;
    FlowPanelMST1: TFlowPanel;
    FlowPanelMST2: TFlowPanel;
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
    FlowPanelWin: TFlowPanel;
    FlowPanelWin1: TFlowPanel;
    FlowPanelWin2: TFlowPanel;
    FlowPanelWin3: TFlowPanel;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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
    LabelNumber: TLabel;
    LabelNumIcons: TLabel;
    LabelNameSelIcon: TLabel;
    LabelIconName: TLabel;
    LabelIconDir: TLabel;
    LabelIconPreview: TLabel;
    Label63: TLabel;
    Label69: TLabel;
    LabelLogInfo: TLabel;
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
    radioBuildModebuildOnly: TRadioButton;
    radioBuildModebuildInstall: TRadioButton;
    RadioButtonBuildPackage: TRadioButton;
    RadioButtonCreateOnly: TRadioButton;
    RadioButtonPackageBuilder: TRadioButton;
    RadioGroup1: TRadioGroup;
    SaveDialogProj: TSaveDialog;
    SBtnExit: TSpeedButton;
    ScrollBox1: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    StringGridDep: TStringGrid;
    StringGridProp: TStringGrid;
    TabSheetSetup3: TTabSheet;
    TabSheetIcons: TTabSheet;
    TabSheetProduct2: TTabSheet;
    TabSheetCreate: TTabSheet;
    TabSheetStart: TTabSheet;
    TabSheetSetup2: TTabSheet;
    TabSheetProduct: TTabSheet;
    TabSheetSetup1: TTabSheet;
    FileExit: TMenuItem;
    //FileCreateLogfile: TMenuItem;
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
    TICheckBoxS1Mst: TTICheckBox;
    TICheckBoxlicenseRequired: TTICheckBox;
    TICheckBoxS2Mst: TTICheckBox;
    TICheckBoxS3Mst: TTICheckBox;
    TIEditInstallDir2: TTIEdit;
    TIEditInstallDir3: TTIEdit;
    TIEditMsiId2: TTIEdit;
    TIEditMsiId3: TTIEdit;
    TIEditMstFile2: TTIEdit;
    TIEditMstFile3: TTIEdit;
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
    TIEditworkbenchpath: TTIEdit;
    TIEditSetupFileSizeMB1: TTIEdit;
    TIEditRequiredSizeMB1: TTIEdit;
    TIEditInstallDir1: TTIEdit;
    TIEditSetupfile1: TTIEdit;
    TIEditMstFile1: TTIEdit;
    TIEditSetupFileSizeMB2: TTIEdit;
    TIGrid1: TTIGrid;
    TIGrid2: TTIGrid;
    TIImageIconPreview: TTIImage;
    TILabelDirSelIcon: TTILabel;
    TILabelInstaller2: TTILabel;
    TILabelInstaller3: TTILabel;
    TIMemoAdvice: TTIMemo;
    TIMemoDesc: TTIMemo;
    TimerFirstconfig: TTimer;
    TIS1Url: TTILabel;
    TILabelInstaller1: TTILabel;
    TIS2Url: TTILabel;
    TIS3Url: TTILabel;
    TISpinEditPrio: TTISpinEdit;
    TISpinEditPackageVers: TTISpinEdit;
    TITrackBarPrio: TTITrackBar;
    ToolBar1: TToolBar;
    mysetup1: TSetupFile;

    procedure BitBtnAddDepClick(Sender: TObject);
    procedure BitBtnAddPropClick(Sender: TObject);
    procedure BitBtnDelDepClick(Sender: TObject);
    procedure BitBtnDelPropClick(Sender: TObject);
    procedure BitBtnEditDepClick(Sender: TObject);
    procedure BitBtnEditPropClick(Sender: TObject);
    procedure BitBtnOpenFile1Click(Sender: TObject);
    procedure BitBtnOpenFileClick(Sender: TObject);
    procedure BitBtnOpenMst1Click(Sender: TObject);
    procedure BitBtnOpenMst2Click(Sender: TObject);
    procedure BitBtnRecheckWorkbenchClick(Sender: TObject);
    procedure BitBtnWorkBenchPathClick(Sender: TObject);
    procedure BtAnalyzeNextStepClick(Sender: TObject);
    procedure BtATwonalyzeAndCreateClick(Sender: TObject);
    procedure BtCreateEmptyTemplateMultiClick(Sender: TObject);
    procedure BtCreateEmptyTemplateWinClick(Sender: TObject);
    procedure BtCreateEmptyTemplateLinClick(Sender: TObject);
    procedure BtCreateEmptyTemplateMacClick(Sender: TObject);
    procedure BtCreateProductClick(Sender: TObject);
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
    procedure CheckBoxDefaultIconChange(Sender: TObject);
    procedure CheckBoxNoIconChange(Sender: TObject);
    procedure FlowPanel14Click(Sender: TObject);
    procedure FlowPanel18Click(Sender: TObject);
    procedure FlowPanel3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    procedure RadioButtonBuildModeChange(Sender: TObject);

    //procedure SBtnOpenClick(Sender: TObject);
    procedure SBtnExitClick(Sender: TObject);
    procedure TabSheetCreateShow(Sender: TObject);
    procedure TabSheetIconsShow(Sender: TObject);
    procedure TabSheetStartExit(Sender: TObject);
    procedure TICheckBoxlicenseRequiredChange(Sender: TObject);
    procedure TICheckBoxS1MstChange(Sender: TObject);
    procedure TICheckBoxS2MstChange(Sender: TObject);
    procedure TIEditProdIDChange(Sender: TObject);
    procedure TIEditProdVersion3Change(Sender: TObject);
    procedure TIEditProdVersion3Exit(Sender: TObject);
    procedure TimerFirstconfigTimer(Sender: TObject);
    procedure TIS1UrlClick(Sender: TObject);
    procedure TIS1UrlMouseEnter(Sender: TObject);
    procedure TIS1UrlMouseLeave(Sender: TObject);
    procedure TISpinEditPrioChange(Sender: TObject);
    procedure TITrackBarPrioChange(Sender: TObject);
    procedure fetchDepPropFromForm;
    procedure ApplicationEventIdle(Sender: TObject; var Done: boolean);
    procedure genRttiEditChange(Sender: TObject);
    procedure makeProperties;

    procedure IconDisplayOnMouseEnter(Sender: TObject);
    procedure PaintPreview(Image: TImage);
    procedure IconDisplayOnClick(Sender: TObject);
    procedure showCheckEntriesWarning;
    //procedure SetFontName(Control: TControl; Name: string);
    function showCompleteDirDlg: boolean;
    procedure showMacOS2StepSelectionDLG;
  private
    { private declarations }
    procedure OpenMSTFile(var mysetup: TSetupFile);
    procedure SetTICheckBoxesMST(Installer: TKnownInstaller);
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
  end;


procedure main1;
procedure main2;
procedure mywrite(line: string); overload;
procedure mywrite(line: string; loglevel: integer); overload;
procedure checkWorkbench;
procedure procmess;


var
  resultForm1: TresultForm1;
  optionlist: TStringList;
  myexitcode: integer;
  Result: integer;
  myExeDir: string;
  myfilename, myerror: string;
  //myVersion: string;
  MSIfilename, MSTfilename, SetupFilename: string;
  showgui: boolean;
  configDir: string;
  configFileName: string;
  packetBaseDir: string;
  productid: string;
  //patchlist: TStringList;
  fConfig: Text;
  setupTypestr: string;
  markerEmbeddedMSI: boolean = False;
  markerInstallShield: boolean = False;
  opsidir: string;   // opsi.org (set in main)
  opsitmp: string;   // %TEMP%/opsitmp  (set in main)
  //Logfile: string;   // name of logfile (set in main)

  //*****************************************
  test: boolean = False;
  overwriteExistingProduct: boolean = False;
  showMSI: boolean = True;
  showInnoSetup: boolean = True;
  showNSIS: boolean = True;
  showInstallShield: boolean = True;
  showInstallShieldMSI: boolean = True;
  showAdvancedMSI: boolean = True;
  //*****************************************
  //myobject : TMyClass;
  firstshowconfigdone: boolean = False;
  startupfinished: boolean = False;
  mylocaledir: string;
//myFont : string;


resourcestring

  sMBoxHeader = 'opsi setup detector';
  sHelpHeader = 'opsi setup detector Help';
  // dialogs
  sAskCreatePacket = 'Create packet %0:s?';
  sHelpFile = 'languages\Help.de.html';
  sLogfileInfo = 'Logfile created: %s';
  // Error messages
  //sErrFldInstDirEmpty = 'Error: Field Install Directory is empty!';
  sErrFldMsiProductCodeEmpty =
    'Error: Field MSI Product Code is empty!' + Lineending +
    'The MSI Product Code is needed for correct Uninstall process.' +
    Lineending +
    'Please install this Product and check for the MSI Product Code and write it to the delsub script';
  sErrProductIdEmpty = 'Error: Field opsi Product ID is empty!';
  sErrProductVersionEmpty = 'Error: Field Product Version is empty!';
  //sErrFldSetupEmpty = 'Error: No setup file selected!';
  //sErrMSINotFound = 'Error: MSI file does not exist!';
  sErrMSTNotFound =
    'Error: MST file does not exist in the MSI-directory!';
  sErrOpsiPackageBuilderStart = 'Error invoking opsi package builder';
  //sErrOpsiPackageBuilderErrCode =
  //  'Error invoking OPSI Package Builder (error code=%0:d). Please remove quiet option to check for further information.';
  sErrPacketBaseDirNotFound =
    'Error: The Path to opsi-work-bench is empty or not valid!';
  //sErrProductVersionInvalid = 'Error: for Product Version only numbers allowed !';
  //sErrSelectInstallType =
  //  'First select setup type tab (MSI, Inno Setup ...) and fill out form';
  //sErrSetupFileNotFound = 'Error: Setup file does not exist!';
  //sErrPacketDirExists = 'Error: opsi packet folder %s already exists!';
  sErrExtractMSIfailed = 'Error: extracting MSI from %s failed!';
  sInfoFinished = 'Create opsi package finished.';
  //sWarnInstDirEmptyNoDeinstall =
  //  'Warning: Install Directory is empty, deinstall script cannot be patched!';

  //sErrProductIdEmpty = 'We need a productId.';
  //sErrProductVersionEmpty = 'We need a productVersion.';
  sWarnInstalldirUnknown = 'Error: Field Install Directory is empty or not valid!' +
    Lineending + 'For this Installer the Installdir could not be detected.' +
    Lineending + 'The Installdir is needed for correct Uninstall process.' +
    Lineending +
    'Please install this Product and check for the Installdir and write it to the setup and the uninstall script';

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
    'First select MacOS .app or Directory where to find MacOS Installer files';
  rsSelectMacFile = 'Now select MacOS installer file.';
  rsRpmAnalyze = 'Analyze of RPM files';
  rsRPMAnalyzeNotLinux = 'Detailed anlyze of deb files can only be done at linux';
  rsDebAnalyze = 'Analyze of DEB files';
  rsDebAnalyzeNotLinux = 'Detailed anlyze of deb files can only be done at linux';
  rscheckEntriesTitle = 'Please keep in mind:';
  rscheckEntriesMsg =
    'The following data are automatically detected.' + Lineending +
    'You have to check every data field if this data are correct or plausible.' +
    Lineending + 'For some data you may have to install the program once ' +
    Lineending + 'and than get the needed data from the completed installation.';
  rscheckEntriesRememberMe = 'Do not show this Message again';
  rsMac3stepSelectionText = 'To select a MacOS installer we have a two Steps' + LineEnding +
                 'Firststep is Dialog to select a Directory where to find the installer ' + LineEnding +
                 'or the "installer.app" Directory' + LineEnding +
                 'Second step (if needed) is a Dialog to select a installer file';
  rsMac3stepSelectionTitle = 'Attention: Two Step Selection Dialog';
  rsMacSelectionRememberMe = 'Do not show this Message again';

implementation

{$R *.lfm}
//{$R manifest.rc}


procedure procmess;
begin
  Application.ProcessMessages;
end;

procedure mywrite(line: string);
begin
  mywrite(line, LLNotice);
end;

procedure mywrite(line: string; loglevel: integer);
begin
  if showgui then
  begin
    resultform1.memoadd(line);
  end
  else
    writeln(line);
  LogDatei.log(line, loglevel);
end;



procedure WriteHelp;
var
  progname: string;
begin
  progname := ExtractFileName(ParamStr(0));
  writeln(ParamStr(0));
  writeln(progname);
  writeln('Version ' + myVersion);
  writeln(myerror);
  writeln('Usage:');
  writeln(progname + '[Options]');
  writeln('Options:');
  writeln(' --help -> write this help and exit');
  writeln(' --help -> write this help and exit');
  writeln(' --filename=<path\filename> -> file to analyze)');
  writeln(' --f <path\filename> -> file to analyze)');
  writeln(' --nogui -> do not show interactive output window)');
  writeln(' --n -> do not show interactive output window)');
  writeln(' --targetOS=<os> -> Analyze for target where <os> is on of (win,lin,mac)');
  writeln(' --t <os> -> Analyze for target where <os> is on of (win,lin,mac)');
  writeln(' --productID=<id> -> Create product with productID <id>');
  writeln(' --p <id> -> Create product with productID <id>');
  Application.Terminate;
  halt(-1);
  Exit;
end;


procedure initGUI;

begin
  LogDatei.log('Start initGUI ... ', LLInfo);
  with resultform1 do
  begin
    with aktProduct do
    begin
      TILabelInstaller1.Link.SetObjectAndProperty(SetupFiles[0], 'installerid');
      TILabelInstaller2.Link.SetObjectAndProperty(SetupFiles[1], 'installerid');
      TILabelInstaller3.Link.SetObjectAndProperty(SetupFiles[2], 'installerid');
      //TIComboBoxInstaller1.Link.SetObjectAndProperty(SetupFiles[0], 'installerid');
      //TIComboBoxInstaller2.Link.SetObjectAndProperty(SetupFiles[1], 'installerid');
      TIEditSetupfile1.Link.SetObjectAndProperty(SetupFiles[0], 'setupFullFileName');
      TIEditSetupFile2.Link.SetObjectAndProperty(SetupFiles[1], 'setupFullFileName');
      TIEditSetupFile3.Link.SetObjectAndProperty(SetupFiles[2], 'setupFullFileName');
      //TIComboBoxArch1.Link.SetObjectAndProperty(SetupFiles[0], 'architecture');
      //TIComboBoxArch2.Link.SetObjectAndProperty(SetupFiles[1], 'architecture');
      TIEditMstFile1.Link.SetObjectAndProperty(SetupFiles[0], 'mstFullFileName');
      TIEditMstFile2.Link.SetObjectAndProperty(SetupFiles[1], 'mstFullFileName');
      TIEditMstFile3.Link.SetObjectAndProperty(SetupFiles[2], 'mstFullFileName');
      TICheckBoxS1Mst.Link.SetObjectAndProperty(SetupFiles[0], 'mstAllowed');
      TICheckBoxS2Mst.Link.SetObjectAndProperty(SetupFiles[1], 'mstAllowed');
      TICheckBoxS3Mst.Link.SetObjectAndProperty(SetupFiles[2], 'mstAllowed');
      TIEditMsiId1.Link.SetObjectAndProperty(SetupFiles[0], 'msiId');
      TIEditMsiId2.Link.SetObjectAndProperty(SetupFiles[1], 'msiId');
      TIEditMsiId3.Link.SetObjectAndProperty(SetupFiles[2], 'msiId');
      TIEditSoftVersion1.Link.SetObjectAndProperty(SetupFiles[0], 'SoftwareVersion');
      TIEditSoftVersion2.Link.SetObjectAndProperty(SetupFiles[1], 'SoftwareVersion');
      TIEditSoftVersion3.Link.SetObjectAndProperty(SetupFiles[2], 'SoftwareVersion');
      TIEditSetupFileSizeMB1.Link.SetObjectAndProperty(SetupFiles[0], 'setupFileSize');
      TIEditSetupFileSizeMB2.Link.SetObjectAndProperty(SetupFiles[1], 'setupFileSize');
      TIEditSetupFileSizeMB3.Link.SetObjectAndProperty(SetupFiles[2], 'setupFileSize');
      TIEditRequiredSizeMB1.Link.SetObjectAndProperty(SetupFiles[0], 'requiredSpace');
      TIEditRequiredSizeMB2.Link.SetObjectAndProperty(SetupFiles[1], 'requiredSpace');
      TIEditRequiredSizeMB3.Link.SetObjectAndProperty(SetupFiles[2], 'requiredSpace');
      TIEditInstallDir1.Link.SetObjectAndProperty(SetupFiles[0], 'installDirectory');
      TIEditInstallDir2.Link.SetObjectAndProperty(SetupFiles[1], 'installDirectory');
      TIEditInstallDir3.Link.SetObjectAndProperty(SetupFiles[2], 'installDirectory');
      TIEditSetup1Command.Link.SetObjectAndProperty(SetupFiles[0], 'installCommandLine');
      TIEditSetup2Command.Link.SetObjectAndProperty(SetupFiles[1], 'installCommandLine');
      TIEditSetup3Command.Link.SetObjectAndProperty(SetupFiles[2], 'installCommandLine');
      TIEditSetup1UnCommand.Link.SetObjectAndProperty(SetupFiles[0],
        'uninstallCommandLine');
      TIEditSetup2UnCommand.Link.SetObjectAndProperty(SetupFiles[1],
        'uninstallCommandLine');
      TIEditSetup3UnCommand.Link.SetObjectAndProperty(SetupFiles[2],
        'uninstallCommandLine');
      TIEditSetup1UnProgram.Link.SetObjectAndProperty(SetupFiles[0], 'uninstallProg');
      TIEditSetup2UnProgram.Link.SetObjectAndProperty(SetupFiles[1], 'uninstallProg');
      TIEditSetup3UnProgram.Link.SetObjectAndProperty(SetupFiles[2], 'uninstallProg');
      TIS1Url.Link.SetObjectAndProperty(SetupFiles[0], 'link');
      TIS2Url.Link.SetObjectAndProperty(SetupFiles[1], 'link');
      TIS3Url.Link.SetObjectAndProperty(SetupFiles[2], 'link');
      // product
      TIEditProdVersion3.Link.SetObjectAndProperty(productdata, 'productVersion');
      TISpinEditPackageVers.Link.SetObjectAndProperty(productdata, 'packageVersion');
      TIEditProdID.Link.SetObjectAndProperty(productdata, 'productId');
      TIEditProdName.Link.SetObjectAndProperty(productdata, 'productName');
      TIMemoAdvice.Link.SetObjectAndProperty(productdata, 'advice');
      TIMemoDesc.Link.SetObjectAndProperty(productdata, 'description');
      TICheckBoxlicenseRequired.Link.SetObjectAndProperty(productdata,
        'licenserequired');
      //TIGridDep.ListObject := dependencies;
      TITrackBarPrio.Link.SetObjectAndProperty(productdata, 'priority');
      TISpinEditPrio.Link.SetObjectAndProperty(productdata, 'priority');
      // definition of class TProductData in unit osdbasedata line ~256
      TILabelDirSelIcon.Link.SetObjectAndProperty(osdbasedata.aktproduct.productdata,
        'productImageFullFileName');
      TIGrid2.ListObject := dependencies;
    end;
    TIEditworkbenchpath.Link.SetObjectAndProperty(myconfiguration, 'workbench_path');
    case myconfiguration.CreateRadioIndex of
      0: RadioButtonCreateOnly.Checked := True;
      1: RadioButtonBuildPackage.Checked := True;
      2: RadioButtonPackageBuilder.Checked := True;
    end;
    case myconfiguration.BuildRadioIndex of
      0: radioBuildModebuildOnly.Checked := True;
      1: radioBuildModebuildInstall.Checked := True;
    end;
    (*
    CheckBoxQuiet.Checked := myconfiguration.CreateQuiet;
    CheckBoxBuild.Checked := myconfiguration.CreateBuild;
    CheckBoxInstall.Checked := myconfiguration.CreateInstall;
    *)
    Visible := True;
    TabSheetStart.ImageIndex := 0;
    TabSheetAnalyze.ImageIndex := 1;
    TabSheetSetup1.ImageIndex := 2;
    TabSheetSetup2.ImageIndex := 2;
    TabSheetSetup3.ImageIndex := 2;
    TabSheetProduct.ImageIndex := 3;
    TabSheetProduct2.ImageIndex := 3;
    TabSheetIcons.ImageIndex := 3;
    TabSheetCreate.ImageIndex := 4;
    TimerFirstconfig.Enabled := True;
    if fileexists(myconfiguration.PathToOpsiPackageBuilder) then
    begin
      RadioButtonBuildPackage.Enabled := True;
      RadioButtonPackageBuilder.Enabled := True;
      CheckGroupBuildMode.Enabled := True;
    end
    else
    begin
      RadioButtonBuildPackage.Enabled := False;
      RadioButtonPackageBuilder.Enabled := False;
      CheckGroupBuildMode.Enabled := False;
    end;
    {$IFDEF LINUX}
    //BtSingleAnalyzeAndCreateWin.Glyph.LoadFromFile('/usr/share/opsi-setup-detector-experimental/analyze4.xpm');
    //BtATwonalyzeAndCreate.Glyph.LoadFromFile('/usr/share/opsi-setup-detector-experimental/analyze5.xpm');
    {$ENDIF LINUX}
    LabelLogInfo.Caption := 'More info in Log file: ' + LogDatei.FileName;
    Application.ProcessMessages;
  end;
  LogDatei.log('Finished initGUI ... ', LLInfo);
end;

procedure TResultform1.FormDestroy(Sender: TObject);
begin
  TIEditSetupfile1.Link.TIObject := nil;
  TIEditSetupFile2.Link.TIObject := nil;
  //TIComboBoxInstaller1.Link.TIObject := nil;
  TILabelInstaller1.Link.TIObject := nil;
  TILabelInstaller2.Link.TIObject := nil;
  //TIComboBoxInstaller2.Link.TIObject := nil;
  //TIComboBoxArch1.Link.TIObject := nil;
  //TIComboBoxArch2.Link.TIObject := nil;
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
end;


function checkAktProduct: boolean;
begin
  Result := True;
  if not Assigned(osdbasedata.aktProduct.SetupFiles[0]) then
  begin
    LogDatei.log('Error: setupfile1 not initalized', LLCritical);
    Result := False;
  end;
  if not Assigned(osdbasedata.aktProduct.SetupFiles[1]) then
  begin
    LogDatei.log('Error: setupfile2 not initalized', LLCritical);
    Result := False;
  end;
  if not Assigned(osdbasedata.aktProduct.productdata) then
  begin
    LogDatei.log('Error: productdata not initalized', LLCritical);
    Result := False;
  end;
  if Result = False then
  begin
    LogDatei.Close;
    Application.Terminate;
  end;
end;


{$IFDEF WINDOWS}
function GetSystemDefaultLocale(const typeOfValue: DWord): string;
  // possible values: cf. "Locale Types" in windows.pas
var
  buffer: PChar;
  size: word = 0;
  usedsize: word = 0;

begin
  Result := '';
  size := 101;
  Buffer := StrAlloc(101);
  usedsize := GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, typeOfValue, buffer, size);
  if usedsize <> 0 then
    Result := StrPas(Buffer);
end;

{$ENDIF WINDOWS}

procedure main1;
var
  //ErrorMsg: string;
  //FileVerInfo: TFileVersionInfo;
  lfilename: string;
begin
  //startupfinished := true; //avoid calling main on every show event
  (*
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
  *)
  (*
  // Initialize logging
  LogDatei := TLogInfo.Create;
  lfilename := ExtractFileName(Application.ExeName);
  lfilename := ExtractFileNameOnly(lfilename);
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.WritePartLog := False;
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
  LogDatei.log('Log for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now), LLEssential);
  LogDatei.log('opsi-setup-detector Version: ' + myVersion, LLEssential);
  LogDatei.LogLevel := 8;
  *)
end;

procedure main2;
var
  ErrorMsg: string;
  i: integer;
  mylang: string;
  myparamstring: string;
  myparamcount: integer;
  allowedOS: TStringList;
begin
  startupfinished := True; //avoid calling main on every show event

  // initialize language
  mylocaledir := '';
  {$IFDEF DARWIN}
  mylocaledir := ExtractFileDir(Application.ExeName) + PathDelim +
    '../Resources/locale';
  {$ENDIF DARWIN}
  {$IFDEF LINUX}
  mylocaledir := ExtractFileDir(Application.ExeName) + PathDelim + 'locale';
  if not DirectoryExists(mylocaledir) then
    mylocaledir := '';
  {$ENDIF LINUX}
  mylang := GetDefaultLang;
  SetDefaultLang(mylang, mylocaledir);

  //check parameters
  myparamcount := ParamCount;
  myparamcount := Application.ParamCount;
  //writeln('paramcount = '+inttostr(myparamcount));
  for i := 1 to myparamcount do
    myparamstring := myparamstring + ' ' + Application.Params[i];
  LogDatei.log('Called as: ' + myparamstring, LLEssential);
  myExeDir := ExtractFileDir(ParamStr(0));
  myexitcode := 0;
  myerror := '';
  showgui := True;
  osdsettings.runmode := gmUnknown;
  opsitmp := GetTempDir(False) + 'opsitmp' + PathDelim;
  optionlist := TStringList.Create;
  optionlist.Append('help');
  optionlist.Append('filename::');
  optionlist.Append('nogui');
  optionlist.Append('lang::');
  optionlist.Append('targetOS::');
  optionlist.Append('productid::');

  // quick check parameters
  ErrorMsg := Application.CheckOptions('hfnltp', optionlist);
  if ErrorMsg <> '' then
  begin
    LogDatei.log('Exception while handling parameters.', LLcritical);
    ErrorMsg := ErrorMsg + ' with params: ' + myparamstring;
    LogDatei.log(ErrorMsg, LLcritical);
    Application.ShowException(Exception.Create(ErrorMsg));
    Application.Terminate;
    Exit;
  end;

  initaktproduct;
  if not checkAktProduct then
  begin
    Application.ShowException(Exception.Create('aktproduct not initialized'));
    Application.Terminate;
    Exit;
  end;

  // parse parameters
  if Application.HasOption('h', 'help') then
  begin
    WriteHelp;
    Application.Terminate;
    Exit;
  end;



  if Application.HasOption('l', 'lang') then
  begin
    LogDatei.log('Found Parameter lang', LLInfo);
    mylang := Application.GetOptionValue('l', 'lang');
    SetDefaultLang(mylang, mylocaledir);
    LogDatei.log('Found Parameter lang: ' + mylang, LLInfo);
    LogDatei.log('Active lang: ' + mylang, LLInfo);
  end
  else
  begin
    mylang := GetDefaultLang;
    {$IFDEF WINDOWS}
    if Mylang = '' then
      mylang := LowerCase(copy(GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME), 1, 2));
    {$ENDIF WINDOWS}
    SetDefaultLang(mylang, mylocaledir);
    LogDatei.log('Detected default lang: ' + mylang, LLInfo);
    LogDatei.log('Detected default lang: ' + GetDefaultLang, LLInfo);
  end;


  if Application.HasOption('n', 'nogui') then
    showgui := False;

  if showgui then
  begin
    //FOSDConfigdlg := TFOSDConfigdlg.Create(resultForm1);
  end;

  if Application.HasOption('t', 'targetOS') then
  begin
    forceTargetOS := lowercase(trim(Application.GetOptionValue('t', 'targetOS')));
    allowedOS := TStringList.Create;
    allowedOS.CommaText := 'win,lin,mac';
    if allowedOS.IndexOf(forceTargetOS) = -1 then
    begin
      myerror := 'Error: Given targetOS: ' + forceTargetOS +
        ' is not valid. Should be on of win,lin,mac';
      writeln(myerror);
      LogDatei.log(myerror, LLCritical);
      WriteHelp;
      Application.Terminate;
      Exit;
    end;
    FreeAndNil(allowedOS);
  end;

  if Application.HasOption('p', 'productId') then
  begin
    forceProductId := trim(Application.GetOptionValue('p', 'productId'));
    LogDatei.log('Will use as productId: ' + forceProductId, LLInfo);
  end;

  if Application.HasOption('f', 'filename') then
  begin
    myfilename := trim(Application.GetOptionValue('f', 'filename'));
    if not FileExists(myfilename) then
    begin
      myerror := 'Error: Given filename: ' + myfilename + ' does not exist.';
      LogDatei.log(myerror, LLCritical);
      WriteHelp;
    end;
    LogDatei.log('Got command line parameter filename with existing: ' +
      myfilename, LLInfo);
    if showgui then
    begin
      LogDatei.log('Start GUI mode: ', LLInfo);
      initGUI;
      with resultform1 do
      begin
        Show;
        osdsettings.runmode := singleAnalyzeCreate;
        setRunMode;
        resultform1.MemoAnalyze.Clear;
        PageControl1.ActivePage := TabSheetAnalyze;
        LogDatei.log('Wait for GUI  ', LLInfo);
        repeat
          Application.ProcessMessages;
          sleep(100);
        until (resultform1.Showing = True) and startupfinished;
      end;
      initaktproduct;
      resultform1.makeProperties;
      LogDatei.log('Start Analyze in GUI mode: ', LLInfo);
      Analyze(myfilename, aktProduct.SetupFiles[0], True);
    end
    else
    begin
      LogDatei.log('Start Analyze in NOGUI mode: ', LLInfo);
      Analyze(myfilename, aktProduct.SetupFiles[0], False);
      //analyze_binary(myfilename, False, False, aktProduct.SetupFiles[0]);
    end;
  end
  else
  begin
    if showgui then
    begin
      LogDatei.log('Start GUI mode: ', LLInfo);
      initGUI;
    end
    else
    begin
      myerror := 'Error: No filename given but nogui';
      LogDatei.log(myerror, LLCritical);
      WriteHelp;
    end;
  end;

  // stop program loop
  if not showgui then
  begin
    resultForm1.Destroy;
    freebasedata;
    Application.Terminate;
  end;
  LogDatei.log('Finished main2 ', LLInfo);
end;



{ TResultform1 }

procedure TResultform1.setRunMode;
begin
  case osdsettings.runmode of
    analyzeOnly:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := True;
      TabSheetSetup1.Enabled := True;
      TabSheetSetup2.Enabled := False;
      TabSheetProduct.Enabled := False;
      TabSheetProduct2.Enabled := False;
      TabSheetCreate.Enabled := False;
      //BtAnalyzeNextStep.Caption:='Next Step';
      //BtAnalyzeNextStep.Glyph.LoadFromResourceName();
      BtSetup1NextStep.Enabled := False;
    end;
    singleAnalyzeCreate:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := True;
      TabSheetSetup1.Enabled := True;
      TabSheetSetup2.Enabled := False;
      TabSheetProduct.Enabled := True;
      TabSheetProduct2.Enabled := True;
      TabSheetCreate.Enabled := True;
      //BtAnalyzeNextStep.Caption:='Next Step';
      //BtAnalyzeNextStep.Glyph.LoadFromResourceName();
      BtSetup1NextStep.Enabled := True;
    end;
    twoAnalyzeCreate_1, twoAnalyzeCreate_2:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := True;
      TabSheetSetup1.Enabled := True;
      TabSheetSetup2.Enabled := True;
      TabSheetProduct.Enabled := True;
      TabSheetProduct2.Enabled := True;
      TabSheetCreate.Enabled := True;
      BtSetup1NextStep.Enabled := True;
    end;
    createTemplate:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := False;
      TabSheetSetup1.Enabled := False;
      TabSheetSetup2.Enabled := False;
      TabSheetProduct.Enabled := True;
      TabSheetProduct2.Enabled := True;
      TabSheetCreate.Enabled := True;
    end;
    gmUnknown:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := True;
      TabSheetSetup1.Enabled := True;
      TabSheetSetup2.Enabled := True;
      TabSheetProduct.Enabled := True;
      TabSheetProduct2.Enabled := True;
      TabSheetCreate.Enabled := True;
      BtSetup1NextStep.Enabled := True;
    end;
  end;
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
  if not startupfinished then
    main2;
end;

procedure TResultform1.MenuItemOpenProjClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 8;   // project file
  if OpenDialog1.Execute then
  begin
    initaktproduct;
    aktProduct.readProjectFile(OpenDialog1.FileName);
    LogDatei.log('Read Project file from: ' + OpenDialog1.FileName, LLnotice);
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
  SetDefaultLang('de', mylocaledir);
end;

procedure TResultform1.MenuItemLangEnClick(Sender: TObject);
begin
  SetDefaultLang('en', mylocaledir);
end;

procedure TResultform1.MenuItemLangEsClick(Sender: TObject);
begin
  SetDefaultLang('es', mylocaledir);
end;

procedure TResultform1.MenuItemLangFrClick(Sender: TObject);
begin
  SetDefaultLang('fr', mylocaledir);
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
  //FOSDConfigdlg.Hide;
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
    RadioButtonBuildPackage.Enabled := True;
    RadioButtonPackageBuilder.Enabled := True;
    CheckGroupBuildMode.Enabled := True;
  end
  else
  begin
    logdatei.log('After configdialog: packagebuilder not found', LLDebug2);
    RadioButtonBuildPackage.Enabled := False;
    RadioButtonPackageBuilder.Enabled := False;
    CheckGroupBuildMode.Enabled := False;
  end;
  logdatei.log('Finished MenuItemConfigClick', LLDebug2);
end;

procedure TResultform1.MenuItemKnownInstallersClick(Sender: TObject);
var
  installerstr: string;
  installer: TKnownInstaller;
begin
  for installer := Low(TKnownInstaller) to High(TKnownInstaller) do
    if not (installer = stUnknown) then
      installerstr := installerstr + installerToInstallerstr(installer) + LineEnding;
  ShowMessage(installerstr);

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


procedure TResultform1.BtSingleAnalyzeAndCreateWinClick(Sender: TObject);
var
  i: integer;
  localTOSset: TTargetOSset;
begin
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    osdsettings.runmode := singleAnalyzeCreate;
    setRunMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    //aktProduct.targetOS := osWin;
    localTOSset := aktProduct.productdata.targetOSset;
    Include(localTOSset, osWin);
    aktProduct.productdata.targetOSset := localTOSset;
    aktProduct.SetupFiles[0].targetOS := osWin;
    //TIProgressBarAnalyze_progress.Link.SetObjectAndProperty(aktProduct.SetupFiles[0], 'analyze_progress');
    //TIProgressBarAnalyze_progress.Loaded;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    (*
    if MessageDlg(sMBoxHeader, rsCopyCompleteDir, mtConfirmation,
      [mbNo, mbYes], 0, mbNo) = mrYes then
      aktProduct.SetupFiles[0].copyCompleteDir := True;
      *)
    aktProduct.SetupFiles[0].copyCompleteDir := showCompleteDirDlg;
    makeProperties;
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
  // chess background as no background
  ChessColors: array[0..1] of TColor = (clMedGray, clSilver);
  picturesize: integer;
begin
  with TIImageIconPreview.Canvas do
  begin
    // paint chess background
    for row := 0 to 9 do
    begin
      for col := 0 to 9 do
      begin
        // paint chess squares
        Brush.Color := ChessColors[(row + col) mod 2];
        FillRect(Rect(22 * row, 22 * col, 22 * row + 22, 22 * col + 22));
      end;
    end;
    // paint chess board
    //RectBackgr := Rect(0, 0, ImageIconPreview.Width, ImageIconPreview.Height);
    picturesize := TIImageIconPreview.Width;
    //picturesize := round(picturesize * (Screen.PixelsPerInch / 91));
    {$IFDEF LINUX}
    // scale rect
    picturesize := round(picturesize * (91 / Screen.PixelsPerInch));
    {$ENDIF LINUX}
    RectBackgr := Rect(0, 0, picturesize, picturesize);
    // paint icon on chess board
    StretchDraw(RectBackgr, Image.Picture.Bitmap);
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

procedure TResultform1.CheckBoxDefaultIconChange(Sender: TObject);
var
  DefaultIcon: TImage;
  defaultIconFullFileName: string;
begin
  if CheckBoxDefaultIcon.Checked = True then
  begin
    { show name 'default icon' but no directory }
    LabelIconName.Visible := True;
    LabelNameSelIcon.Caption := rsDefaultIcon;
    LabelIconDir.Visible := False;
    TILabelDirSelIcon.Visible := False;
    (*
    { set productImageFullFileName to full file name of the default icon }
    {$IFDEF WINDOWS}
    defaultIconFullFileName :=
      ExtractFileDir(Application.Params[0]) + PathDelim + 'template-files' +
      PathDelim + 'template.png';
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
    defaultIconFullFileName := '/usr/share/opsi-setup-detector' +
      PathDelim + 'template-files' + PathDelim + 'template.png';
    {$ENDIF UNIX}
    osdbasedata.aktProduct.productdata.productImageFullFileName :=
      defaultIconFullFileName;
      *)

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
  myprop: TStringList;
  index: integer;
  i: integer;
  localTOSset: TTargetOSset;
begin
  MessageDlg(rsTwonalyzeAndCreateMsgHead, rsTwonalyzeAndCreateMsgFirstSetup,
    mtInformation, [mbOK], '');
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    osdsettings.runmode := twoAnalyzeCreate_1;
    setRunMode;
    (*
    if MessageDlg(sMBoxHeader, rsCopyCompleteDir, mtConfirmation,
      [mbYes, mbNo], 0, mbNo) = mrYes then
      aktProduct.SetupFiles[0].copyCompleteDir := True;   *)
    aktProduct.SetupFiles[0].copyCompleteDir := showCompleteDirDlg;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    //if StringGridProp.RowCount > 1 then
    //  for i := StringGridProp.RowCount-1 downto 1 do StringGridProp.DeleteRow(i);
    Application.ProcessMessages;
    initaktproduct;
    //aktProduct.targetOS := osWin;
    localTOSset := aktProduct.productdata.targetOSset;
    Include(localTOSset, osWin);
    aktProduct.productdata.targetOSset := localTOSset;
    aktProduct.SetupFiles[0].targetOS := osWin;
    (* moved to makeProperties
    // start add property
    index := StringGridProp.RowCount;
    Inc(index);
    StringGridProp.RowCount := index;
    myprop := TStringList.Create;
    myprop.Add(IntToStr(index - 1));
    myprop.Add('install_architecture');
    myprop.Add('Which architecture (32 / 64 Bit) has to be installed?');
    myprop.Add('unicode');  //type
    myprop.Add('False');      //multivalue
    myprop.Add('False');      //editable
    myprop.Add('["32 only","64 only","system specific","both"]');      //possible values
    myprop.Add('["system specific"]');      //default values
    StringGridProp.Rows[index - 1].AddStrings(myprop);
    myprop.Free;
    *)
    // start add property
    makeProperties;
    aktProduct.SetupFiles[0].active := True;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;

procedure TResultform1.BtCreateEmptyTemplateMultiClick(Sender: TObject);
begin
  begin
    osdsettings.runmode := createMultiTemplate;
    setRunMode;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    PageControl1.ActivePage := resultForm1.TabSheetProduct;
    Application.ProcessMessages;
    initaktproduct;
    makeProperties;
    aktProduct.productdata.targetOSset := [osWin, osLin, osMac, osMulti];
    aktProduct.productdata.productId := 'opsi-template';
    aktProduct.productdata.productName := 'opsi template for multi platform';
    aktProduct.productdata.productversion := '1.0.0';
    aktProduct.productdata.packageversion := 1;
    aktProduct.productdata.description :=
      'A template for opsi products for Win, Lin, Mac';
  end;
end;

procedure TResultform1.showCheckEntriesWarning;
var
  tmpbool: boolean;
begin
  if myconfiguration.ShowCheckEntryWarning then
  begin
    (*
    FCheckenties.ShowModal;
    myconfiguration.ShowCheckEntryWarning :=
      not FCheckenties.CheckBoxDoNotShowCheckEntries.Checked;
    *)
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
    singleAnalyzeCreate:
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
    gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup1NextStepClick osdsettings.runmode: gmUnknown',
        LLError);
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
  mydep: TStringList;
  index: integer;
begin
  FNewDepDlg.ComboBoxActState.Text := '';
  FNewDepDlg.RadioButtonState.Checked := True;
  FNewDepDlg.RadioButtonActionChange(Sender);
  procmess;
  if FNewDepDlg.ShowModal = mrOk then
  begin
    // add
    index := StringGridDep.RowCount;
    Inc(index);
    StringGridDep.RowCount := index;
    mydep := TStringList.Create;
    mydep.Add(IntToStr(index - 1));
    mydep.Add(FNewDepDlg.Editproductid.Text);
    if FNewDepDlg.RadioButtonAction.Checked then
    begin
      mydep.Add('');
      mydep.Add(FNewDepDlg.ComboBoxActState.Text);
    end
    else
    begin
      mydep.Add(FNewDepDlg.ComboBoxActState.Text);
      mydep.Add('');
    end;
    mydep.Add(FNewDepDlg.ComboBoxReqType.Text);
    //StringGridDep.Rows[index - 1].AddStrings(mydep);
    StringGridDep.Rows[index - 1].SetStrings(mydep);
    StringGridDep.Repaint;
    procmess;
    FreeAndNil(mydep);
    fetchDepPropFromForm;
  end
  else
  begin
    // cancel add
  end;
end;

procedure TResultform1.BitBtnAddPropClick(Sender: TObject);
// add property
var
  myprop: TStringList;
  index: integer;
  i: integer;
  tmpliststr: string;
  tmpstr: string;
  exists: boolean;
  //valid : boolean;
begin
  FNewPropDlg.initFields;
  FNewPropDlg.RadioButtonPropBool.Checked := True;
  FNewPropDlg.RadioButtonPropStringChange(Sender);
  procmess;
  if FNewPropDlg.ShowModal = mrOk then
  begin
    // add
    index := StringGridProp.RowCount;
    tmpstr := lowercase(FNewPropDlg.EditPropName.Text);
    exists := False;
    for i := 0 to index - 1 do
      if lowercase(tmpstr) = lowercase(StringGridProp.Cells[1, i]) then
        exists := True;
    if exists then
      MessageDlg(rsPropEditErrorHead,
        rsPropEditErrorDoubleMsgStart + FNewPropDlg.EditPropName.Text +
        rsPropEditErrorDoubleMsgFinish,
        mtError, [mbOK], '')
    else
    begin
      Inc(index);
      StringGridProp.RowCount := index;
      myprop := TStringList.Create;
      myprop.Add(IntToStr(index - 1));
      myprop.Add(FNewPropDlg.EditPropName.Text);
      myprop.Add(FNewPropDlg.MemoDesc.Text);
      if FNewPropDlg.RadioButtonPropBool.Checked then
      begin
        myprop.Add('bool');  //type
        myprop.Add('False');      //multivalue
        myprop.Add('False');      //editable
        myprop.Add('[]');      //possible values
        myprop.Add(FNewPropDlg.ListBoxPropDefVal.Items[
          FNewPropDlg.ListBoxPropDefVal.ItemIndex]);   //default values
      end
      else
      begin
        myprop.Add('unicode');  //type
        if FNewPropDlg.CheckBoxPropMultiVal.Checked then
          myprop.Add('True')      //multivalue
        else
          myprop.Add('False');      //multivalue
        if FNewPropDlg.CheckBoxPropEdit.Checked then
          myprop.Add('True')      //editable
        else
          myprop.Add('False');      //editable
        tmpliststr := '[';
        for i := 0 to FNewPropDlg.ListBoxPropPosVal.Count - 1 do
          tmpliststr := tmpliststr + '"' + FNewPropDlg.ListBoxPropPosVal.Items[i] + '",';
        // remove trailing comma
        RemoveTrailingChars(tmpliststr, [',']);
        tmpliststr := tmpliststr + ']';
        myprop.Add(tmpliststr);      //possible values
        tmpliststr := '[';
        for i := 0 to FNewPropDlg.ListBoxPropDefVal.Count - 1 do
          if FNewPropDlg.ListBoxPropDefVal.Selected[i] then
            tmpliststr := tmpliststr + '"' +
              FNewPropDlg.ListBoxPropDefVal.Items[i] + '",';
        // remove trailing comma
        RemoveTrailingChars(tmpliststr, [',']);
        tmpliststr := tmpliststr + ']';
        myprop.Add(tmpliststr);      //default values
      end;
      StringGridProp.Rows[index - 1].SetStrings(myprop);
    end;
  end
  else
  begin
    // cancel add
  end;
end;

procedure TResultform1.BitBtnDelDepClick(Sender: TObject);
begin
  StringGridDep.DeleteRow(StringGridDep.Row);
  fetchDepPropFromForm;
end;

procedure TResultform1.BitBtnDelPropClick(Sender: TObject);
begin
  // delete property
  StringGridProp.DeleteRow(StringGridProp.Row);
end;

procedure TResultform1.BitBtnEditDepClick(Sender: TObject);
var
  //mydep: TStringList;
  y: integer;
  //aPoint: TPoint;
begin
  y := StringGridDep.Row;
  if y > 0 then
  begin
    FNewDepDlg.Editproductid.Text := StringGridDep.Cells[1, y];
    if StringGridDep.Cells[2, y] = '' then
    begin
      FNewDepDlg.RadioButtonState.Checked := False;
      FNewDepDlg.RadioButtonAction.Checked := True;
      FNewDepDlg.ComboBoxActState.Text := StringGridDep.Cells[3, y];
    end
    else
    begin
      FNewDepDlg.RadioButtonState.Checked := True;
      FNewDepDlg.RadioButtonAction.Checked := False;
      FNewDepDlg.ComboBoxActState.Text := StringGridDep.Cells[2, y];
    end;
    FNewDepDlg.RadioButtonActionChange(Sender);
    procmess;

    FNewDepDlg.ComboBoxReqType.Text := StringGridDep.Cells[4, y];
    if FNewDepDlg.ShowModal = mrOk then
    begin
      // modify
      StringGridDep.Cells[1, y] := FNewDepDlg.Editproductid.Text;
      if FNewDepDlg.RadioButtonAction.Checked then
      begin
        StringGridDep.Cells[2, y] := '';
        StringGridDep.Cells[3, y] := FNewDepDlg.ComboBoxActState.Text;
      end
      else
      begin
        StringGridDep.Cells[2, y] := FNewDepDlg.ComboBoxActState.Text;
        StringGridDep.Cells[3, y] := '';
      end;
      StringGridDep.Cells[4, y] := FNewDepDlg.ComboBoxReqType.Text;
      fetchDepPropFromForm;
    end
    else
    begin
      // cancel add
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
  //myprop: TStringList;
  //index: integer;
  i: integer;
  tmpliststr: string;
  tmpstr: string;
  errorstr: string;
  remaining: string;
  y: integer;
  //aPoint: TPoint;
begin
  FNewPropDlg.initFields;
  y := StringGridProp.Row;
  if y > 0 then
  begin
    FNewPropDlg.ListBoxPropPosVal.Clear;
    FNewPropDlg.ListBoxPropDefVal.Clear;
    FNewPropDlg.EditPropName.Text := StringGridProp.Cells[1, y];
    FNewPropDlg.MemoDesc.Text := StringGridProp.Cells[2, y];
    if StringGridProp.Cells[3, y] = 'bool' then
    begin
      FNewPropDlg.RadioButtonPropString.Checked := False;
      FNewPropDlg.RadioButtonPropBool.Checked := True;
      FNewPropDlg.RadioButtonPropStringChange(Sender);
      procmess;
      FNewPropDlg.CheckBoxPropMultiVal.Checked := False;
      FNewPropDlg.CheckBoxPropEdit.Checked := False;
      if StringGridProp.Cells[7, y] = 'True' then
        FNewPropDlg.ListBoxPropDefVal.ItemIndex := 0
      else
        FNewPropDlg.ListBoxPropDefVal.ItemIndex := 1;
    end
    else  // unicode
    begin
      FNewPropDlg.RadioButtonPropString.Checked := True;
      FNewPropDlg.RadioButtonPropBool.Checked := False;
      FNewPropDlg.RadioButtonPropStringChange(Sender);
      procmess;
      if StringGridProp.Cells[4, y] = 'True' then
        FNewPropDlg.CheckBoxPropMultiVal.Checked := True
      else
        FNewPropDlg.CheckBoxPropMultiVal.Checked := False;
      FNewPropDlg.CheckBoxPropMultiValChange(Sender);
      if StringGridProp.Cells[5, y] = 'True' then
        FNewPropDlg.CheckBoxPropEdit.Checked := True
      else
        FNewPropDlg.CheckBoxPropEdit.Checked := False;
      // pos values
      tmpliststr := StringGridProp.Cells[6, y];
      remaining := opsiunquotestr2(tmpliststr, '[]');
      while remaining <> '' do
      begin
        GetWordOrStringConstant(remaining, tmpstr, remaining, [',']);
        tmpstr := opsiunquotestr2(tmpstr, '"');
        if Skip(',', remaining, remaining, errorstr) then
        ;
        FNewPropDlg.ListBoxPropPosVal.Items.Add(tmpstr);
        FNewPropDlg.ListBoxPropDefVal.Items.Add(tmpstr);
      end;
      procmess;
      // def values
      tmpliststr := StringGridProp.Cells[7, y];
      remaining := opsiunquotestr2(tmpliststr, '[]');
      while remaining <> '' do
      begin
        GetWordOrStringConstant(remaining, tmpstr, remaining, [',']);
        tmpstr := opsiunquotestr2(tmpstr, '"');
        if Skip(',', remaining, remaining, errorstr) then
        ;
        i := FNewPropDlg.ListBoxPropDefVal.Items.IndexOf(tmpstr);
        if i > -1 then
          FNewPropDlg.ListBoxPropDefVal.Selected[i] := True;
      end;
    end;
    if FNewPropDlg.ShowModal = mrOk then
    begin
      // modify
      StringGridProp.Cells[1, y] := FNewPropDlg.EditPropName.Text;
      StringGridProp.Cells[2, y] := FNewPropDlg.MemoDesc.Text;
      if FNewPropDlg.RadioButtonPropBool.Checked then
      begin
        StringGridProp.Cells[3, y] := 'bool'; //type
        StringGridProp.Cells[4, y] := '';  //multivalue
        StringGridProp.Cells[5, y] := ''; //editable
        StringGridProp.Cells[6, y] := ''; //possible values
        StringGridProp.Cells[7, y] :=
          FNewPropDlg.ListBoxPropDefVal.Items[FNewPropDlg.ListBoxPropDefVal.ItemIndex];
        //default values
      end
      else
      begin
        StringGridProp.Cells[3, y] := 'unicode';  //type
        if FNewPropDlg.CheckBoxPropMultiVal.Checked then
          StringGridProp.Cells[4, y] := 'True'      //multivalue
        else
          StringGridProp.Cells[4, y] := 'False';      //multivalue
        if FNewPropDlg.CheckBoxPropEdit.Checked then
          StringGridProp.Cells[5, y] := 'True'      //editable
        else
          StringGridProp.Cells[5, y] := 'False';      //editable
        tmpliststr := '[';
        for i := 0 to FNewPropDlg.ListBoxPropPosVal.Count - 1 do
          tmpliststr := tmpliststr + '"' + FNewPropDlg.ListBoxPropPosVal.Items[i] + '",';
        // remove trailing comma
        RemoveTrailingChars(tmpliststr, [',']);
        tmpliststr := tmpliststr + ']';
        StringGridProp.Cells[6, y] := tmpliststr;      //possible values
        tmpliststr := '[';
        for i := 0 to FNewPropDlg.ListBoxPropDefVal.Count - 1 do
          if FNewPropDlg.ListBoxPropDefVal.Selected[i] then
            tmpliststr := tmpliststr + '"' +
              FNewPropDlg.ListBoxPropDefVal.Items[i] + '",';
        // remove trailing comma
        RemoveTrailingChars(tmpliststr, [',']);
        tmpliststr := tmpliststr + ']';
        StringGridProp.Cells[7, y] := tmpliststr;      //default values
      end;
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

end;

procedure TResultform1.BitBtnOpenFile1Click(Sender: TObject);
begin

end;

procedure TResultform1.BitBtnOpenFileClick(Sender: TObject);
begin

end;

procedure TResultform1.OpenMSTFile(var mysetup: TSetupFile);
begin
  OpenDialog1.FilterIndex := 4;
  if OpenDialog1.Execute then
  begin
    mysetup.mstFullFileName := OpenDialog1.FileName;
    mysetup.installCommandLine :=
      mysetup.installCommandLine + ' TRANSFORMS=' + '"%scriptpath%\files' +
      IntToStr(mysetup.ID) + '\' + mysetup.mstFileName + '"';
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


procedure TResultform1.BtCreateEmptyTemplateWinClick(Sender: TObject);
begin
  begin
    osdsettings.runmode := createTemplate;
    setRunMode;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    PageControl1.ActivePage := resultForm1.TabSheetProduct;
    Application.ProcessMessages;
    initaktproduct;
    makeProperties;
    aktProduct.productdata.targetOSset := [osWin];
    aktProduct.productdata.productId := 'opsi-template';
    aktProduct.productdata.productName := 'opsi template for Windows';
    aktProduct.productdata.productversion := '1.0.0';
    aktProduct.productdata.packageversion := 1;
    aktProduct.productdata.description := 'A template for opsi products for Windows';
  end;
end;

procedure TResultform1.BtCreateEmptyTemplateMacClick(Sender: TObject);
begin
  begin
    osdsettings.runmode := createTemplate;
    setRunMode;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    PageControl1.ActivePage := resultForm1.TabSheetProduct;
    Application.ProcessMessages;
    initaktproduct;
    makeProperties;
    aktProduct.productdata.targetOSset := [osMac];
    aktProduct.productdata.productId := 'm-opsi-template';
    aktProduct.productdata.productName := 'opsi template for macos';
    aktProduct.productdata.productversion := '1.0.0';
    aktProduct.productdata.packageversion := 1;
    aktProduct.productdata.description := 'A template for opsi products for MacOS';
  end;
end;

procedure TResultform1.BtCreateEmptyTemplateLinClick(Sender: TObject);
begin
  begin
    osdsettings.runmode := createTemplate;
    setRunMode;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    PageControl1.ActivePage := resultForm1.TabSheetProduct;
    Application.ProcessMessages;
    initaktproduct;
    makeProperties;
    aktProduct.productdata.targetOSset := [osLin];
    aktProduct.productdata.productId := 'l-opsi-template';
    aktProduct.productdata.productName := 'opsi template for Linux';
    aktProduct.productdata.productversion := '1.0.0';
    aktProduct.productdata.packageversion := 1;
    aktProduct.productdata.description := 'A template for opsi products for Linux';
  end;
end;


procedure TResultform1.fetchDepPropFromForm;
var
  i: integer;
  mydep: TPDependency;
  myprop: TPProperty;
  tmpstr: string;
begin
  //dependencies
  aktProduct.dependencies.Clear;
  for i := 1 to StringGridDep.RowCount - 1 do
  begin
    mydep := TPDependency(aktProduct.dependencies.add);
    mydep.init;
    //aktProduct.dependencies.;
    mydep.requProductId := StringGridDep.Cells[1, i];
    case StringGridDep.Cells[2, i] of
      '': mydep.requState := noState;
      'installed': mydep.requState := installed;
      'not installed': mydep.requState := not_installed;
      'unknown': mydep.requState := unknown;
    end;
    case StringGridDep.Cells[3, i] of
      '': mydep.requAction := noRequest;
      'setup': mydep.requAction := setup;
      'uninstall': mydep.requAction := uninstall;
      'update': mydep.requAction := TPActionRequest.update;
    end;
    case StringGridDep.Cells[4, i] of
      '': mydep.RequType := doNotMatter;
      'before': mydep.RequType := before;
      'after': mydep.RequType := after;
    end;
  end;

  //properties
  aktProduct.properties.Clear;
  for i := 1 to StringGridProp.RowCount - 1 do
  begin
    myprop := TPProperty(aktProduct.properties.add);
    myprop.init;
    //myprop.Strvalues := TStringlist.Create;
    //myprop.StrDefault := TStringlist.Create;
    myprop.Name := StringGridProp.Cells[1, i];
    myprop.description := StringGridProp.Cells[2, i];
    tmpstr := StringGridProp.Cells[3, i];
    case StringGridProp.Cells[3, i] of
      'bool': myprop.ptype := bool;
      'unicode': myprop.ptype := unicode;
    end;
    if myprop.ptype = unicode then
    begin
      tmpstr := StringGridProp.Cells[4, i];
      myprop.multivalue := StringGridProp.Cells[4, i].ToBoolean;
      tmpstr := StringGridProp.Cells[5, i];
      myprop.editable := StringGridProp.Cells[5, i].ToBoolean;
      myprop.Strvalues.Text := StringGridProp.Cells[6, i];
      myprop.StrDefault.Text := StringGridProp.Cells[7, i];
      myprop.boolDefault := False;
    end
    else
    begin
      myprop.multivalue := False;
      myprop.editable := False;
      myprop.Strvalues.Text := '';
      myprop.StrDefault.Text := BoolToStr(StringGridProp.Cells[7, i].ToBoolean, True);
      myprop.boolDefault := StringGridProp.Cells[7, i].ToBoolean;
    end;
  end;
  FlowPanel14.Caption := '';
end;


procedure TResultform1.BtCreateProductClick(Sender: TObject);
var
  radioindex: integer;
  //checkok: boolean = True;
begin
  logdatei.log('Start BtCreateProductClick', LLDebug2);
  if not DirectoryExists(myconfiguration.workbench_Path) then
  begin
    //checkok := False;
    ShowMessage(sErrPacketBaseDirNotFound);
  end;
  try
    PanelProcess.Visible := True;
    procmess;
    fetchDepPropFromForm;
    procmess;
    createProductStructure;
    procmess;
    if (not RadioButtonCreateOnly.Checked) then
      callOpsiPackageBuilder;
    procmess;
    PanelProcess.Visible := False;
    ShowMessage(sInfoFinished);
  finally
    PanelProcess.Visible := False;
    procmess;
  end;
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
  (*
  if CheckBoxQuiet.Checked then
    myconfiguration.CreateQuiet := True
  else
    myconfiguration.CreateQuiet := False;
  if CheckBoxBuild.Checked then
    myconfiguration.CreateBuild := True
  else
    myconfiguration.CreateBuild := False;
  if CheckBoxInstall.Checked then
    myconfiguration.CreateInstall := True
  else
    myconfiguration.CreateInstall := False;
    *)
  logdatei.log('Finished BtCreateProductClick', LLDebug2);
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
    createMultiTemplate,
    singleAnalyzeCreate,
    twoAnalyzeCreate_1,
    twoAnalyzeCreate_2,
    threeAnalyzeCreate_1,
    threeAnalyzeCreate_2,
    threeAnalyzeCreate_3:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    (*
    singleAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    twoAnalyzeCreate_1,
    twoAnalyzeCreate_2:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    createTemplate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    *)
    gmUnknown:
    begin
      // we should never be here
      logdatei.log(
        'Error: in BtProductNextStepClick RunMode: gmUnknown', LLError);
    end;
  end;
end;

procedure TResultform1.BtProduct1NextStepClick(Sender: TObject);
var
  checkok: boolean = True;
begin
  //makeProperties;
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
      (*
      singleAnalyzeCreate:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct2;
        Application.ProcessMessages;
      end;
      *)
      createTemplate,
      createMultiTemplate,
      singleAnalyzeCreate,
      twoAnalyzeCreate_1,
      twoAnalyzeCreate_2,
      threeAnalyzeCreate_1,
      threeAnalyzeCreate_2,
      threeAnalyzeCreate_3:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct2;
        Application.ProcessMessages;
      end;
      (*
      createTemplate:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct2;
        Application.ProcessMessages;
      end;
      *)
      gmUnknown:
      begin
        // we should never be here
        logdatei.log(
          'Error: in BtProductNextStepClick RunMode: gmUnknown', LLError);
      end;
    end;
  end;
end;

procedure TResultform1.BtProduct2NextStepClick(Sender: TObject);
begin
  case osdsettings.runmode of
    analyzeOnly:
    begin
      // we should never be here
      logdatei.log('Error: in BtProductNextStepClick RunMode: analyzeOnly', LLError);
    end;
    createTemplate,
    createMultiTemplate,
    singleAnalyzeCreate,
    twoAnalyzeCreate_1,
    twoAnalyzeCreate_2,
    threeAnalyzeCreate_1,
    threeAnalyzeCreate_2,
    threeAnalyzeCreate_3:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetIcons;
      Application.ProcessMessages;
    end;
    (*
    singleAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetIcons;
      Application.ProcessMessages;
    end;
    twoAnalyzeCreate_1, twoAnalyzeCreate_2:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetIcons;
      Application.ProcessMessages;
    end;
    createTemplate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetIcons;
      Application.ProcessMessages;
    end;
    *)
    gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtProductNextStepClick RunMode: gmUnknown', LLError);
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
      singleAnalyzeCreate:
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
          (*
          if MessageDlg(sMBoxHeader, rsCopyCompleteDir, mtConfirmation,
            [mbYes, mbNo], 0, mbNo) = mrYes then
            aktProduct.SetupFiles[1].copyCompleteDir := True; *)
          aktProduct.SetupFiles[1].copyCompleteDir := showCompleteDirDlg;
          PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
          MemoAnalyze.Clear;
          Application.ProcessMessages;
          aktProduct.SetupFiles[1].active := True;
          Analyze(OpenDialog1.FileName,
            aktProduct.SetupFiles[1], True);
          SetTICheckBoxesMST(aktProduct.SetupFiles[1].installerId);
        end;
        //PageControl1.ActivePage := resultForm1.TabSheetSetup2;
        //Application.ProcessMessages;
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
            (*
            if MessageDlg(sMBoxHeader, rsCopyCompleteDir, mtConfirmation,
              [mbYes, mbNo], 0, mbNo) = mrYes then
              aktProduct.SetupFiles[1].copyCompleteDir := True; *)
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
      createTemplate:
      begin
        // we should never be here
        logdatei.log(
          'Error: in BtSetup1NextStepClick RunMode: createTemplate', LLError);
        //PageControl1.ActivePage := resultForm1.TabSheetSetup1;
        //Application.ProcessMessages;
      end;
      gmUnknown:
      begin
        // we should never be here
        logdatei.log(
          'Error: in BtSetup1NextStepClick RunMode: gmUnknown', LLError);
      end;
    end;
  end;
end;

procedure TResultform1.BtSetup2NextStepClick(Sender: TObject);
var
  checkok, isapp, goon: boolean;
  filename: string;
  localTOSset: TTargetOSset;
  installerselected: boolean = False;
  //macosOpendlg: TSelectFileOrDirectoryDialog;
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
      analyzeOnly:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode: analyzeOnly', LLError);
      end;
      singleAnalyzeCreate:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode: singleAnalyzeCreate',
          LLError);
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
          (*
          goon := False;
          isapp := False;
          macosOpendlg := TSelectFileOrDirectoryDialog.Create(self);
          //macosOpendlg.InitialDir := filename;
           macosOpendlg.Filter:= OpenDialog1.Filter;
            macosOpendlg.FilterIndex := 5;   // macos
            macosOpendlg.Options:= [ofPathMustExist,ofEnableSizing,ofViewDetail,ofAllowMultiSelect,ofNoValidate];
            if macosOpendlg.Execute then
            begin
              if macosOpendlg.Files.Count >0 then
              begin
              filename := macosOpendlg.Files[0];
              goon := True;
              if lowercase(ExtractFileExt(filename)) = lowercase('.app')
              then isapp := true;

              end
              else goon := false;
            end
            else
              goon := False;
            FreeAndNil(macosOpendlg);
            *)
          (*
          if SelectDirectoryDialog1.Execute then
          begin
            goon := True;
            filename := SelectDirectoryDialog1.FileName;
            if ExtractFileExt(filename) = '.app' then
            begin
              isapp := True;
              aktProduct.SetupFiles[2].copyCompleteDir := True;
            end;
          end;
          if goon and not isapp then
          begin
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
          *)
          if goon then
          begin
            if not isapp then
            (*
              if MessageDlg(sMBoxHeader, rsCopyCompleteDir, mtConfirmation,
                [mbYes, mbNo], 0, mbNo) = mrYes then
                aktProduct.SetupFiles[2].copyCompleteDir := True; *)
              // no showCompleteDirDlg for macos
              //aktProduct.SetupFiles[2].copyCompleteDir := showCompleteDirDlg;
              aktProduct.SetupFiles[2].copyCompleteDir := False;
            PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
            MemoAnalyze.Clear;
            Application.ProcessMessages;
            //aktProduct.SetupFiles[2] := osMac;
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
      createTemplate:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup1NextStepClick RunMode: createTemplate', LLError);
        //PageControl1.ActivePage := resultForm1.TabSheetSetup1;
        //Application.ProcessMessages;
      end;
      gmUnknown:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode: gmUnknown', LLError);
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
      analyzeOnly:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode: analyzeOnly', LLError);
      end;
      singleAnalyzeCreate:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode: singleAnalyzeCreate',
          LLError);
      end;
      twoAnalyzeCreate_2:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode: singleAnalyzeCreate',
          LLError);
      end;
      threeAnalyzeCreate_3:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct;
        Application.ProcessMessages;
      end;
      createTemplate:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup1NextStepClick RunMode: createTemplate', LLError);
        //PageControl1.ActivePage := resultForm1.TabSheetSetup1;
        //Application.ProcessMessages;
      end;
      gmUnknown:
      begin
        // we should never be here
        logdatei.log('Error: in BtSetup2NextStepClick RunMode: gmUnknown', LLError);
      end;
    end;
  end;
end;

procedure TResultform1.BtSingleAnalyzeAndCreateLinClick(Sender: TObject);
var
  i: integer;
  filename: string;
  goon: boolean;
  isapp: boolean;
  localTOSset: TTargetOSset;
begin
  goon := False;
  isapp := False;

  OpenDialog1.InitialDir := filename;
  ;
  OpenDialog1.FilterIndex := 6;   // linux
  if OpenDialog1.Execute then
  begin
    filename := OpenDialog1.FileName;
    goon := True;
  end
  else
    goon := False;

  if goon then
  begin
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
    //TIProgressBarAnalyze_progress.Link.SetObjectAndProperty(aktProduct.SetupFiles[0], 'analyze_progress');
    //TIProgressBarAnalyze_progress.Loaded;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    // we expect deb, rpm so we do not ask for complete directories
    (*
    if MessageDlg(sMBoxHeader, rsCopyCompleteDir, mtConfirmation,
      [mbNo, mbYes], 0, mbNo) = mrYes then
      aktProduct.SetupFiles[0].copyCompleteDir := True;
    *)
    makeProperties;
    Application.ProcessMessages;
    aktProduct.SetupFiles[0].active := True;
    AnalyzeLin(FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;




procedure TResultform1.BtSingleAnalyzeAndCreateMacClick(Sender: TObject);
var
  i: integer;
  filename: string;
  goon: boolean;
  isapp: boolean;
  localTOSset: TTargetOSset;
begin
  goon := False;
  isapp := False;
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
    osdsettings.runmode := singleAnalyzeCreate;
    setRunMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    //aktProduct.targetOS := osMac;
    localTOSset := aktProduct.productdata.targetOSset;
    Include(localTOSset, osMac);
    aktProduct.productdata.targetOSset := localTOSset;
    aktProduct.SetupFiles[0].targetOS := osMac;
    //TIProgressBarAnalyze_progress.Link.SetObjectAndProperty(aktProduct.SetupFiles[0], 'analyze_progress');
    //TIProgressBarAnalyze_progress.Loaded;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    (*
    if MessageDlg(sMBoxHeader, rsCopyCompleteDir, mtConfirmation,
      [mbNo, mbYes], 0, mbNo) = mrYes then
      aktProduct.SetupFiles[0].copyCompleteDir := True; *)
    // no showCompleteDirDlg for macos
    //aktProduct.SetupFiles[0].copyCompleteDir := showCompleteDirDlg;
    aktProduct.SetupFiles[0].copyCompleteDir := False;
    makeProperties;
    Application.ProcessMessages;
    aktProduct.SetupFiles[0].active := True;
    AnalyzeMac(FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;

procedure TResultform1.BtSingleAnalyzeAndCreateMultiClick(Sender: TObject);
var
  i: integer;
  localTOSset: TTargetOSset;
  installerselected: boolean = False;
begin
  osdsettings.runmode := threeAnalyzeCreate_1;
  setRunMode;
  initaktproduct;
  aktProduct.productdata.targetOSset := [osMulti];
  if MessageDlg(rsThreeAnalyzeAndCreateMsgHead, rsThreeAnalyzeAndCreateMsgFirstSetup,
    mtConfirmation, [mbYes, mbNo], '') = mrYes then
  begin
    OpenDialog1.FilterIndex := 1;   // setup
    if OpenDialog1.Execute then
    begin
      PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
      Application.ProcessMessages;
      //initaktproduct;
      //aktProduct.targetOS := osWin;
      localTOSset := aktProduct.productdata.targetOSset;
      Include(localTOSset, osWin);
      aktProduct.productdata.targetOSset := localTOSset;
      //aktProduct.SetupFiles[0] := osWin;
      aktProduct.SetupFiles[0].targetOS := osWin;
      //TIProgressBarAnalyze_progress.Link.SetObjectAndProperty(aktProduct.SetupFiles[0], 'analyze_progress');
      //TIProgressBarAnalyze_progress.Loaded;
      MemoAnalyze.Clear;
      StringGridDep.Clean([gzNormal, gzFixedRows]);
      StringGridDep.RowCount := 1;
      (*
      if MessageDlg(sMBoxHeader, rsCopyCompleteDir, mtConfirmation,
        [mbNo, mbYes], 0, mbNo) = mrYes then
        aktProduct.SetupFiles[0].copyCompleteDir := True; *)
      aktProduct.SetupFiles[0].copyCompleteDir := showCompleteDirDlg;
      makeProperties;
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
var
  i: integer;
begin
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    osdsettings.runmode := analyzeOnly;
    setRunMode;
    MemoAnalyze.Clear;
    StringGridDep.Clean([gzNormal, gzFixedRows]);
    StringGridDep.RowCount := 1;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    makeProperties;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;




(*

procedure TResultform1.ComboBoxArchModeChange(Sender: TObject);
var
  modestr: string;
  mode: TArchitectureMode;
begin
  modestr := TComboBox(Sender).Text;
  mode := archModeStrToArchmode(modestr);
  case mode of
    am32only_fix:
    begin
      FlowPanelSetup32.Enabled := True;
    end;
    am64only_fix:
    begin
      FlowPanelSetup32.Enabled := False;
    end;
    amBoth_fix, amSystemSpecific_fix, amSelectable:
    begin
      FlowPanelSetup32.Enabled := True;
    end;
  end;
end;

procedure TResultform1.FileCreateLogfileClick(Sender: TObject);
begin

end;
*)

(*
procedure TResultform1.FileCreateLogfileClick(Sender: TObject);
var
  msg: string;
begin
  WriteLogfile;
  msg := format(sLogfileInfo, [Logfile]);
  Application.MessageBox(pchar(msg), pchar(sMBoxHeader), MB_OK);
end;
*)

procedure TResultform1.FileHelpClick(Sender: TObject);
begin
  ShowMessage(rsNotImplemented);
  (*
   FormHelp.Caption:=sHelpHeader;
   FormHelp.SetHelpFile(myExeDir + DirectorySeparator + sHelpFile);
   FormHelp.Show;
   *)
end;


procedure TResultform1.mst32NameEditChange(Sender: TObject);
begin
  (*
    OpenDialog1.FilterIndex := 3;  // MST
  if OpenDialog1.Execute then
  begin
     TFileNameEdit(sender).Text := ExtractFileName(OpenDialog1.FileName);
     TFileNameEdit(sender).Hint:=OpenDialog1.FileName;
     aktProduct.mst32FileNamePath:=OpenDialog1.FileName;
     aktProduct.mst32FileName:= ExtractFileName(OpenDialog1.FileName);
  end;
  *)
end;

procedure TResultform1.PageControl1Change(Sender: TObject);
var
  myimage: TImage;
begin
  if FileExists(aktProduct.productdata.productImageFullFileName) then
  begin
    myimage := TImage.Create(self);
    myimage.Picture.LoadFromFile(aktProduct.productdata.productImageFullFileName);
    PaintPreview(myimage);
    FreeAndNil(myimage);
  end;

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
  BtAnalyzeOnly.Click;
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
  msg := progname + ' Version: ' + myVersion;
  list.Add(msg);
  list.Add('(c) uib gmbh under AGPLv3');
  list.Add('This is a part of the opsi.org Project: https://opsi.org');
  list.Add('');
  list.add('Icons from Iconic (https://useiconic.com/) under MIT Licnse.');
  //list.add('https://github.com/iconic/open-iconic/blob/master/ICON-LICENSE');
  list.Add('');
  list.Add('Configuration: ');
  list.Add(aktconfigfile);
  list.Add('Log: ');
  list.Add(logdatei.FileName);
  ShowMessage(list.Text);
  list.Free;

  //Application.MessageBox(PChar(msg), PChar(sMBoxHeader), MB_OK);

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
        Caption := rsCopyCompleteDirCap;
        ModalResult := mrYes;
      end;
      with TTaskDialogButtonItem(Buttons.Add) do
      begin
        Caption := rsCopyFileOnlyCap;
        ModalResult := mrNo;
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

procedure TResultform1.FormCreate(Sender: TObject);
var
  DefaultIcon: TImage;
  tmpimage: TPicture;
  filename: string;
  resourcedir, templatePath: string;
begin
  tmpimage := TPicture.Create;
  loadDefaultIcon := True;
  Application.OnIdle := @ApplicationEventIdle;
  //main1;
  // TabSheetIcons presets
  BtnOpenIconFolder.Font.Size := 12;
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
    PathDelim + 'template-files' + PathDelim + 'images' + PathDelim + 'template.png');
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
  templatePath := resourcedir + PathDelim + 'template-files';
  if not DirectoryExists(templatePath) then
    //templatePath := '/usr/local/share/opsi-setup-detector/template-files';
    resourcedir := ExtractFileDir(Application.ExeName) + PathDelim +
      '../Resources';
  {$ENDIF DARWIN}
  filename := resourcedir + PathDelim + 'template-files' + PathDelim +
    'images' + PathDelim + 'template.png';
  if fileexists(filename) then
    DefaultIcon.Picture.LoadFromFile(filename)
  else
  begin
    filename := ExtractFileDir(Application.Params[0]) + PathDelim +
      'template-files' + PathDelim + 'images' + PathDelim + 'template.png';
    if fileexists(filename) then
      DefaultIcon.Picture.LoadFromFile(filename)
    else
      LogDatei.log('Could not find template.png ', LLError);
  end;
  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' +
    PathDelim + 'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateWin.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' +
    PathDelim + 'analyzepack4.xpm');
  BtATwonalyzeAndCreate.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' +
    PathDelim + 'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateLin.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' +
    PathDelim + 'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateMac.Glyph.Assign(tmpimage.Bitmap);

  tmpimage.LoadFromFile(resourcedir + PathDelim + 'images' +
    PathDelim + 'analyzepack4.xpm');
  BtSingleAnalyzeAndCreateMulti.Glyph.Assign(tmpimage.Bitmap);

  FreeAndNil(tmpimage);
  {$ENDIF UNIX}
  PaintPreview(DefaultIcon);
  DataModule1.SetFontName(TControl(Sender), myFont);
  LogDatei.log('Finished FormCreate ', LLInfo);
end;

procedure TResultform1.memoadd(line: string);
begin
  MemoAnalyze.append(line);
end;

procedure TResultform1.ApplicationEventIdle(Sender: TObject; var Done: boolean);
begin
  (*
  if not startupfinished then
    main2;
    *)
  Application.ProcessMessages;
  sleep(100);
end;




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


(*
procedure TResultform1.SBtnOpenClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    initaktproduct;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
  end;
end;
*)

procedure TResultform1.SBtnExitClick(Sender: TObject);
begin
  resultForm1.Close;
  resultForm1.Destroy;
  freebasedata;
  Application.Terminate;
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
end;

procedure TResultform1.TabSheetIconsShow(Sender: TObject);
begin

end;

procedure TResultform1.TabSheetStartExit(Sender: TObject);
begin
  //ResultForm1.Width := 1185;
  //ResultForm1.Height := 566;
end;

procedure TResultform1.TICheckBoxlicenseRequiredChange(Sender: TObject);
begin
  makeProperties;
end;

procedure TResultform1.TICheckBoxS1MstChange(Sender: TObject);
begin
  if (Sender as TTICheckBox).State = cbChecked then
  begin
    FlowPanelMst.Enabled := True;
    //TIEditMstFile1.Enabled:=true;
  end
  else
  begin
    FlowPanelMst.Enabled := False;
    //TIEditMstFile1.Enabled:=false;
  end;
end;

procedure TResultform1.TICheckBoxS2MstChange(Sender: TObject);
begin
  if (Sender as TTICheckBox).State = cbChecked then
  begin
    FlowPanelMst1.Enabled := True;
    //TIEditMstFile2.Enabled:=true
  end
  else
  begin
    FlowPanelMst1.Enabled := False;
    //TIEditMstFile2.Enabled:=false;
  end;
end;

procedure TResultform1.TIEditProdIDChange(Sender: TObject);
begin

end;

procedure TResultform1.TIEditProdVersion3Change(Sender: TObject);
begin

end;

procedure TResultform1.TIEditProdVersion3Exit(Sender: TObject);
begin

end;


procedure TResultform1.genRttiEditChange(Sender: TObject);
begin
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

procedure TResultform1.TIS1UrlClick(Sender: TObject);
var
  link: string;
  PropInfo: PPropInfo;
begin
  link := TTILabel(Sender).Caption;
  (*
  PropInfo := GetPropInfo(Sender, 'Caption', []);
  if Assigned(PropInfo) then
    link := GetStrProp(Sender, PropInfo);
    *)
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

procedure TResultform1.makeProperties;
var
  myprop: TStringList;
  index, i: integer;
begin
  // clear existing props in StringGridProp
  StringGridProp.Clean([gzNormal, gzFixedRows]);
  StringGridProp.RowCount := 1;

  if myconfiguration.UsePropDesktopicon and
    (StringGridProp.Cols[1].IndexOf('DesktopIcon') = -1) then
  begin
    index := StringGridProp.RowCount;
    //Inc(index);
    //StringGridProp.InsertColRow(false,index);
    //StringGridProp.RowCount := index;
    myprop := TStringList.Create;
    myprop.Add(IntToStr(index));
    myprop.Add('DesktopIcon');
    myprop.Add('Soll es ein Desktop Icon geben ?');
    myprop.Add('bool');  //type
    myprop.Add('False');      //multivalue
    myprop.Add('False');      //editable
    myprop.Add('[]');      //possible values
    myprop.Add('False');      //default values
    //StringGridProp.InsertRowWithValues(index,myprop);
    StringGridProp.InsertColRow(False, index);
    StringGridProp.Rows[index].Clear;
    StringGridProp.Rows[index].SetStrings(myprop);
    myprop.Free;
      (*
      myprop := TPProperty(aktProduct.properties.add);
      myprop.init;
      myprop.Name := 'DesktopIcon';
      myprop.description := 'Soll es ein Desktop Icon geben ?';
      myprop.ptype := bool;
      myprop.multivalue := False;
      myprop.editable := False;
      myprop.Strvalues.Text := '';
      myprop.StrDefault.Text := '';
      myprop.boolDefault := False;
      *)
  end;

  if myconfiguration.UsePropLicenseOrPool and
    aktProduct.productdata.licenserequired and
    (StringGridProp.Cols[1].IndexOf('LicenseOrPool') = -1) then
  begin
    index := StringGridProp.RowCount;
    //Inc(index);
    //StringGridProp.RowCount := index;
    myprop := TStringList.Create;
    myprop.Add(IntToStr(index));
    myprop.Add('SecretLicense_or_Pool');
    myprop.Add('LicenseKey or opsi-LicensePool');
    myprop.Add('unicode');  //type
    myprop.Add('False');      //multivalue
    myprop.Add('True');      //editable
    myprop.Add('[]');      //possible values
    myprop.Add('[""]');      //default values
    StringGridProp.InsertColRow(False, index);
    StringGridProp.Rows[index].Clear;
    StringGridProp.Rows[index].AddStrings(myprop);
    myprop.Free;
      (*
      myprop := TPProperty(aktProduct.properties.add);
      myprop.init;
      myprop.Name := 'LicenseOrPool';
      myprop.description := 'LicenseKey or opsi-LicensePool';
      myprop.ptype := unicode;
      myprop.multivalue := False;
      myprop.editable := True;
      myprop.Strvalues.Text := '';
      myprop.StrDefault.Text := '';
      myprop.boolDefault := False;
      *)
  end;

  if osdsettings.runmode = twoAnalyzeCreate_1 then
  begin
    index := StringGridProp.RowCount;
    //Inc(index);
    //StringGridProp.RowCount := index;
    myprop := TStringList.Create;
    myprop.Add(IntToStr(index));
    myprop.Add('install_architecture');
    myprop.Add('Which architecture (32 / 64 Bit) has to be installed?');
    myprop.Add('unicode');  //type
    myprop.Add('False');      //multivalue
    myprop.Add('False');      //editable
    myprop.Add('["32 only","64 only","system specific","both"]');
    //possible values
    myprop.Add('["system specific"]');      //default values
    StringGridProp.InsertColRow(False, index);
    StringGridProp.Rows[index].Clear;
    StringGridProp.Rows[index].AddStrings(myprop);
    myprop.Free;
  end;
end;


end.
