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
  osdanalyze,
  //winpeimagereader,
  lcltranslator,
  EditBtn,
  //Spin,
  //JSONPropStorage,
  Grids,
  PairSplitter, ColorBox,
  oslog,
  osdbasedata, osdconfigdlg, osdcreate, fpjsonrtti, osddlgnewdependency,
  osddlgnewproperty, osparserhelper,
  Contnrs;

type
  TIconDisplay = class(TObject)
    Panel: TPanel;
    Image: TImage;
    FileName: string;
  private
  public
  end;

type

  { TResultform1 }

  TResultform1 = class(TForm)
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
    BtProduct2NextStep: TBitBtn;
    BtSetup1NextStep: TBitBtn;
    BtSetup2NextStep: TBitBtn;
    BtSingleAnalyzeAndCreate: TBitBtn;
    BitBtnDefault: TBitBtn;
    BitBtnMST1: TBitBtn;
    BitBtnOpenMst1: TBitBtn;
    BitBtnOpenMst2: TBitBtn;
    BtATwonalyzeAndCreate: TBitBtn;
    BtCreateEmptyTemplate: TBitBtn;
    BtAnalyzeOnly: TBitBtn;
    BtnOpenIconFolder: TButton;
    CheckBoxBackgr: TCheckBox;
    CheckGroupBuildMode: TCheckGroup;
    ColorBoxIconBackgr: TColorBox;
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
    FlowPanel23: TFlowPanel;
    FlowPanel3: TFlowPanel;
    FlowPanel4: TFlowPanel;
    FlowPanel6: TFlowPanel;
    FlowPanelMsiId1: TFlowPanel;
    FlowPanelMST1: TFlowPanel;
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
    GroupBox2: TGroupBox;
    ImageIconPreview: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    LabelNameSelectedIcon: TLabel;
    LabelIconPreview: TLabel;
    LabelNumIcons: TLabel;
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
    PanelSelectBackgrCol: TPanel;
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
    SBtnExit: TSpeedButton;
    ScrollBox1: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    StringGridDep: TStringGrid;
    StringGridProp: TStringGrid;
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
    TIComboBoxInstaller1: TTIComboBox;
    TIComboBoxInstaller2: TTIComboBox;
    TIEditInstallDir2: TTIEdit;
    TIEditMsiId2: TTIEdit;
    TIEditMstFile2: TTIEdit;
    TIEditProdVersion1: TTIEdit;
    TIEditMsiId1: TTIEdit;
    TIEditProdVersion2: TTIEdit;
    TIEditProdVersion3: TTIEdit;
    TIEditProdID: TTIEdit;
    TIEditProdName: TTIEdit;
    TIEditRequiredSizeMB2: TTIEdit;
    TIEditSetup2Command: TTIEdit;
    TIEditSetup1UnCommand: TTIEdit;
    TIEditSetup2UnCommand: TTIEdit;
    TIEditSetup1UnProgram: TTIEdit;
    TIEditSetup2UnProgram: TTIEdit;
    TIEditSetupfile2: TTIEdit;
    TIEditSetup1Command: TTIEdit;
    TIEditworkbenchpath: TTIEdit;
    TIEditSetupFileSizeMB1: TTIEdit;
    TIEditRequiredSizeMB1: TTIEdit;
    TIEditInstallDir1: TTIEdit;
    TIEditSetupfile1: TTIEdit;
    TIEditMstFile1: TTIEdit;
    TIEditSetupFileSizeMB2: TTIEdit;
    TIMemoAdvice: TTIMemo;
    TIMemoDesc: TTIMemo;
    TimerFirstconfig: TTimer;
    TIS1Url: TTILabel;
    TIS2Url: TTILabel;
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
    procedure BtCreateEmptyTemplateClick(Sender: TObject);
    procedure BtCreateProductClick(Sender: TObject);
    procedure BtProduct1NextStepClick(Sender: TObject);
    procedure BtProduct2NextStepClick(Sender: TObject);
    procedure BtSetup1NextStepClick(Sender: TObject);
    procedure BtSetup2NextStepClick(Sender: TObject);
    procedure BtSingleAnalyzeAndCreateClick(Sender: TObject);
    procedure BtnOpenIconFolderClick(Sender: TObject);
    procedure ColorBoxIconBackgrSelect(Sender: TObject);
    procedure FlowPanel14Click(Sender: TObject);
    procedure FlowPanel18Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemLangDeClick(Sender: TObject);
    procedure MenuItemLangEnClick(Sender: TObject);
    procedure MenuItemLangEsClick(Sender: TObject);
    procedure MenuItemStartClick(Sender: TObject);
    procedure MenuItemConfigClick(Sender: TObject);
    procedure MenuItemKnownInstallersClick(Sender: TObject);
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

    procedure IconPanelOnMouseEnter(Sender: TObject);
    procedure IconImageOnMouseEnter(Sender: TObject);
    procedure PaintBackgr(SelColor: TColor);
    procedure IconImageOnClick(Sender: TObject);
    procedure IconPanelOnClick(Sender: TObject);
    procedure CheckBoxBackgrChange(Sender: TObject);
  private
    { private declarations }
    procedure OpenMSTFile(var mysetup: TSetupFile);
    procedure SetTICheckBoxesMST(Installer: TKnownInstaller);
  public
    { public declarations }

    // create a FlowPanel dynamically to be able to free it before selecting a
    //new directory
    dynIconFlowPanel: TFlowPanel;
    // TFPObjectList needs Contnrs in uses
    IconList: TFPObjectList;
    iconDirectory: string;
    numberIcons, indexSelectedIcon: integer;
    SelectedIcon: TIconDisplay;
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
  ArchitecturesInstallIn64BitMode: string; // INNO: {pf}={pf64}/{pf32}?
  //Revision: string = '$Rev: 126 $';   // set manually in Project-Settings-Version
  //RevDate: string = '$Date: 2014-09-23 17:46:39 +0200 (Di, 23 Sep 2014) $';
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
  sWarnInstalldirUnknown = 'Error: Field Install Directory is empty!' +
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
  rsPropEditErrorHead = 'opsi-setup-detector: Property Editor: Error';
  rsPropEditErrorDoubleMsgStart = 'property Id: ';
  rsPropEditErrorDoubleMsgFinish = ' exists. Duplicates not allowed.';
  rsPropEditErrorNoSelect = 'No Property selected.';
  rsDependencyEditErrorHead = 'opsi-setup-detector: Dependency Editor: Error';
  rsDependencyEditErrorNoSelect = 'No Dependency selected.';


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
  writeln(' --filename=<path\filename> -> file to analyze)');
  writeln(' --nogui -> do not show interactive output window)');
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
      TIComboBoxInstaller1.Link.SetObjectAndProperty(SetupFiles[0], 'installerid');
      TIComboBoxInstaller2.Link.SetObjectAndProperty(SetupFiles[1], 'installerid');
      TIEditSetupfile1.Link.SetObjectAndProperty(SetupFiles[0], 'setupFullFileName');
      TIEditSetupFile2.Link.SetObjectAndProperty(SetupFiles[1], 'setupFullFileName');
      //TIComboBoxArch1.Link.SetObjectAndProperty(SetupFiles[0], 'architecture');
      //TIComboBoxArch2.Link.SetObjectAndProperty(SetupFiles[1], 'architecture');
      TIEditMstFile1.Link.SetObjectAndProperty(SetupFiles[0], 'mstFullFileName');
      TIEditMstFile2.Link.SetObjectAndProperty(SetupFiles[1], 'mstFullFileName');
      TICheckBoxS1Mst.Link.SetObjectAndProperty(SetupFiles[0], 'mstAllowed');
      TICheckBoxS2Mst.Link.SetObjectAndProperty(SetupFiles[1], 'mstAllowed');
      TIEditMsiId1.Link.SetObjectAndProperty(SetupFiles[0], 'msiId');
      TIEditMsiId2.Link.SetObjectAndProperty(SetupFiles[1], 'msiId');
      TIEditProdVersion1.Link.SetObjectAndProperty(SetupFiles[0], 'SoftwareVersion');
      TIEditProdVersion2.Link.SetObjectAndProperty(SetupFiles[1], 'SoftwareVersion');
      TIEditSetupFileSizeMB1.Link.SetObjectAndProperty(SetupFiles[0], 'setupFileSize');
      TIEditSetupFileSizeMB2.Link.SetObjectAndProperty(SetupFiles[1], 'setupFileSize');
      TIEditRequiredSizeMB1.Link.SetObjectAndProperty(SetupFiles[0], 'requiredSpace');
      TIEditRequiredSizeMB2.Link.SetObjectAndProperty(SetupFiles[1], 'requiredSpace');
      TIEditInstallDir1.Link.SetObjectAndProperty(SetupFiles[0], 'installDirectory');
      TIEditInstallDir2.Link.SetObjectAndProperty(SetupFiles[1], 'installDirectory');
      TIEditSetup1Command.Link.SetObjectAndProperty(SetupFiles[0], 'installCommandLine');
      TIEditSetup2Command.Link.SetObjectAndProperty(SetupFiles[1], 'installCommandLine');
      TIEditSetup1UnCommand.Link.SetObjectAndProperty(SetupFiles[0],
        'uninstallCommandLine');
      TIEditSetup2UnCommand.Link.SetObjectAndProperty(SetupFiles[1],
        'uninstallCommandLine');
      TIEditSetup1UnProgram.Link.SetObjectAndProperty(SetupFiles[0], 'uninstallProg');
      TIEditSetup2UnProgram.Link.SetObjectAndProperty(SetupFiles[1], 'uninstallProg');
      TIS1Url.Link.SetObjectAndProperty(SetupFiles[0], 'link');
      TIS2Url.Link.SetObjectAndProperty(SetupFiles[1], 'link');
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
    TabSheetProduct.ImageIndex := 3;
    TabSheetProduct2.ImageIndex := 3;
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
    //BtSingleAnalyzeAndCreate.Glyph.LoadFromFile('/usr/share/opsi-setup-detector-experimental/analyze4.xpm');
    //BtATwonalyzeAndCreate.Glyph.LoadFromFile('/usr/share/opsi-setup-detector-experimental/analyze5.xpm');
    {$ENDIF LINUX}
    LabelLogInfo.Caption := 'More info in Log file: ' + LogDatei.FileName;
    Application.ProcessMessages;
  end;
end;

procedure TResultform1.FormDestroy(Sender: TObject);
begin
  TIEditSetupfile1.Link.TIObject := nil;
  TIEditSetupFile2.Link.TIObject := nil;
  TIComboBoxInstaller1.Link.TIObject := nil;
  TIComboBoxInstaller2.Link.TIObject := nil;
  //TIComboBoxArch1.Link.TIObject := nil;
  //TIComboBoxArch2.Link.TIObject := nil;
  TIEditMstFile1.Link.TIObject := nil;
  TIEditMstFile2.Link.TIObject := nil;
  TIEditMsiId1.Link.TIObject := nil;
  TIEditMsiId2.Link.TIObject := nil;
  TIEditProdVersion1.Link.TIObject := nil;
  TIEditProdVersion2.Link.TIObject := nil;
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
begin
  startupfinished := True; //avoid calling main on every show event
  myExeDir := ExtractFileDir(ParamStr(0));
  myexitcode := 0;
  myerror := '';
  showgui := True;
  useRunMode := gmUnknown;
  opsitmp := GetTempDir(False) + 'opsitmp' + PathDelim;
  optionlist := TStringList.Create;
  optionlist.Append('help');
  optionlist.Append('filename::');
  optionlist.Append('nogui');
  optionlist.Append('lang::');

  // quick check parameters
  ErrorMsg := Application.CheckOptions('', optionlist);
  if ErrorMsg <> '' then
  begin
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



  if Application.HasOption('lang') then
  begin
    LogDatei.log('Found Parameter lang', LLInfo);
    mylang := Application.GetOptionValue('lang');
    SetDefaultLang(mylang);
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
    SetDefaultLang(mylang);
    LogDatei.log('Detected default lang: ' + mylang, LLInfo);
    LogDatei.log('Detected default lang: ' + GetDefaultLang, LLInfo);
  end;


  if Application.HasOption('nogui') then
    showgui := False;

  if showgui then
  begin
    //FOSDConfigdlg := TFOSDConfigdlg.Create(resultForm1);
  end;

  if Application.HasOption('filename') then
  begin
    myfilename := trim(Application.GetOptionValue('filename'));
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
        useRunMode := singleAnalyzeCreate;
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
      analyze_binary(myfilename, False, False, aktProduct.SetupFiles[0]);
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
end;



{ TResultform1 }

procedure TResultform1.setRunMode;
begin
  case useRunMode of
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

end;

procedure TResultform1.MenuItemLangDeClick(Sender: TObject);
begin
  SetDefaultLang('de');
end;

procedure TResultform1.MenuItemLangEnClick(Sender: TObject);
begin
  SetDefaultLang('en');
end;

procedure TResultform1.MenuItemLangEsClick(Sender: TObject);
begin
  SetDefaultLang('es');
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


procedure TResultform1.BtSingleAnalyzeAndCreateClick(Sender: TObject);
var
  i: integer;
begin
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    useRunMode := singleAnalyzeCreate;
    setRunMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    //TIProgressBarAnalyze_progress.Link.SetObjectAndProperty(aktProduct.SetupFiles[0], 'analyze_progress');
    //TIProgressBarAnalyze_progress.Loaded;
    MemoAnalyze.Clear;
    if StringGridDep.RowCount > 1 then
      for i := StringGridDep.RowCount - 1 downto 1 do
        StringGridDep.DeleteRow(i);
    StringGridDep.Clear;
    //if StringGridProp.RowCount > 1 then
    //  for i := StringGridProp.RowCount-1 downto 1 do StringGridProp.DeleteRow(i);
    makeProperties;
    Application.ProcessMessages;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;
///////////////////////////////////////////////////////////////////////////////
procedure TResultform1.IconPanelOnMouseEnter(Sender: TObject);
var
  index: integer;
begin
  for index := 0 to (numberIcons - 1) do
  begin
    TIconDisplay(IconList.Items[index]).Panel.Color := clDefault;
  end;
  (Sender as TPanel).Color := clSkyBlue;
end;

procedure TResultform1.IconImageOnMouseEnter(Sender: TObject);
var
  index: integer;
begin
  for index := 0 to (numberIcons - 1) do
  begin
    TIconDisplay(IconList.Items[index]).Panel.Color := clDefault;
  end;
  (Sender as TImage).Parent.Color := clSkyBlue;
end;

// paint icon preview with selected background
procedure TResultform1.PaintBackgr(SelColor: TColor);
var
  RectBackgr: TRect; // rectangle as background
begin
  with ImageIconPreview.Canvas do
  begin
    Brush.Color := SelColor;
    RectBackgr := Rect(0, 0, ImageIconPreview.Width, ImageIconPreview.Height);
    FillRect(RectBackgr);
    StretchDraw(RectBackgr, SelectedIcon.Image.Picture.Bitmap);
  end;
end;

procedure TResultform1.CheckBoxBackgrChange(Sender: TObject);
begin
  if CheckBoxBackgr.Checked = True then
  begin
    PanelSelectBackgrCol.Visible := True;
    PaintBackgr(ColorBoxIconBackgr.Selected);
    //ColorDialog1.Execute;
  end
  else
  begin
    PanelSelectBackgrCol.Visible := False;
    PaintBackgr(clForm);
  end;
end;

procedure TResultform1.IconImageOnClick(Sender: TObject);
var
  index: integer;
begin
  for index := 0 to (numberIcons - 1) do
  begin
    if TIconDisplay(IconList.Items[index]).Image = (Sender as TImage) then
    begin
      indexSelectedIcon := index;
      break;
    end;
  end;
  CheckBoxBackgr.Enabled:=True;
  SelectedIcon := TIconDisplay(IconList.Items[indexSelectedIcon]);
  LabelNameSelectedIcon.Caption := SelectedIcon.FileName;
  PaintBackgr(ColorBoxIconBackgr.Selected);
end;

procedure TResultform1.IconPanelOnClick(Sender: TObject);
var
  index: integer;
begin
  for index := 0 to (numberIcons - 1) do
  begin
    if TIconDisplay(IconList.Items[index]).Panel = (Sender as TPanel) then
    begin
      indexSelectedIcon := index;
      break;
    end;
  end;
  CheckBoxBackgr.Enabled:=True;
  SelectedIcon := TIconDisplay(IconList.Items[indexSelectedIcon]);
  LabelNameSelectedIcon.Caption := SelectedIcon.FileName;
  PaintBackgr(ColorBoxIconBackgr.Selected);
end;

procedure TResultform1.BtnOpenIconFolderClick(Sender: TObject);
var
  selectDirectory, loadFromFile: boolean;
  Background: TPanel;
  Image: TImage;
  IconDisplay: TIconDisplay;
  IconSearch: TSearchRec;
begin
  // clear list for new directory
  IconList.Free;
  dynIconFlowPanel.Free;

  dynIconFlowPanel := TFlowPanel.Create(TabSheetIcons);
  dynIconFlowPanel.Parent := ScrollBox1;
  dynIconFlowPanel.Align := TAlign.alClient;

  // select new directory the icons shall come from
  try
    selectDirectory := SelectDirectoryDialog1.Execute;
    iconDirectory := SelectDirectoryDialog1.FileName + '/';
  except
    on E: Exception do
    begin
      ShowMessage('Exception while preview directory: ' + E.Message);
      selectDirectory := False;
    end;
  end;
  // fill the FlowPanel and store the icon display infos in IconList
  if selectDirectory then
  begin
    IconList := TFPObjectList.Create;
    numberIcons := 0;
    // get icons from the selected directory
    if FindFirst(iconDirectory + '*.png', faAnyFile, IconSearch) = 0 then
    begin
      repeat
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
        Background.OnMouseEnter := @IconPanelOnMouseEnter;
        Background.OnClick := @IconPanelOnClick;
        // image properties
        Image := TImage.Create(TabSheetIcons);
        Image.Parent := Background;
        Image.Stretch := True;
        Image.BorderSpacing.Around := 5;
        Image.Height := 50;
        Image.Width := 50;
        // image procedures
        Image.OnMouseEnter := @IconImageOnMouseEnter;
        Image.OnClick := @IconImageOnClick;

        // now load icon into Image
        loadFromFile := True;
        try
          // IconSearch.Name = name of icon file as string
          Image.Picture.LoadFromFile(iconDirectory + IconSearch.Name);
        except
          on E: Exception do
          begin
            ShowMessage('Exception while load from file: ' + E.Message);
            loadFromFile := False;
            Image.Destroy;
            Background.Destroy;
          end;
        end;
        if loadFromFile then
        begin
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
      until FindNext(IconSearch) <> 0; // I have no idea how they are ordered
      FindClose(IconSearch);
    end;
    LabelNumIcons.Caption := 'Icons zur Auswahl: ' + IntToStr(numberIcons);
  end;
end;

procedure TResultform1.ColorBoxIconBackgrSelect(Sender: TObject);
begin
    PaintBackgr(ColorBoxIconBackgr.Selected);
end;

///////////////////////////////////////////////////////////////////////////////

procedure TResultform1.FlowPanel14Click(Sender: TObject);
begin

end;

procedure TResultform1.FlowPanel18Click(Sender: TObject);
begin

end;

procedure TResultform1.FormActivate(Sender: TObject);
begin
    BtnOpenIconFolder.Font.Size:=12;
end;

procedure TResultform1.BtATwonalyzeAndCreateClick(Sender: TObject);
var
  myprop: TStringList;
  index: integer;
  i: integer;
begin
  MessageDlg(rsTwonalyzeAndCreateMsgHead, rsTwonalyzeAndCreateMsgFirstSetup,
    mtInformation, [mbOK], '');
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    useRunMode := twoAnalyzeCreate_1;
    setRunMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    MemoAnalyze.Clear;
    StringGridDep.Clear;
    if StringGridDep.RowCount > 1 then
      for i := StringGridDep.RowCount - 1 downto 1 do
        StringGridDep.DeleteRow(i);
    //if StringGridProp.RowCount > 1 then
    //  for i := StringGridProp.RowCount-1 downto 1 do StringGridProp.DeleteRow(i);
    Application.ProcessMessages;
    initaktproduct;
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
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
    SetTICheckBoxesMST(aktProduct.SetupFiles[0].installerId);
  end;
end;

procedure TResultform1.BtAnalyzeNextStepClick(Sender: TObject);
begin
  case useRunMode of
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

    createTemplate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      Application.ProcessMessages;
    end;
    gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup1NextStepClick RunMode: gmUnknown', LLError);
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
    StringGridDep.Rows[index - 1].AddStrings(mydep);
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
        myprop.Add('');      //multivalue
        myprop.Add('');      //editable
        myprop.Add('');      //possible values
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
      StringGridProp.Rows[index - 1].AddStrings(myprop);
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


procedure TResultform1.BtCreateEmptyTemplateClick(Sender: TObject);
begin
  begin
    useRunMode := createTemplate;
    setRunMode;
    MemoAnalyze.Clear;
    StringGridDep.Clear;
    PageControl1.ActivePage := resultForm1.TabSheetProduct;
    Application.ProcessMessages;
    initaktproduct;
    makeProperties;
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
      myprop.StrDefault.Text := '';
      myprop.boolDefault := StringGridProp.Cells[7, i].ToBoolean;
    end;
  end;
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
    case useRunMode of
      analyzeOnly:
      begin
        //we should never be here
        logdatei.log(
          'Error: in BtProductNextStepClick RunMode: analyzeOnly', LLError);
      end;
      singleAnalyzeCreate:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct2;
        Application.ProcessMessages;
      end;
      twoAnalyzeCreate_1,
      twoAnalyzeCreate_2:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct2;
        Application.ProcessMessages;
      end;
      createTemplate:
      begin
        PageControl1.ActivePage := resultForm1.TabSheetProduct2;
        Application.ProcessMessages;
      end;
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
  case useRunMode of
    analyzeOnly:
    begin
      // we should never be here
      logdatei.log('Error: in BtProductNextStepClick RunMode: analyzeOnly', LLError);
    end;
    singleAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    twoAnalyzeCreate_1, twoAnalyzeCreate_2:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    createTemplate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
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
begin
  if ((aktProduct.SetupFiles[0].installDirectory = '') or
    (aktProduct.SetupFiles[0].installDirectory = 'unknown')) and
    (aktProduct.SetupFiles[0].installerId <> stMsi) then
  begin
    //checkok := False;
    // we warn here only
    ShowMessage(sWarnInstalldirUnknown);
  end;
  if checkok then
  begin
    case useRunMode of
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
        useRunMode := twoAnalyzeCreate_2;
        MessageDlg(rsTwonalyzeAndCreateMsgHead,
          rsTwonalyzeAndCreateMsgSecondSetup,
          mtInformation, [mbOK], '');
        OpenDialog1.FilterIndex := 1;   // setup
        if OpenDialog1.Execute then
        begin
          PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
          MemoAnalyze.Clear;
          Application.ProcessMessages;
          Analyze(OpenDialog1.FileName,
            aktProduct.SetupFiles[1], True);
          SetTICheckBoxesMST(aktProduct.SetupFiles[1].installerId);
        end;
        //PageControl1.ActivePage := resultForm1.TabSheetSetup2;
        //Application.ProcessMessages;
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
  checkok: boolean = True;
begin
  if ((aktProduct.SetupFiles[1].installDirectory = '') or
    (aktProduct.SetupFiles[1].installDirectory = 'unknown')) and
    (aktProduct.SetupFiles[1].installerId <> stMsi) then
  begin
    // checkok := False;
    // we warn here only
    ShowMessage(sWarnInstalldirUnknown);
  end;
  if checkok then
  begin

    case useRunMode of
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
    useRunMode := analyzeOnly;
    setRunMode;
    MemoAnalyze.Clear;
    if StringGridDep.RowCount > 1 then
      for i := StringGridDep.RowCount - 1 downto 1 do
        StringGridDep.DeleteRow(i);
    if StringGridProp.RowCount > 1 then
      for i := StringGridProp.RowCount - 1 downto 1 do
        StringGridProp.DeleteRow(i);
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
begin

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


procedure TResultform1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := @ApplicationEventIdle;
  main1;
end;

procedure TResultform1.memoadd(line: string);
begin
  MemoAnalyze.append(line);
end;

procedure TResultform1.ApplicationEventIdle(Sender: TObject; var Done: boolean);
begin
  if not startupfinished then
    main2;
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

procedure TResultform1.TabSheetStartExit(Sender: TObject);
begin
  ResultForm1.Width := 1185;
  ResultForm1.Height := 566;
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
  if StringGridProp.RowCount > 1 then
    for i := StringGridProp.RowCount - 1 downto 1 do
      StringGridProp.DeleteRow(i);
  //StringGridProp.Clear;

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
    StringGridProp.Rows[index].AddStrings(myprop);
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

  if useRunMode = twoAnalyzeCreate_1 then
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
