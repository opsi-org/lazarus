unit osdform;

// {$mode objfpc}{$H+}
{$MODE DELPHI}{$H+}



// ToDo:

// - Description auslesen
// - Eingaben prüfen

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  ShlObj,
  {$ENDIF WINDOWS}
  Classes, SysUtils, FileUtil, RTTICtrls, RTTIGrids,
  //SynEdit, SynMemo,
  Forms, Controls, Graphics,
  LCLType,
  Dialogs, ExtCtrls,
  StdCtrls,
  Buttons,
  ComCtrls,
  Menus,
  Registry,
  Strings,
  StrUtils,
  //VersionInfoX,
  Process,
  typinfo,
  CustApp,
  //htmlview,
  //help,
  fileinfo,
  osdhelper,
  osdanalyze,
  winpeimagereader,
  lcltranslator, EditBtn, Spin, JSONPropStorage, Grids, PairSplitter,
  oslog,
  osdbasedata,
  osdconfigdlg,
  osdcreate,
  fpjsonrtti,
  osddlgnewdependency,
  osddlgnewproperty,
  osparserhelper;

type

  TGuiMode = (analyzeOnly, singleAnalyzeCreate, twoAnalyzeCreate_1,
    twoAnalyzeCreate_2,createTemplate, gmUnknown);

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
    BitBtnOpenFile: TBitBtn;
    BitBtnDefault: TBitBtn;
    BitBtnMST1: TBitBtn;
    BitBtnOpenFile1: TBitBtn;
    BitBtnOpenMst1: TBitBtn;
    BitBtnOpenMst2: TBitBtn;
    BtATwonalyzeAndCreate: TBitBtn;
    BtCreateEmptyTemplate: TBitBtn;
    BtAnalyzeOnly: TBitBtn;
    CheckBoxBuild: TCheckBox;
    CheckBoxInstall: TCheckBox;
    CheckBoxQuiet: TCheckBox;
    CheckBoxUseMst: TCheckBox;
    CheckBoxUseMst1: TCheckBox;
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
    ImageList1: TImageList;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
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
    MenuItemConfig: TMenuItem;
    OpenDialogSetupfile: TOpenDialog;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel10: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelDefault: TPanel;
    PanelProcess: TPanel;
    processing: TLabel;
    processStatement: TLabel;
    ProgressBar1: TProgressBar;
    ProgressBarAnalyze: TProgressBar;
    RadioButtonAuto: TRadioButton;
    RadioButtonCreateOnly: TRadioButton;
    RadioButtonInteractive: TRadioButton;
    SBtnOpen: TSpeedButton;
    SBtnExit: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    StringGridDep: TStringGrid;
    StringGridProp: TStringGrid;
    TabSheetProduct2: TTabSheet;
    TabSheetCreate: TTabSheet;
    TabSheetStart: TTabSheet;
    TabSheetSetup2: TTabSheet;
    TabSheetProduct: TTabSheet;
    TabSheetSetup1: TTabSheet;
    FileOpenSetupFile: TMenuItem;
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
    TICheckBoxlicenseRequired: TTICheckBox;
    TIComboBoxArch1: TTIComboBox;
    TIComboBoxArch2: TTIComboBox;
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
    procedure FlowPanel14Click(Sender: TObject);
    procedure FlowPanel18Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItemConfigClick(Sender: TObject);
    procedure setGuiMode;
    procedure BitBtnClose1Click(Sender: TObject);
    procedure BtAnalyzeOnlyClick(Sender: TObject);
    (*
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBoxUseMstChange(Sender: TObject);
    procedure ComboBoxArchModeChange(Sender: TObject);
    procedure FileCreateLogfileClick(Sender: TObject);
    *)
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

    procedure SBtnOpenClick(Sender: TObject);
    procedure SBtnExitClick(Sender: TObject);
    procedure TabSheetCreateShow(Sender: TObject);
    procedure TISpinEditPrioChange(Sender: TObject);
    procedure TITrackBarPrioChange(Sender: TObject);
    procedure fetchDepPropFromForm;
  private
    { private declarations }
    useGuiMode: TGuiMode;
  public
    { public declarations }
    procedure memoadd(line: string);

  end;



procedure main;
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
  myVersion: string;
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


resourcestring
  sMBoxHeader = 'opsi setup detector';
  sHelpHeader = 'opsi setup detector Help';
  // dialogs
  sAskCreatePacket = 'Create packet %0:s?';
  sHelpFile = 'languages\Help.de.html';
  sLogfileInfo = 'Logfile created: %s';

  // Error messages
  sErrFldInstDirEmpty = 'Error: Field Install Directory is empty!';
  sErrFldMsiProductCodeEmpty = 'Error: Field MSI Product Code is empty!';
  sErrFldOpsiProductIdEmpty = 'Error: Field opsi Packet ID is empty!';
  sErrFldOpsiProductVersionEmpty = 'Error: Field Product Version is empty!';
  sErrFldSetupEmpty = 'Error: No setup file selected!';
  sErrMSINotFound = 'Error: MSI file does not exist!';
  sErrMSTNotFound =
    'Error: MST file does not exist in the MSI-directory!';
  sErrOpsiPackageBuilderStart = 'Error invoking opsi package builder';
  sErrOpsiPackageBuilderErrCode =
    'Error invoking OPSI Package Builder (error code=%0:d). Please remove quiet option to check for further information.';
  sErrPacketBaseDirNotFound = 'Error: Packet BaseDir does not exist!';
  sErrProductVersionInvalid = 'Error: for Product Version only numbers allowed !';
  sErrSelectInstallType =
    'First select setup type tab (MSI, Inno Setup ...) and fill out form';
  sErrSetupFileNotFound = 'Error: Setup file does not exist!';
  sErrPacketDirExists = 'Error: opsi packet folder %s already exists!';
  sErrExtractMSIfailed = 'Error: extracting MSI from %s failed!';
  sInfoFinished = 'finished.';
  sWarnInstDirEmptyNoDeinstall =
    'Warning: Install Directory is empty, deinstall script cannot be patched!';


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
  with resultform1 do
  begin
    with aktProduct do
    begin
      TIComboBoxInstaller1.Link.SetObjectAndProperty(SetupFiles[0], 'installerid');
      TIComboBoxInstaller2.Link.SetObjectAndProperty(SetupFiles[1], 'installerid');
      TIEditSetupfile1.Link.SetObjectAndProperty(SetupFiles[0], 'setupFullFileName');
      TIEditSetupFile2.Link.SetObjectAndProperty(SetupFiles[1], 'setupFullFileName');
      TIComboBoxArch1.Link.SetObjectAndProperty(SetupFiles[0], 'architecture');
      TIComboBoxArch2.Link.SetObjectAndProperty(SetupFiles[1], 'architecture');
      TIEditMstFile1.Link.SetObjectAndProperty(SetupFiles[0], 'mstFullFileName');
      TIEditMstFile2.Link.SetObjectAndProperty(SetupFiles[1], 'mstFullFileName');
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
      TIEditSetup1UnCommand.Link.SetObjectAndProperty(SetupFiles[0],
        'uninstallCommandLine');
      TIEditSetup1UnProgram.Link.SetObjectAndProperty(SetupFiles[0], 'uninstallProg');
      (*
      TIEditProductIdS1.Link.SetObjectAndProperty(productdata, 'productId');
      TIEditProductIdS2.Link.SetObjectAndProperty(productdata, 'productId');
      TIEditProductNameS1.Link.SetObjectAndProperty(productdata, 'productName');
      TIEditProductNameS2.Link.SetObjectAndProperty(productdata, 'productName');
      *)
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
    Visible := True;
  end;

end;

procedure TResultform1.FormDestroy(Sender: TObject);
begin
  TIEditSetupfile1.Link.TIObject := nil;
  TIEditSetupFile2.Link.TIObject := nil;
  TIComboBoxInstaller1.Link.TIObject := nil;
  TIComboBoxInstaller2.Link.TIObject := nil;
  TIComboBoxArch1.Link.TIObject := nil;
  TIComboBoxArch2.Link.TIObject := nil;
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
  //TIEditProductIdS1.Link.TIObject := nil;
  //TIEditProductIdS2.Link.TIObject := nil;
  //TIEditProductNameS1.Link.TIObject := nil;
  //TIEditProductNameS2.Link.TIObject := nil;
  TIEditworkbenchpath.Link.TIObject := nil;
  TIEditSetup1Command.Link.TIObject := nil;
  //TIGridDep.ListObject := nil;
  TITrackBarPrio.Link.TIObject := nil;
  TISpinEditPrio.Link.TIObject := nil;
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

procedure main;
var
  ErrorMsg: string;
  FileVerInfo: TFileVersionInfo;
  lfilename: string;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
  // Initialize logging
  LogDatei := TLogInfo.Create;
  lfilename := ExtractFileName(Application.ExeName);
  lfilename := ExtractFileNameWithoutExt(lfilename);
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.WritePartLog := False;
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
  (*
  // push prelog buffer to logfile
  if preloglist.Count > 0 then
    for i := 0 to preloglist.Count - 1 do
      LogDatei.log(preloglist.Strings[i], LLEssential);
  if logAndTerminate then
  begin
    LogDatei.log('Closing log and terminating due to previous errors.', LLCritical);
    logdatei.Close;
    Application.Terminate;
    Exit;
  end;
  *)
  LogDatei.log('Log for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now), LLinfo);

  LogDatei.LogLevel := 8;
  myExeDir := ExtractFileDir(ParamStr(0));
  myexitcode := 0;
  myerror := '';
  showgui := True;
  resultForm1.useGuiMode := gmUnknown;
  optionlist := TStringList.Create;
  optionlist.Append('help');
  optionlist.Append('filename::');
  optionlist.Append('nogui');

  (*
  opsitmp := SysUtils.GetEnvironmentVariable('TEMP')+'\opsitmp\';  // global var, extractMSI.cmd must use same path
  opsidir := 'C:\opsi.org';  // global var

  if DirectoryExists(opsidir) then
  begin
    if not DirectoryExists(opsidir+'\log') then
       CreateDir(opsidir+'\log');
    Logfile := opsidir+'\log\opsiSetupDetector.log';
  end
  else
      Logfile := opsitmp+'opsiSetupDetector.log';

  // if resultForm1.TabSheetAnalyze.Visible = true then
  //   resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAnalyze;


  // get global ConfigDir
  configDir := GetAppConfigDir(True);
  // configDir = "All Users\Appdata\opsi setup detector"
  if not DirectoryExists(configDir) then
    CreateDir(configDir);

  // read config file
  configFileName := configDir + 'config.txt';
  AssignFile(fConfig, configFileName);
  if FileExists(configFileName) then
  begin
    Reset(fConfig);
    if not EOF(fConfig) then
      ReadLn(fConfig, packetBaseDir);
    CloseFile(fConfig);
  end;
  if (packetBaseDir = '') or (not DirectoryExists(packetBaseDir)) then
  begin
    packetBaseDir := 'C:' + DirectorySeparator + 'opsi' + DirectorySeparator +
      'testproducts';
    if DirectoryExists(packetBaseDir) then
    begin
      Rewrite(fConfig);
      WriteLn(fConfig, packetBaseDir);
      CloseFile(fConfig);
    end
    else
      packetBaseDir := '';
  end;
  *)

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

  if Application.HasOption('nogui') then
    showgui := False;

  if showgui then
  begin
    //resultform1.Edit_PacketbaseDir.text := packetBaseDir;
    //resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
  end;

  if Application.HasOption('filename') then
  begin
    myfilename := trim(Application.GetOptionValue('filename'));
    if not FileExists(myfilename) then
    begin
      myerror := 'Error: Given filename: ' + myfilename + ' does not exist.';
      WriteHelp;
    end;
    if showgui then
    begin
      resultform1.Visible := True;
      Application.ProcessMessages;
      Analyze(myfilename, aktProduct.SetupFiles[0], True);
    end
    else
      analyze_binary(myfilename, False, False, aktProduct.SetupFiles[0]);
  end
  else
  begin
    if showgui then
    begin
      initGUI;
    end
    else
    begin
      myerror := 'Error: No filename given but nogui';
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

procedure TResultform1.setGuiMode;
begin
  case useGuiMode of
    analyzeOnly:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := True;
      TabSheetSetup1.Enabled := True;
      TabSheetSetup2.Enabled := False;
      TabSheetProduct.Enabled := False;
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
      //BtAnalyzeNextStep.Caption:='Next Step';
      //BtAnalyzeNextStep.Glyph.LoadFromResourceName();
      BtSetup1NextStep.Enabled := True;
    end;
    twoAnalyzeCreate_1,twoAnalyzeCreate_2:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := True;
      TabSheetSetup1.Enabled := True;
      TabSheetSetup2.Enabled := True;
      TabSheetProduct.Enabled := True;
      BtSetup1NextStep.Enabled := True;
    end;
    createTemplate:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := False;
      TabSheetSetup1.Enabled := False;
      TabSheetSetup2.Enabled := False;
      TabSheetProduct.Enabled := True;
    end;
    gmUnknown:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := True;
      TabSheetSetup1.Enabled := True;
      TabSheetSetup2.Enabled := True;
      TabSheetProduct.Enabled := True;
      BtSetup1NextStep.Enabled := True;
    end;
  end;
end;

procedure TResultform1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TResultform1.MenuItemConfigClick(Sender: TObject);
var
  Streamer: TJSONStreamer;
  JSONString: string;
begin
  FOSDConfigdlg.ShowModal;
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
    JSONString := Streamer.ObjectToJSONString(myconfiguration);
    logdatei.log('After configdialog: ' + JSONString, LLDebug);
  finally
    Streamer.Destroy;
  end;
end;

procedure TResultform1.BtSingleAnalyzeAndCreateClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    useGuiMode := singleAnalyzeCreate;
    setGuiMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    //TIProgressBarAnalyze_progress.Link.SetObjectAndProperty(aktProduct.SetupFiles[0], 'analyze_progress');
    //TIProgressBarAnalyze_progress.Loaded;
    MemoAnalyze.Clear;
    Application.ProcessMessages;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
  end;
end;


procedure TResultform1.FlowPanel14Click(Sender: TObject);
begin

end;

procedure TResultform1.FlowPanel18Click(Sender: TObject);
begin

end;

procedure TResultform1.BtATwonalyzeAndCreateClick(Sender: TObject);
var
  myprop : Tstringlist;
  index : integer;
begin
  MessageDlg('opsi-setup-detector: Two File (32/64 Bit) Product',
                  'First Select the 32 Bit Setup exe',
                   mtInformation, [mbOK],'');
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    useGuiMode := twoAnalyzeCreate_1;
    setGuiMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    MemoAnalyze.Clear;
    Application.ProcessMessages;
    initaktproduct;
    // start add property
    index := StringGridProp.RowCount;
      Inc(index);
      StringGridProp.RowCount := index;
      myprop := TStringList.Create;
      myprop.Add(IntToStr(index - 1));
      myprop.Add('install_architecture');
      myprop.Add('Which archtiecture (32 / 64 Bit) has to be installed?');
      myprop.Add('unicode');  //type
      myprop.Add('False');      //multivalue
      myprop.Add('False');      //editable
      myprop.Add('["32","64","system specific"]');      //possible values
      myprop.Add('["system specific"]');      //default values
      StringGridProp.Rows[index - 1].AddStrings(myprop);
      myprop.Free;
      // start add property
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
  end;
end;

procedure TResultform1.BtAnalyzeNextStepClick(Sender: TObject);
begin
  case useGuiMode of
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
      logdatei.log('Error: in BtSetup1NextStepClick guimode: gmUnknown', LLError);
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
  tmpstr : string;
  exists : boolean;
begin
  FNewPropDlg.RadioButtonPropBool.Checked := True;
  FNewPropDlg.RadioButtonPropStringChange(Sender);
  procmess;
  if FNewPropDlg.ShowModal = mrOk then
  begin
    // add
    index := StringGridProp.RowCount;
    tmpstr := FNewPropDlg.EditPropName.Text;
    exists := false;
    for i:= 0 to index-1 do
      if lowercase(tmpstr) = lowercase(StringGridProp.Cells[1, i]) then exists := true;
    if exists then
      MessageDlg('opsi-setup-detector: Property Editor: Error',
                  'property Id: '+FNewPropDlg.EditPropName.Text+' exists. Duplicates not allowed.',
                   mtError, [mbOK],'')
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
            tmpliststr := tmpliststr + '"' + FNewPropDlg.ListBoxPropDefVal.Items[i] + '",';
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
var
  range: integer;
  str: string;
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
  mydep: TStringList;
  x, y: integer;
  aPoint: TPoint;
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
                  'No Dependency selected.',
                   mtError, [mbOK],'')
  end;

end;

procedure TResultform1.BitBtnEditPropClick(Sender: TObject);
// edit property
var
  myprop: TStringList;
  index: integer;
  i: integer;
  tmpliststr: string;
  tmpstr: string;
  errorstr : string;
  remaining: string;
  x, y: integer;
  aPoint: TPoint;
begin
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
      FNewPropDlg.CheckBoxPropMultiValChange(sender);
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
        if Skip(',',remaining,remaining,errorstr) then ;
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
        if Skip(',',remaining,remaining,errorstr) then ;
        i := FNewPropDlg.ListBoxPropDefVal.Items.IndexOf(tmpstr);
        if i > -1 then FNewPropDlg.ListBoxPropDefVal.Selected[i] := True;
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
            tmpliststr := tmpliststr + '"' + FNewPropDlg.ListBoxPropDefVal.Items[i] + '",';
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
    MessageDlg('opsi-setup-detector: Property Editor: Error',
                  'No Property selected.',
                   mtError, [mbOK],'')
  end;

end;


procedure TResultform1.BtCreateEmptyTemplateClick(Sender: TObject);
begin
  begin
    useGuiMode := createTemplate;
    setGuiMode;
    PageControl1.ActivePage := resultForm1.TabSheetProduct;
    Application.ProcessMessages;
    initaktproduct;
  end;
end;

procedure TResultform1.fetchDepPropFromForm;
var
  i : integer;
  mydep
begin
  //dependencies
  for i := 1 to StringGridDep.RowCount do
  begin
    //aktProduct.dependencies.;
    akt := StringGridDep.Cells[1, i];
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
  end;


end;

procedure TResultform1.BtCreateProductClick(Sender: TObject);
begin
  try
    PanelProcess.Visible := True;
    fetchDepPropFromForm;
    createProductStructure;
  finally
    PanelProcess.Visible := False;
  end;
end;

procedure TResultform1.BtProduct1NextStepClick(Sender: TObject);
begin
  case useGuiMode of
    analyzeOnly:
    begin
      // we should never be here
      logdatei.log('Error: in BtProductNextStepClick guimode: analyzeOnly', LLError);
    end;
    singleAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetProduct2;
      Application.ProcessMessages;
    end;
    twoAnalyzeCreate_1,twoAnalyzeCreate_2:
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
      logdatei.log('Error: in BtProductNextStepClick guimode: gmUnknown', LLError);
    end;
  end;
end;

procedure TResultform1.BtProduct2NextStepClick(Sender: TObject);
begin
  case useGuiMode of
    analyzeOnly:
    begin
      // we should never be here
      logdatei.log('Error: in BtProductNextStepClick guimode: analyzeOnly', LLError);
    end;
    singleAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    twoAnalyzeCreate_1,twoAnalyzeCreate_2:
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
      logdatei.log('Error: in BtProductNextStepClick guimode: gmUnknown', LLError);
    end;
  end;
end;

procedure TResultform1.BtSetup1NextStepClick(Sender: TObject);
begin
  case useGuiMode of
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
      useGuiMode := twoAnalyzeCreate_2;
      MessageDlg('opsi-setup-detector: Two File (32/64 Bit) Product',
                  'Now Select the 64 Bit Setup exe',
                   mtInformation, [mbOK],'');
      OpenDialog1.FilterIndex := 1;   // setup
      if OpenDialog1.Execute then
      begin
        PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
        MemoAnalyze.Clear;
        Application.ProcessMessages;
        Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[1], True);
      end;
      //PageControl1.ActivePage := resultForm1.TabSheetSetup2;
      //Application.ProcessMessages;
    end;
    createTemplate:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup1NextStepClick guimode: createTemplate', LLError);
      //PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      //Application.ProcessMessages;
    end;
    gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup1NextStepClick guimode: gmUnknown', LLError);
    end;
  end;
end;

procedure TResultform1.BtSetup2NextStepClick(Sender: TObject);
begin
  case useGuiMode of
    analyzeOnly:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup2NextStepClick guimode: analyzeOnly', LLError);
    end;
    singleAnalyzeCreate:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup2NextStepClick guimode: singleAnalyzeCreate',
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
      logdatei.log('Error: in BtSetup1NextStepClick guimode: createTemplate', LLError);
      //PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      //Application.ProcessMessages;
    end;
    gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup2NextStepClick guimode: gmUnknown', LLError);
    end;
  end;
end;

procedure TResultform1.BitBtnClose1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TResultform1.BtAnalyzeOnlyClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    useGuiMode := analyzeOnly;
    setGuiMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0], True);
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
var
  myimage: TImage;
  str: string;
begin
  main;
  //(*
  //PageControl1.Images.AddLazarusResource('HOME2XA');
  (*
  PageControl1.Images.AddLazarusResource('MAGNIFYING-GLASS-2X');
  PageControl1.Images.AddLazarusResource('COG-2X');
  PageControl1.Images.AddLazarusResource('FLASH-2X');
  *)
  (*
  str :=ExpandFileName(ExtractFilePath(Application.ExeName)+ 'home-2xb.png');
  myimage := TImage.Create(nil);
  myimage.Picture.LoadFromFile(str);
  PageControl1.Images.add(myimage.Picture.Bitmap,nil);
  *)
  (*
  PageControl1.Images.AddLazarusResource('MAGNIFYING-GLASS-2X');
  PageControl1.Images.AddLazarusResource('COG-2X');
  PageControl1.Images.AddLazarusResource('FLASH-2X');
  *)
  //(*
  TabSheetStart.ImageIndex := 0;
  //(*
  TabSheetAnalyze.ImageIndex := 1;
  TabSheetSetup1.ImageIndex := 2;
  TabSheetSetup2.ImageIndex := 2;
  TabSheetProduct.ImageIndex := 3;
  TabSheetProduct2.ImageIndex := 3;
  TabSheetCreate.ImageIndex := 4;
  //*)
end;

procedure TResultform1.memoadd(line: string);
begin
  MemoAnalyze.append(line);
end;




procedure TResultform1.RadioButtonBuildModeChange(Sender: TObject);
var
  RadioButtonName: string;
begin
  RadioButtonName := (Sender as TRadioButton).Name;
  CheckBoxBuild.Enabled := False;
  CheckBoxInstall.Enabled := False;
  CheckBoxQuiet.Enabled := False;
  if RadioButtonName = 'RadioButtonCreateOnly' then
  begin
  end
  else if RadioButtonName = 'RadioButtonInteractive' then
  begin
  end
  else if RadioButtonName = 'RadioButtonAuto' then
  begin
    CheckBoxBuild.Enabled := True;
    CheckBoxInstall.Enabled := True;
    CheckBoxQuiet.Enabled := True;
  end;
end;



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
