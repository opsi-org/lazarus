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
  lcltranslator, EditBtn, Spin, JSONPropStorage,
  oslog,
  osdbasedata,
  osdconfigdlg,
  osdcreate,
  fpjsonrtti;




type

  TGuiMode = (analyzeOnly, singleAnalyzeCreate, twoAnalyzeCreate,
    createTemplate, gmUnknown);

  { TResultform1 }

  TResultform1 = class(TForm)
    BitBtnWorkBenchPath: TBitBtn;
    BitBtnRecheckWorkbench: TBitBtn;
    BtAnalyzeNextStep: TBitBtn;
    BtCreateProduct: TBitBtn;
    BtProductNextStep: TBitBtn;
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
    ButtonCreatePacket: TButton;
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
    FlowPanel3: TFlowPanel;
    FlowPanel4: TFlowPanel;
    FlowPanel6: TFlowPanel;
    FlowPanel8: TFlowPanel;
    FlowPanel9: TFlowPanel;
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
    LabelWorkbenchOK: TLabel;
    LabelWorkbenchNotOK: TLabel;
    MemoDefault: TMemo;
    MenuItemConfig: TMenuItem;
    OpenDialogSetupfile: TOpenDialog;
    PanelProcess: TPanel;
    PanelDefault: TPanel;
    processing: TLabel;
    processStatement: TLabel;
    ProgressBar1: TProgressBar;
    RadioButtonAuto: TRadioButton;
    RadioButtonCreateOnly: TRadioButton;
    RadioButtonInteractive: TRadioButton;
    SBtnOpen: TSpeedButton;
    SBtnExit: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    TabSheetCreate: TTabSheet;
    TabSheetStart: TTabSheet;
    TabSheetSetup2: TTabSheet;
    TabSheetProduct: TTabSheet;
    TabSheetSetup1: TTabSheet;
    CheckBox1: TCheckBox;
    FileOpenSetupFile: TMenuItem;
    FileExit: TMenuItem;
    //FileCreateLogfile: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemFile: TMenuItem;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
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
    TIMemoDesc: TTIMemo;
    TIMemoAdvice: TTIMemo;
    TISpinEditPackageVers: TTISpinEdit;
    ToolBar1: TToolBar;
    mysetup1: TSetupFile;

    procedure BitBtnRecheckWorkbenchClick(Sender: TObject);
    procedure BitBtnWorkBenchPathClick(Sender: TObject);
    procedure BtAnalyzeNextStepClick(Sender: TObject);
    procedure BtATwonalyzeAndCreateClick(Sender: TObject);
    procedure BtCreateEmptyTemplateClick(Sender: TObject);
    procedure BtCreateProductClick(Sender: TObject);
    procedure BtProductNextStepClick(Sender: TObject);
    procedure BtSetup1NextStepClick(Sender: TObject);
    procedure BtSetup2NextStepClick(Sender: TObject);
    procedure BtSingleAnalyzeAndCreateClick(Sender: TObject);
    procedure FlowPanel14Click(Sender: TObject);
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
      TIEditSetup1UnCommand.Link.SetObjectAndProperty(SetupFiles[0], 'uninstallCommandLine');
      TIEditSetup1UnProgram.Link.SetObjectAndProperty(SetupFiles[0], 'uninstallProg');
      (*
      TIEditProductIdS1.Link.SetObjectAndProperty(produktpropties, 'productId');
      TIEditProductIdS2.Link.SetObjectAndProperty(produktpropties, 'productId');
      TIEditProductNameS1.Link.SetObjectAndProperty(produktpropties, 'productName');
      TIEditProductNameS2.Link.SetObjectAndProperty(produktpropties, 'productName');
      *)
      // product
      TIEditProdVersion3.Link.SetObjectAndProperty(produktpropties, 'productVersion');
      TISpinEditPackageVers.Link.SetObjectAndProperty(produktpropties, 'packageVersion');
      TIEditProdID.Link.SetObjectAndProperty(produktpropties, 'productId');
      TIEditProdName.Link.SetObjectAndProperty(produktpropties, 'productName');
      TIMemoAdvice.Link.SetObjectAndProperty(produktpropties, 'advice');
      TIMemoDesc.Link.SetObjectAndProperty(produktpropties, 'description');
      TICheckBoxlicenseRequired.Link.SetObjectAndProperty(produktpropties,
        'licenserequired');
    end;
    TIEditworkbenchpath.Link.SetObjectAndProperty(myconfiguration, 'workbench_path');;
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
  TIEditProductIdS1.Link.TIObject := nil;
  TIEditProductIdS2.Link.TIObject := nil;
  TIEditProductNameS1.Link.TIObject := nil;
  TIEditProductNameS2.Link.TIObject := nil;
  TIEditworkbenchpath.Link.TIObject := nil;
  TIEditSetup1Command.Link.TIObject := nil;
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
  if not Assigned(osdbasedata.aktProduct.produktpropties) then
  begin
    LogDatei.log('Error: produktpropties not initalized', LLCritical);
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
      Analyze(myfilename, aktProduct.SetupFiles[0]);
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
    twoAnalyzeCreate:
    begin
      TabSheetStart.Enabled := True;
      TabSheetAnalyze.Enabled := True;
      TabSheetSetup1.Enabled := True;
      TabSheetSetup2.Enabled := False;
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
    logdatei.log('After configdialog: '+JSONString, LLDebug);
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
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0]);
  end;
end;


procedure TResultform1.FlowPanel14Click(Sender: TObject);
begin

end;

procedure TResultform1.BtATwonalyzeAndCreateClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 1;   // setup
  if OpenDialog1.Execute then
  begin
    useGuiMode := twoAnalyzeCreate;
    setGuiMode;
    PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
    Application.ProcessMessages;
    initaktproduct;
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0]);
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
    twoAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup1;
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
      logdatei.log('Error: in BtSetup1NextStepClick guimode: gmUnknown',LLError);
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

procedure TResultform1.BtCreateProductClick(Sender: TObject);
begin
  try
    PanelProcess.Visible:=true;
    createProductStructure;
  finally
    PanelProcess.Visible:=false;
  end;
end;

procedure TResultform1.BtProductNextStepClick(Sender: TObject);
begin
  case useGuiMode of
    analyzeOnly:
    begin
      // we should never be here
      logdatei.log('Error: in BtProductNextStepClick guimode: analyzeOnly',LLError);
    end;
    singleAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetCreate;
      Application.ProcessMessages;
    end;
    twoAnalyzeCreate:
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
      logdatei.log('Error: in BtProductNextStepClick guimode: gmUnknown',LLError);
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
    twoAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetSetup2;
      Application.ProcessMessages;
    end;
    createTemplate:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup1NextStepClick guimode: createTemplate',LLError);
      //PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      //Application.ProcessMessages;
    end;
    gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup1NextStepClick guimode: gmUnknown',LLError);
    end;
  end;
end;

procedure TResultform1.BtSetup2NextStepClick(Sender: TObject);
begin
  case useGuiMode of
    analyzeOnly:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup2NextStepClick guimode: analyzeOnly',LLError);
    end;
    singleAnalyzeCreate:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup2NextStepClick guimode: singleAnalyzeCreate',LLError);
    end;
    twoAnalyzeCreate:
    begin
      PageControl1.ActivePage := resultForm1.TabSheetProduct;
      Application.ProcessMessages;
    end;
    createTemplate:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup1NextStepClick guimode: createTemplate',LLError);
      //PageControl1.ActivePage := resultForm1.TabSheetSetup1;
      //Application.ProcessMessages;
    end;
    gmUnknown:
    begin
      // we should never be here
      logdatei.log('Error: in BtSetup2NextStepClick guimode: gmUnknown',LLError);
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
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0]);
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
  list : Tstringlist;
begin
  list := TStringlist.Create;
  progname := ExtractFileName(ParamStr(0));
  msg := progname + ' Version: ' + myVersion;
  list.Add(msg);
  list.Add('(c) uib gmbh under AGPLv3');
  list.Add('This is a part of the opsi.org Project: https://opsi.org');
  list.Add('');
  list.add('Icons from Iconic (https://useiconic.com/) under MIT Licnse.');
  //list.add('https://github.com/iconic/open-iconic/blob/master/ICON-LICENSE');
  ShowMessage(list.text);
  list.free;

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
  str : string;
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
  TabSheetStart.ImageIndex:=0;
  //(*
  TabSheetAnalyze.ImageIndex:=1;
  TabSheetSetup1.ImageIndex:=2;
  TabSheetSetup2.ImageIndex:=2;
  TabSheetProduct.ImageIndex:=3;
  TabSheetCreate.ImageIndex:=4;
  //*)
end;

procedure TResultform1.memoadd(line: string);
begin
  memo1.append(line);
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
    Analyze(OpenDialog1.FileName, aktProduct.SetupFiles[0]);
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
    resultForm1.LabelWorkbenchOK.Visible:=true;
    resultForm1.LabelWorkbenchNotOK.Visible:=false;
  end
  else
  begin
    resultForm1.LabelWorkbenchOK.Visible:=false;
    resultForm1.LabelWorkbenchNotOK.Visible:=true;
  end;
end;

procedure TResultform1.TabSheetCreateShow(Sender: TObject);
begin
  checkWorkbench;
end;


end.
