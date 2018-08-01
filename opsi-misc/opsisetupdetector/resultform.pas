unit resultform;

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
  lcltranslator, EditBtn, Spin,
  oslog,
  osdbasedata;

const
  sMBoxHeader = 'opsi setup detector';
  sHelpHeader = 'opsi setup detector Help';

  sAskCreatePacket = 'Create packet %0:s?';

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

  sHelpFile = 'languages\Help.de.html';

  sLogfileInfo = 'Logfile created: %s';

type

  { TResultform1 }

  TResultform1 = class(TForm)
    BitBtnOpenFile: TBitBtn;
    BitBtnDefault: TBitBtn;
    BitBtnMST1: TBitBtn;
    BitBtnOpenFile1: TBitBtn;
    BitBtnOpenMst1: TBitBtn;
    BitBtnOpenMst2: TBitBtn;
    CheckBoxUseMst: TCheckBox;
    CheckBoxUseMst1: TCheckBox;
    CheckBox_Default_License: TCheckBox;
    Edit_opsiProductID1: TEdit;
    Edit_ProductName1: TEdit;
    Edit_ProductVersion1: TEdit;
    FlowPanel1: TFlowPanel;
    FlowPanel10: TFlowPanel;
    FlowPanel11: TFlowPanel;
    FlowPanel12: TFlowPanel;
    FlowPanel13: TFlowPanel;
    FlowPanel2: TFlowPanel;
    FlowPanel6: TFlowPanel;
    FlowPanel8: TFlowPanel;
    FlowPanel9: TFlowPanel;
    FlowPanelMsiId1: TFlowPanel;
    FlowPanelMST1: TFlowPanel;
    FlowPanelSetup32: TFlowPanel;
    FlowPanelMST: TFlowPanel;
    FlowPanel4: TFlowPanel;
    FlowPanel5: TFlowPanel;
    FlowPanelMsiId: TFlowPanel;
    FlowPanel7: TFlowPanel;
    FlowPanelSetup33: TFlowPanel;
    FileHelp: TMenuItem;
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
    MemoAdvice: TMemo;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    MemoDefault: TMemo;
    MemoDescription: TMemo;
    MenuItemConfig: TMenuItem;
    OpenDialogSetupfile: TOpenDialog;
    PanelDefault: TPanel;
    SBtnOpen: TSpeedButton;
    SBtnExit: TSpeedButton;
    SpinEditPackageVersion1: TSpinEdit;
    StatusBar1: TStatusBar;
    TabSheetStart: TTabSheet;
    TabSheetSetup2: TTabSheet;
    TabSheetProduct: TTabSheet;
    TabSheetSetup1: TTabSheet;
    BitBtn_PacketBasedir: TBitBtn;
    BitBtnClose2: TBitBtn;
    BitBtnAnalyze: TBitBtn;
    ButtonCreatePacket: TButton;
    CheckBox1: TCheckBox;
    CheckBoxBuild: TCheckBox;
    CheckBoxInstall: TCheckBox;
    CheckBoxQuiet: TCheckBox;
    Edit_PacketbaseDir: TEdit;
    GroupBox2: TGroupBox;
    FileOpenSetupFile: TMenuItem;
    FileExit: TMenuItem;
    //FileCreateLogfile: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemFile: TMenuItem;
    processStatement: TLabel;
    processing: TLabel;
    Label9: TLabel;
    LabelVersion: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel9: TPanel;
    RadioButtonAuto: TRadioButton;
    RadioButtonCreateOnly: TRadioButton;
    RadioButtonInteractive: TRadioButton;
    SelectPacketBaseDir: TSelectDirectoryDialog;
    TabSheetAnalyze: TTabSheet;
    TIComboBoxArch1: TTIComboBox;
    TIComboBoxArch2: TTIComboBox;
    TIComboBoxInstaller1: TTIComboBox;
    TIComboBoxInstaller2: TTIComboBox;
    TIEditInstallDir2: TTIEdit;
    TIEditMsiId2: TTIEdit;
    TIEditMstFile2: TTIEdit;
    TIEditProductIdS1: TTIEdit;
    TIEditProductIdS2: TTIEdit;
    TIEditProductNameS1: TTIEdit;
    TIEditProductNameS2: TTIEdit;
    TIEditProdVersion1: TTIEdit;
    TIEditMsiId1: TTIEdit;
    TIEditProdVersion2: TTIEdit;
    TIEditRequiredSizeMB2: TTIEdit;
    TIEditSetupfile2: TTIEdit;
    TIEditSetupFileSizeMB1: TTIEdit;
    TIEditRequiredSizeMB1: TTIEdit;
    TIEditInstallDir1: TTIEdit;
    TIEditSetupfile1: TTIEdit;
    TIEditMstFile1: TTIEdit;
    TIEditSetupFileSizeMB2: TTIEdit;
    ToolBar1: TToolBar;
    mysetup1: TSetupFile;

    procedure BitBtnOpenFileClick(Sender: TObject);
    procedure BitBtn_PacketBasedirClick(Sender: TObject);
    procedure BitBtnClose1Click(Sender: TObject);
    procedure ButtonCreatePacketClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBoxUseMstChange(Sender: TObject);
    procedure ComboBoxArchModeChange(Sender: TObject);
    procedure FileCreateLogfileClick(Sender: TObject);
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
    procedure removeOtherTypeSpecificSections(setupType, setupFile: string);
    procedure SBtnOpenClick(Sender: TObject);
    procedure SBtnExitClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure memoadd(line: string);

  end;

procedure main;
procedure mywrite(line: string); overload;
procedure mywrite(line: string; loglevel: integer); overload;


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
  patchlist: TStringList;
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


implementation

{$R *.lfm}
//{$R manifest.rc}

(*
procedure registerForExplorer;
var
  myreg: Tregistry;
  doregister: boolean;
begin
  doregister := True;
  myreg := TRegistry.Create(KEY_ALL_ACCESS);
  myreg.RootKey := HKEY_CURRENT_USER;
  myreg.OpenKey('SOFTWARE\opsi.org\opsisetupdetector', True);
  if myreg.ValueExists('regsterAtExplorer') then
    doregister := myreg.ReadBool('regsterAtExplorer')
  else
    myreg.WriteBool('regsterAtExplorer', doregister);
  myreg.CloseKey;
  resultform1.CheckBox1.Checked := doregister;
  myreg.RootKey := HKEY_CURRENT_USER;
  myreg.OpenKey('Software\Classes\*\shell\opsi setup detector\Command', True);
  if doregister then
    myreg.WriteString('', '"' + ParamStr(0) + '" --filename=%1')
  else
    myreg.WriteString('', '');
  myreg.CloseKey;
  myreg.Free;
end;


function getSpecialFolder(csidlValue: integer): string;
var
  csidl: pItemIDList;
  nameBuf: array [0..MAX_PATH] of char;
begin
  Result := '';
  if SUCCEEDED(SHGetSpecialFolderLocation(0, csidlValue, csidl)) then
  begin
    if csidl <> nil then
    begin
      if SHGetPathFromIDList(csidl, namebuf) then
        Result := StrPas(namebuf);
      // Freecsidl(csidl);
    end;
  end
  else
    //Fix:
    // if assigned(SHGetFolderPath) and ((csidlvalue = CSIDL_APPDATA) or
    //   (csidlvalue = CSIDL_PERSONAL)) then
    begin
      if SUCCEEDED(SHGetFolderPath(0, csidlValue, 0, 0, namebuf)) then
        Result := StrPas(namebuf);
    end;
  //debugmessages.Add('getSpecialFolder: '+inttostr(csidlValue)+' -> ' + result);
  //if Assigned(LogDatei) then
  //LogDatei.DependentAdd('getSpecialFolder: '+inttostr(csidlValue)+' -> ' + result, LLDebug2);
end;
*)


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
      TIEditProductIdS1.Link.SetObjectAndProperty(produktpropties, 'productId');
      TIEditProductIdS2.Link.SetObjectAndProperty(produktpropties, 'productId');
      TIEditProductNameS1.Link.SetObjectAndProperty(produktpropties, 'productName');
      TIEditProductNameS2.Link.SetObjectAndProperty(produktpropties, 'productName');
    end;
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
  resultform1.LabelVersion.Caption := 'Version: ' + myVersion;
  myExeDir := ExtractFileDir(ParamStr(0));
  {$IFDEF WINDOWS}
  registerForExplorer;
  {$ENDIF WINDOWS}
  myexitcode := 0;
  myerror := '';
  showgui := True;
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
  *)
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
    Application.Terminate;
  end;
end;


{ TResultform1 }

procedure TResultform1.BitBtnClose1Click(Sender: TObject);
begin
  packetBaseDir := Edit_PacketbaseDir.Text;
  if (packetBaseDir <> '') and DirectoryExists(packetBaseDir) then
  begin
    Rewrite(fConfig);
    WriteLn(fConfig, packetBaseDir);
    CloseFile(fConfig);
  end;
  Application.Terminate;
end;


procedure TResultform1.BitBtn_PacketBasedirClick(Sender: TObject);
begin
  SelectPacketBaseDir.InitialDir := packetBaseDir;
  if SelectPacketBaseDir.Execute then
  begin
    packetBaseDir := SelectPacketBaseDir.FileName;
    Resultform1.Edit_PacketbaseDir.Text := packetBaseDir; // MSI
    Rewrite(fConfig);
    WriteLn(fConfig, packetBaseDir);
    CloseFile(fConfig);
  end;
end;

procedure TResultform1.BitBtnOpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    aktSetupFile.setupFullFileName := OpenDialog1.FileName;
end;

function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: integer;
begin
  Result := '';
  aPos := pos(A, Value);
  if aPos > 0 then
  begin
    aPos := aPos + Length(A);
    bPos := PosEx(B, Value, aPos);
    if bPos > 0 then
    begin
      Result := Copy(Value, aPos, bPos - aPos);
    end;
  end;
end;

procedure TResultform1.removeOtherTypeSpecificSections(setupType, setupFile: string);
var
  infileHandle, outfileHandle: Text;
  Patchmarker: string = '**--** PATCH_';
  filterType: string = '';
  thisType: string = '';
  aktline: string = '';
  tmpfile: string;
  isMarkerLine: boolean;
begin
  mywrite('removing all other type specific sections from: ' + setupfile);
  tmpfile := setupfile + '.tmp';

  {$I+}//use exceptions
  try
    AssignFile(infileHandle, setupfile);
    AssignFile(outfileHandle, tmpfile);
    reset(infileHandle);
    rewrite(outfileHandle);

    filterType := lowercase(setupType);

    // find and handle type specific patch sections
    while (not EOF(infileHandle)) do
    begin
      // find next type specific marker
      repeat
        ReadLn(infileHandle, aktline);
        isMarkerLine := (0 < pos(Patchmarker, aktline));
        if not isMarkerLine then
          writeln(outfileHandle, aktline);
      until isMarkerLine or EOF(infileHandle);
      if not EOF(infileHandle) then
      begin
        thisType := lowercase(ExtractBetween(aktline, Patchmarker, ' '));
        // write lines (or skip lines if other type)
        repeat
          ReadLn(infileHandle, aktline);
          isMarkerLine := (0 < pos(Patchmarker, aktline));
          if (thisType = filterType) and not isMarkerLine then
            writeln(outfileHandle, aktline);
        until isMarkerLine or EOF(infileHandle);
      end;
    end;

    CloseFile(infileHandle);
    CloseFile(outfileHandle);

    // delete setupfile and rename tmpfile to setupfile
    DeleteFile(PChar(setupfile));
    RenameFile(PChar(tmpfile), PChar(setupfile));

  except
    on E: EInOutError do
      mywrite('removeOtherTypeSpecificSections error: ' + E.ClassName + '/' + E.Message);
  end;
end;


procedure TResultform1.ButtonCreatePacketClick(Sender: TObject);
var
  msg1: string;
  description: string;
  buildCall: string = '';
  OpsiBuilderProcess: TProcess;
  packit: boolean = False;
  errorstate: boolean = False;
  notused: string = '(not used)';

begin

  if patchlist = nil then
    patchlist := TStringList.Create
  else
    patchlist.Clear;
  myExeDir := ExtractFileDir(ParamStr(0));


  Panel9.Visible := False;
      {$IFDEF WINDOWS}
  // execute opsiPacketBuilder

  if RadioButtonCreateOnly.Checked = True then
  begin
    Application.MessageBox(PChar(sInfoFinished), PChar(sMBoxHeader), MB_OK);
  end
  else
  begin  // execute OPSIPackageBuilder
    OpsiBuilderProcess := process.TProcess.Create(nil);
    buildCall := getSpecialFolder(CSIDL_PROGRAM_FILES) + DirectorySeparator +
      'OPSI PackageBuilder' + DirectorySeparator +
      'OPSIPackageBuilder.exe' + ' -p=' + packetBaseDir;
    if AnsiLastChar(packetBaseDir) <> DirectorySeparator then
      buildCall := buildCall + DirectorySeparator;
    buildCall := buildCall + productId;
    if RadioButtonInteractive.Checked = True then
    begin
      OpsiBuilderProcess.ShowWindow := swoMinimize;
    end
    else  // auto
    if RadioButtonAuto.Checked = True then
    begin
      if CheckboxBuild.Checked = True then
        buildCall := buildCall + ' --build=rebuild';
      if CheckboxInstall.Checked = True then
        buildCall := buildCall + ' --install';
      if CheckboxQuiet.Checked = True then
        buildCall := buildCall + ' --quiet';
      OpsiBuilderProcess.ShowWindow := swoMinimize;
    end;

    mywrite('invoke opsi package builder');
    mywrite(buildcall);

    try
      OpsiBuilderProcess.CommandLine := buildCall;
      OpsiBuilderProcess.Execute;
      if CheckboxQuiet.Checked = True then
      begin
        Panel9.Visible := True;
        processStatement.Caption := 'invoke opsi package builder ...';
        Application.ProcessMessages;
        while OpsiBuilderProcess.Running do
        begin
          Application.ProcessMessages;
        end;
      end;
    except
      errorstate := True;
      Application.MessageBox(PChar(sErrOpsiPackageBuilderStart),
        PChar(sMBoxHeader), MB_OK);
    end;

    Panel9.Visible := False;
    if (CheckboxQuiet.Checked = True) then
    begin
      if (errorstate = False) then
      begin
        if (OpsiBuilderProcess.ExitStatus = 0) then
          Application.MessageBox(PChar(sInfoFinished), PChar(sMBoxHeader), MB_OK)
        else
          Application.MessageBox(
            PChar(format(sErrOpsiPackageBuilderErrCode, [IntToStr(OpsiBuilderProcess.ExitStatus)])),
            PChar(sMBoxHeader), MB_OK);
      end;
    end;
  end;   // execute OPSIPackageBuilder
      {$ENDIF WINDOWS}

  //  end;
end;

procedure TResultform1.CheckBox1Change(Sender: TObject);
var
  myreg: Tregistry;
begin
  myreg := TRegistry.Create(KEY_ALL_ACCESS);
  myreg.RootKey := HKEY_CURRENT_USER;
  myreg.OpenKey('SOFTWARE\opsi.org\opsisetupdetector', True);
  myreg.WriteBool('regsterAtExplorer', checkbox1.Checked);
  myreg.CloseKey;
  myreg.RootKey := HKEY_CURRENT_USER;
  if checkbox1.Checked then
  begin
    myreg.OpenKey('Software\Classes\*\shell\opsi setup detector\Command', True);
    myreg.WriteString('', '"' + ParamStr(0) + '" --filename=%1');
  end
  else
  begin
    myreg.DeleteKey('Software\Classes\*\shell\opsi setup detector\Command');
    myreg.DeleteKey('Software\Classes\*\shell\opsi setup detector');
  end;
  myreg.CloseKey;
  myreg.Free;
end;

procedure TResultform1.CheckBoxUseMstChange(Sender: TObject);
begin
  (*
  if CheckBoxUseMst.Checked then
  begin
    mst32NameEdit.Enabled:=setup32NameEdit.Enabled;
    mst64NameEdit.Enabled:=setup64NameEdit.Enabled;
    FlowPanelMST.Enabled:=true;
  end
  else
  begin
    mst32NameEdit.Enabled:=false;
    mst64NameEdit.Enabled:=false;
    FlowPanelMST.Enabled:=true;
  end;
  *)
end;

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
  BitBtnAnalyze.Click;
end;

procedure TResultform1.FileExitClick(Sender: TObject);
begin
  BitBtnClose2.Click;
end;

procedure TResultform1.MenuItemAboutClick(Sender: TObject);
var
  progname: string;
  msg: string;
begin
  progname := ExtractFileName(ParamStr(0));
  //msg := 'Version: ' +   myVersion  + '  -  '+Revision + '  -  '+ RevDate;
  msg := 'Version: ' + myVersion;
  //msg := StringReplace (msg, '$', '', [rfReplaceAll,rfIgnoreCase]);

  Application.MessageBox(PChar(msg), PChar(sMBoxHeader), MB_OK);
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
  main;
end;

procedure TResultform1.memoadd(line: string);
begin
  memo1.append(line);
end;


procedure patchScript(infile, outfile: string);
var
  infileHandle, outfileHandle: Text;
  aktline: string;
  i: integer;
begin
  mywrite('creating: ' + outfile + ' from: ' + infile);

  {$I+}//use exceptions
  try
    AssignFile(infileHandle, infile);
    AssignFile(outfileHandle, outfile);
    reset(infileHandle);
    rewrite(outfileHandle);

    while not EOF(infileHandle) do
    begin
      ReadLn(infileHandle, aktline);
      for i := 0 to patchlist.Count - 1 do
        aktline := StringReplace(aktline, patchlist.Names[i],
          patchlist.ValueFromIndex[i], [rfReplaceAll, rfIgnoreCase]);
      writeln(outfileHandle, aktline);
    end;
    CloseFile(infileHandle);
    CloseFile(outfileHandle)
  except
    on E: EInOutError do
      mywrite('patchScript file error: ' + E.ClassName + '/' + E.Message);
  end;
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
  packetBaseDir := Edit_PacketbaseDir.Text;
  if (packetBaseDir <> '') and DirectoryExists(packetBaseDir) then
  begin
    Rewrite(fConfig);
    WriteLn(fConfig, packetBaseDir);
    CloseFile(fConfig);
  end;
  Application.Terminate;
end;


end.
