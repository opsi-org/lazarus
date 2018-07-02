unit resultform;

// {$mode objfpc}{$H+}
{$MODE DELPHI}{$H+}

// ToDo:

// - Description auslesen
// - Eingaben prüfen

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, RTTIGrids,
  //SynEdit, SynMemo,
  Forms, Controls, Graphics,
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
  Windows,
  CustApp,
  ShlObj,
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

  sErrFldInstDirEmpty            = 'Error: Field Install Directory is empty!';
  sErrFldMsiProductCodeEmpty     = 'Error: Field MSI Product Code is empty!';
  sErrFldOpsiProductIdEmpty      = 'Error: Field opsi Packet ID is empty!';
  sErrFldOpsiProductVersionEmpty = 'Error: Field Product Version is empty!';
  sErrFldSetupEmpty              = 'Error: No setup file selected!';
  sErrMSINotFound                = 'Error: MSI file does not exist!';
  sErrMSTNotFound                = 'Error: MST file does not exist in the MSI-directory!';
  sErrOpsiPackageBuilderStart    = 'Error invoking opsi package builder';
  sErrOpsiPackageBuilderErrCode  = 'Error invoking OPSI Package Builder (error code=%0:d). Please remove quiet option to check for further information.';
  sErrPacketBaseDirNotFound      = 'Error: Packet BaseDir does not exist!';
  sErrProductVersionInvalid      = 'Error: for Product Version only numbers allowed !';
  sErrSelectInstallType          = 'First select setup type tab (MSI, Inno Setup ...) and fill out form';
  sErrSetupFileNotFound          = 'Error: Setup file does not exist!';
  sErrPacketDirExists            = 'Error: opsi packet folder %s already exists!';
  sErrExtractMSIfailed           = 'Error: extracting MSI from %s failed!';
  sInfoFinished                  = 'finished.';

  sWarnInstDirEmptyNoDeinstall = 'Warning: Install Directory is empty, deinstall script cannot be patched!';

  sHelpFile = 'languages\Help.de.html';

  sLogfileInfo = 'Logfile created: %s';

type

  { TResultform1 }

  TResultform1 = class(TForm)
    BitBtnDefault: TBitBtn;
    BitBtnMST1: TBitBtn;
    CheckBox3: TCheckBox;
    CheckBox_MSI_License: TCheckBox;
    CheckBox_AdvancedMSI_License: TCheckBox;
    CheckBox_Inno_License: TCheckBox;
    CheckBox_InstallShield_License: TCheckBox;
    CheckBox_InstallShieldMSI_License: TCheckBox;
    CheckBox_Default_License: TCheckBox;
    CheckBox_Nsis_License: TCheckBox;
    ComboBoxArchMode: TComboBox;
    Edit_installer_type: TEdit;
    InstallDirEdit: TDirectoryEdit;
    EditAdvancedMSIFilename: TEdit;
    EditAdvancedMSIFileSize: TEdit;
    EditAdvancedMSIProductCode: TEdit;
    EditAdvancedMSIProductID: TEdit;
    EditAdvancedMSIProductName: TEdit;
    EditAdvancedMSIProductVersion: TEdit;
    EditAdvancedMSIRequiredSpace: TEdit;
    Edit_FileSize: TEdit;
    Edit_opsiProductID: TEdit;
    Edit_MsiId: TEdit;
    Edit_ProductName: TEdit;
    Edit_ProductVersion: TEdit;
    Edit_RequiredSpace: TEdit;
    FileHelp: TMenuItem;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    setup32NameEdit: TFileNameEdit;
    Image7: TImage;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    MemoAdvancedMSIDescription: TMemo;
    MemoDefault: TMemo;
    Panel17: TPanel;
    Panel18: TPanel;
    PanelDefault: TPanel;
    SBtnOpen: TSpeedButton;
    SBtnExit: TSpeedButton;
    setup64NameEdit: TFileNameEdit;
    mst32NameEdit: TFileNameEdit;
    mst64NameEdit: TFileNameEdit;
    SpinEditPackageVersion: TSpinEdit;
    StatusBar1: TStatusBar;
    TabSheetDefault: TTabSheet;
    TabSheetAdvancedMSI: TTabSheet;
    BitBtnAdvancedMSISetup: TBitBtn;
    BitBtnInstallShieldMSISetup: TBitBtn;
    BitBtnNsisSetup: TBitBtn;
    BitBtnInstallShieldSetup: TBitBtn;
    BitBtn_PacketBasedir: TBitBtn;
    BitBtnClose2: TBitBtn;
    BitBtnMSI: TBitBtn;
    BitBtnInno: TBitBtn;
    BitBtnMST: TBitBtn;
    BitBtnAnalyze: TBitBtn;
    ButtonCreatePacket: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBoxBuild: TCheckBox;
    CheckBoxInstall: TCheckBox;
    CheckBoxQuiet: TCheckBox;
    EditInstallShieldMSIFilename: TEdit;
    EditInstallShieldMSIFileSize: TEdit;
    EditInstallShieldMSIProductCode: TEdit;
    EditInstallShieldMSIProductID: TEdit;
    EditInstallShieldMSIProductName: TEdit;
    EditInstallShieldMSIProductVersion: TEdit;
    EditInstallShieldMSIRequiredSpace: TEdit;
    EditNsisFilename: TEdit;
    EditInstallShieldFilename: TEdit;
    EditNsisFileSize: TEdit;
    EditInstallShieldFileSize: TEdit;
    EditNsisInstallDir: TEdit;
    EditInstallShieldInstallDir: TEdit;
    EditNsisProductID: TEdit;
    EditInstallShieldProductID: TEdit;
    EditNsisProductName: TEdit;
    EditInstallShieldProductName: TEdit;
    EditNsisProductVersion: TEdit;
    EditInstallShieldProductVersion: TEdit;
    EditNsisRequiredSpace: TEdit;
    EditMSI_file: TEdit;
    EditInnoInstallDir: TEdit;
    EditInstallShieldRequiredSpace: TEdit;
    Edit_PacketbaseDir: TEdit;
    EditMSI_opsiProductID: TEdit;
    EditInnoProductVersion: TEdit;
    EditInnoRequiredSpace: TEdit;
    EditInnoFileSize: TEdit;
    EditInnoProductID: TEdit;
    EditInnoFilename: TEdit;
    EditMSI_ProductName: TEdit;
    EditMSI_ProductVersion: TEdit;
    EditMSI_ProductCode: TEdit;
    EditInnoProductName: TEdit;
    EditMSI_RequiredSpace: TEdit;
    EditMSI_FileSize: TEdit;
    EditMSI_MSTfile: TEdit;
    GroupBox2: TGroupBox;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label6: TLabel;
    MemoInnoDescription: TMemo;
    FileOpenSetupFile: TMenuItem;
    MemoInstallShieldMSIDescription: TMemo;
    MemoNsisDescription: TMemo;
    MemoInstallShieldDescription: TMemo;
    FileExit: TMenuItem;
    //FileCreateLogfile: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemFile: TMenuItem;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel7: TPanel;
    processStatement: TLabel;
    processing: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelVersion: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MemoMSI: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    RadioButtonAuto: TRadioButton;
    RadioButtonCreateOnly: TRadioButton;
    RadioButtonInteractive: TRadioButton;
    SelectPacketBaseDir: TSelectDirectoryDialog;
    TabSheetInstallShieldMSI: TTabSheet;
    TabSheetAnalyze: TTabSheet;
    TabSheetMSI: TTabSheet;
    TabSheetInstallShield: TTabSheet;
    TabSheetInnoSetup: TTabSheet;
    TabSheetNsis: TTabSheet;
    ToolBar1: TToolBar;

    procedure BitBtn_PacketBasedirClick(Sender: TObject);
    procedure BitBtnClose1Click(Sender: TObject);
    procedure BitBtnAnalyzeClick(Sender: TObject);
    procedure BitBtnAdvancedMSISetupClick(Sender: TObject);
    procedure BitBtnInnoClick(Sender: TObject);
    procedure BitBtnNsisClick(Sender: TObject);
    procedure BitBtnMSIClick(Sender: TObject);
    procedure BitBtnMSTClick(Sender: TObject);
    procedure BitBtnInstallShieldClick(Sender: TObject);
    procedure ButtonCreatePacketClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure ComboBoxArchModeChange(Sender: TObject);
    procedure FileCreateLogfileClick(Sender: TObject);
    //procedure FileCreateLogfileClick(Sender: TObject);
    procedure FileHelpClick(Sender: TObject);
    procedure mst32NameEditChange(Sender: TObject);
    procedure setup32NameEditChange(Sender: TObject);
    procedure FileOpenSetupFileClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure ProductIDChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButtonBuildModeChange(Sender: TObject);
    procedure setDefaultParametersMSI;
    procedure setDefaultParametersInno;
    procedure removeOtherTypeSpecificSections (setupType, setupFile: string);
    function createProductMSI : Boolean;
    function createProductInno : Boolean;
    function createProductNsis : Boolean;
    function createProductInstallShield : Boolean;
    function createProductInstallShieldMSI : Boolean;
    function createProductAdvancedMSI : Boolean;
    function checkFormParameters(installerId : TKnownInstaller) : Boolean;
    function checkFormParametersMSI : Boolean;
    function checkFormParametersInno : Boolean;
    function checkFormParametersNsis : Boolean;
    function checkFormParametersInstallShield : Boolean;
    function checkFormParametersInstallShieldMSI : Boolean;
    function checkFormParametersAdvancedMSI : Boolean;
    function getPacketIDfromFilename(str: string) : string;
    function getPacketIDShort(str: string) : string;
    function ExtractVersion(str: string) : string;
    procedure SBtnOpenClick(Sender: TObject);
    procedure SBtnExitClick(Sender: TObject);
    procedure TabSheetAdvancedMSIEnter(Sender: TObject);
    procedure TabSheetInstallShieldMSIEnter(Sender: TObject);
    procedure TabSheetMSIEnter(Sender: TObject);
    procedure clearAllTabs;
  private
    { private declarations }
  public
    { public declarations }
    procedure memoadd(line: string);
  end;

procedure main;
procedure mywrite(line: string); overload;
procedure mywrite(line: string; loglevel : integer); overload;

var
  resultForm1: TresultForm1;
  optionlist: TStringList;
  myexitcode: Integer;
  result: Integer;
  myExeDir : string;
  myfilename, myerror: string;
  myVersion : string;
  MSIfilename, MSTfilename, SetupFilename: string;
  showgui: boolean;
  configDir: string;
  configFileName: string;
  packetBaseDir: string;
  productid: string;
  patchlist: TStringList;
  fConfig: Text;
  setupTypestr: string;
  markerEmbeddedMSI: boolean = false;
  markerInstallShield: boolean = false;
  ArchitecturesInstallIn64BitMode: string; // INNO: {pf}={pf64}/{pf32}?
  //Revision: string = '$Rev: 126 $';   // set manually in Project-Settings-Version
  //RevDate: string = '$Date: 2014-09-23 17:46:39 +0200 (Di, 23 Sep 2014) $';
  opsidir: string;   // opsi.org (set in main)
  opsitmp: string;   // %TEMP%/opsitmp  (set in main)
  //Logfile: string;   // name of logfile (set in main)

  //*****************************************
  test: boolean = false;
  overwriteExistingProduct: boolean = false;
  showMSI: boolean = true;
  showInnoSetup: boolean = true;
  showNSIS: boolean = true;
  showInstallShield: boolean = true;
  showInstallShieldMSI: boolean = true;
  showAdvancedMSI: boolean = true;
  //*****************************************


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


procedure mywrite(line: string);
begin
  if showgui then
  begin
    resultform1.memoadd(line);
  end
  else
    writeln(line);
end;



procedure grepexe(instring: string);
var lowerstring: string;
begin
  lowerstring := lowercase(instring);
  if
    (0 < pos('installshield', lowerstring)) or
    (0 < pos('inno', lowerstring)) or
    (0 < pos('wise', lowerstring)) or
    (0 < pos('nullsoft', lowerstring)) or
    (0 < pos('wixquery', lowerstring)) or
    (0 < pos('product_build_number{', lowerstring)) or
    (0 < pos('productcode{', lowerstring)) or
    (0 < pos('msiexec', lowerstring)) or
    (0 < pos('extract', lowerstring)) or
    // (0 < pos('setup', lowerstring)) or
    (0 < pos('installer', lowerstring))
    then
       mywrite(instring);
end;


procedure grepmarker(instring: string);
var lowerstring: string;
begin
  lowerstring := lowercase(instring);
  if (0 < pos('installer,msi,database', lowerstring))  then begin
     markerEmbeddedMSI := true;
  end;
  if (0 < pos('installshield', lowerstring)) then begin
     markerInstallShield := true;
  end;
  if (markerEmbeddedMSI and markerInstallShield) then begin
     mywrite('found strings "Installer,MSI,Database" and "InstallShield"');
     mywrite('detected InstallShield+MSI Setup (InstallShield with embedded MSI)');
     setupType := SetupType_InstallShieldMSI;
  end;
  if (markerInstallShield and ((0 < pos('<description>InstallShield.Setup</description>', instring))) or ((0 < pos('InstallShield', instring)))) then begin
     mywrite('found string "<description>InstallShield.Setup</description>" or "InstallShield"');
     mywrite('detected InstallShield Setup');
     setupType := SetupType_InstallShield;
  end;
  if (0 < pos('inno', lowerstring)) and ((0 < pos('<description>inno setup</description>', lowerstring)) or (0 < pos('jr.inno.setup', lowerstring))) then begin
     mywrite('found string "<description>Inno Setup</description>" or "JR.Inno.Setup"');
     mywrite('detected Inno Setup');
     setupType := SetupType_Inno;
  end;
  if (0 < pos('nullsoft', lowerstring)) and ((0 < pos('Nullsoft.NSIS.exehead', lowerstring)) or (0 < pos('nullsoft install system', lowerstring))) then begin
      if (0 < pos('nullsoft.nsis.exehead', lowerstring)) then
         mywrite('found string "Nullsoft.NSIS.exehead"')
      else
         mywrite('found string "Nullsoft Install System"');
     mywrite('detected NSIS Setup');
     setupType := SetupType_NSIS;
  end;
  if (0 < pos('name="microsoft.windows.advancedinstallersetup"', lowerstring))  then begin
     mywrite('found string "microsoft.windows.advancedinstallersetup"');
     mywrite('detected Advanced Setup (with embedded MSI)');
     setupType := SetupType_AdvancedMSI;
  end;
end;


procedure grepmsi(instring: string);
begin
  if (0 < pos('product_build_number{', lowercase(instring))) or
    (0 < pos('productcode{', lowercase(instring))) then
    mywrite(instring);
end;


function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
const
  ReadBufferSize = 2048;

var
  //myStringlist : TStringlist;
  S: TStringList;
  M: TMemoryStream;
  FpcProcess: TProcess;
  n: longint;
  BytesRead: longint;

begin

  Result := True;
  try
    try
      M := TMemoryStream.Create;
      BytesRead := 0;
      FpcProcess := process.TProcess.Create(nil);
      FpcProcess.CommandLine := cmd;
      FpcProcess.Options := [poUsePipes, poStderrToOutput];
      FpcProcess.ShowWindow := swoMinimize;
      FpcProcess.Execute;
      //mywrite('RunCommandAndCaptureOut: started: ' + cmd);
      while FpcProcess.Running do
      begin
        // stellt sicher, dass wir Platz haben
        M.SetSize(BytesRead + ReadBufferSize);

        // versuche, es zu lesen
        if FpcProcess.Output.NumBytesAvailable > 0 then
        begin
          n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
          //mywrite('RunCommandAndCaptureOut: read: ' + IntToStr(n) + ' bytes');
          if n > 0 then
          begin
            Inc(BytesRead, n);
            //mywrite('RunCommandAndCaptureOut: read: ' + IntToStr(n) + ' bytes');
            //Write('.')
          end;
        end
        else
        begin
          // keine Daten, warte 100 ms
          //mywrite('RunCommandAndCaptureOut: no data - waiting....');
          Sleep(100);
        end;
      end;
      // lese den letzten Teil
      repeat
        // stellt sicher, dass wir Platz haben
        M.SetSize(BytesRead + ReadBufferSize);
        if FpcProcess.Output.NumBytesAvailable > 0 then
        begin
          // versuche es zu lesen
          n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
          if n > 0 then
          begin
            Inc(BytesRead, n);
            //mywrite('RunCommandAndCaptureOut: read: ' + IntToStr(n) + ' bytes');
            //Write('.');
          end;
        end
        else
          n := 0;
      until n <= 0;
      //if BytesRead > 0 then WriteLn;
      M.SetSize(BytesRead);
      //mywrite('RunCommandAndCaptureOut: -- executed --');
      //WriteLn('-- executed --');

      S := TStringList.Create;
      S.LoadFromStream(M);
      //mywrite('RunCommandAndCaptureOut: -- linecount = ' + IntToStr(S.Count));
      //WriteLn('-- linecount = ', S.Count, ' --');
      for n := 0 to S.Count - 1 do
      begin
        //WriteLn('| ', S[n]);
        outlines.Add(S[n]);
      end;
      //WriteLn('-- end --');
    except
      on e: Exception do
      begin
        Mywrite('Exception in RunCommandAndCaptureOut: ' +
          e.message);
        Result := False;
      end;
    end;
  finally
    S.Free;
    FpcProcess.Free;
    M.Free;
  end;
end;
*)

procedure mywrite(line: string);
begin
  mywrite(line, LLNotice);
end;

procedure mywrite(line: string; loglevel : integer);
begin
  if showgui then
  begin
    resultform1.memoadd(line);
  end
  else
    writeln(line);
  LogDatei.log(line,loglevel);
end;


procedure TresultForm1.clearAllTabs;
begin
  setupTypestr := '';

  resultForm1.Memo1.Clear;

  resultForm1.EditMSI_file.Text := '';
  resultForm1.EditMSI_opsiProductID.Text := '';
  resultForm1.EditMSI_ProductName.Text := '';
  resultForm1.EditMSI_ProductVersion.Text := '';
  resultForm1.EditMSI_ProductCode.Text := '';
  resultForm1.EditMSI_RequiredSpace.Text := '';
  resultForm1.EditMSI_FileSize.Text := '';
  resultForm1.EditMSI_MSTfile.Text := '';
  resultForm1.CheckBox2.Checked := false;
  resultForm1.CheckBox_MSI_License.Checked := false;
  resultForm1.MemoMSI.Clear;

  resultForm1.EditInnoFilename.Text := '';
  resultForm1.EditInnoProductID.Text := '';
  resultForm1.EditInnoProductName.Text := '';
  resultForm1.EditInnoProductVersion.Text := '';
  resultForm1.EditInnoRequiredSpace.Text := '';
  resultForm1.EditInnoFileSize.Text := '';
  resultForm1.EditInnoInstallDir.Text := '';
  resultForm1.CheckBox_Inno_License.Checked := false;
  resultForm1.MemoInnoDescription.Clear;

  resultForm1.EditNsisFilename.Text := '';
  resultForm1.EditNsisProductID.Text := '';
  resultForm1.EditNsisProductName.Text := '';
  resultForm1.EditNsisProductVersion.Text := '';
  resultForm1.EditNsisRequiredSpace.Text := '';
  resultForm1.EditNsisFileSize.Text := '';
  resultForm1.EditNsisInstallDir.Text := '';
  resultForm1.CheckBox_Nsis_License.Checked := false;
  resultForm1.MemoNsisDescription.Clear;

  resultForm1.EditInstallShieldFilename.Text := '';
  resultForm1.EditInstallShieldProductID.Text := '';
  resultForm1.EditInstallShieldProductName.Text := '';
  resultForm1.EditInstallShieldProductVersion.Text := '';
  resultForm1.EditInstallShieldRequiredSpace.Text := '';
  resultForm1.EditInstallShieldFileSize.Text := '';
  resultForm1.EditInstallShieldInstallDir.Text := '';
  resultForm1.CheckBox_InstallShield_License.Checked := false;
  resultForm1.MemoInstallShieldDescription.Clear;

  resultForm1.EditInstallShieldMSIFilename.Text := '';
  resultForm1.EditInstallShieldMSIProductID.Text := '';
  resultForm1.EditInstallShieldMSIProductName.Text := '';
  resultForm1.EditInstallShieldMSIProductVersion.Text := '';
  resultForm1.EditInstallShieldMSIProductCode.Text := '';
  resultForm1.EditInstallShieldMSIRequiredSpace.Text := '';
  resultForm1.EditInstallShieldMSIFileSize.Text := '';
  resultForm1.CheckBox_InstallShieldMSI_License.Checked := false;
  resultForm1.MemoInstallShieldMSIDescription.Clear;

  resultForm1.EditAdvancedMSIFilename.Text := '';
  resultForm1.EditAdvancedMSIProductID.Text := '';
  resultForm1.EditAdvancedMSIProductName.Text := '';
  resultForm1.EditAdvancedMSIProductVersion.Text := '';
  resultForm1.EditAdvancedMSIProductCode.Text := '';
  resultForm1.EditAdvancedMSIRequiredSpace.Text := '';
  resultForm1.EditAdvancedMSIFileSize.Text := '';
  resultForm1.CheckBox_AdvancedMSI_License.Checked := false;
  resultForm1.MemoAdvancedMSIDescription.Clear;

end;

(*

procedure get_msi_info(myfilename: string);
var
  mycommand : string;
  myoutlines : TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb : double;
  sMsiSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;

begin
  Mywrite('Analyzing MSI: ' + myfilename);

  mycommand := 'cmd.exe /C cscript.exe "'+ExtractFilePath(paramstr(0))+'msiinfo.js" "'+myfilename+'"';
  mywrite (mycommand);
  myoutlines := TStringList.Create;
  if not RunCommandAndCaptureOut (mycommand, true, myoutlines, myreport, SW_SHOWMINIMIZED, myexitcode) then
  begin
    mywrite('Failed to analyze: '+myreport);
  end
  else
  begin
    resultForm1.EditMSI_file.Text := myfilename;
    mywrite('........');
    resultForm1.EditMSI_ProductName.Text := '';
    resultForm1.EditMSI_ProductVersion.Text := '';
    resultForm1.EditMSI_ProductCode.Text := '';
    for i := 0 to myoutlines.Count-1 do
    begin
      mywrite(myoutlines.Strings[i]);

      // sSearch := 'Manufacturer: ';
      // iPos := Pos (sSearch, myoutlines.Strings[i]);
      // if (iPos <> 0) then
      //   resultForm1.Edit2.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

      sSearch := 'ProductName: ';
      iPos := Pos (sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        resultForm1.EditMSI_ProductName.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

      sSearch := 'ProductVersion: ';
      iPos := Pos (sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        resultForm1.EditMSI_ProductVersion.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

      sSearch := 'ProductCode: ';
      iPos := Pos (sSearch, myoutlines.Strings[i]);
      if (iPos <> 0) then
        resultForm1.EditMSI_ProductCode.Text := Copy(myoutlines.Strings[i], Length(sSearch)+1, Length(myoutlines.Strings[i])-Length(sSearch));

    end;
    fsize := fileutil.FileSize(myfilename);
    fsizemb :=  fsize /(1024*1024);
    sMsiSize := FormatFloat('##0.0',fsizemb)+' MB';
    sReqSize := FormatFloat('###0',fsizemb*6)+' MB';

    mywrite('MSI file size is: ' + sMsiSize);
    mywrite('Estimated required space is: ' + sReqSize);
    mywrite('........');

    if fsizemb < 1 then
       fsizemb := 1;

    sMsiSize := FormatFloat('#',fsizemb)+' MB';
    sReqSize := FormatFloat('#',fsizemb*6)+' MB';

    resultForm1.EditMSI_RequiredSpace.Text := sReqSize;
    resultForm1.EditMSI_FileSize.Text := sMsiSize;

    resultForm1.setDefaultParametersMSI;

    mywrite('get_MSI_info finished');
    myoutlines.Free;

  end;
  resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
  if showMSI then
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetMSI;
end;


procedure get_inno_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myCommand : string;
  installScriptISS : string;
  issLine: string = '';
  fISS: Text;
  // components from install_script.iss[setup]
  AppName: string ='';
  AppVersion: string ='';
  AppVerName: string ='';
  DefaultDirName: string ='';

begin
  Mywrite('Analyzing Inno-Setup:');
  Mywrite('extract install_script.iss from ' + myfilename + ' to');

  myoutlines := TStringList.Create;
  destDir := StringReplace (ExtractFileName(myfilename), ExtractFileExt(myfilename), '', [rfReplaceAll,rfIgnoreCase]); // delete Extension
  destDir := configDir + 'INNO' +DirectorySeparator+ StringReplace (destDir, '.', '-', [rfReplaceAll,rfIgnoreCase]);
  installScriptISS := destDir+DirectorySeparator+'install_script.iss';

  Mywrite(installScriptISS);
  // myCommand := 'cmd.exe /C "'+ExtractFilePath(paramstr(0))+'innounp.exe" -x -a -y -d"'+destDir+'" ' +myfilename+ ' install_script.iss';
  myCommand := '"'+ExtractFilePath(paramstr(0))+'innounp.exe" -x -a -y -d"'+destDir+'" ' +myfilename+ ' install_script.iss';
  Mywrite(myCommand);

  if not RunCommandAndCaptureOut(myCommand, true,myoutlines,myreport, SW_SHOWMINIMIZED, myexitcode) then
  begin
    mywrite('Failed to extract install_script.iss: '+myreport);
  end
  else
  begin
    for i := 0 to myoutlines.Count-1 do
      mywrite(myoutlines.Strings[i]);
    myoutlines.Free;

    // read install_script.iss
    if FileExists(installScriptISS) then
    begin
      Mywrite(installScriptISS);
      AssignFile(fISS, installScriptISS);
      Reset(fISS);
      // search [Setup] section
      while (not EOF(fISS)) and (not(0 < pos('[setup]', lowercase(issLine)))) do begin
        ReadLn(fISS,issLine);
      end;

      AppName := '';
      AppVersion := '';
      AppVerName := '';
      DefaultDirName := '';
      ArchitecturesInstallIn64BitMode := '';

      mywrite('........');
      mywrite('[Setup]');
      ReadLn(fISS,issLine);

      // read until next section (usually [Files])
      while (not EOF(fISS)) and (Length(issLine)>0) and (issLine[1] <>'[') do begin
        mywrite(issLine);
        if (0 < pos('appname=', lowercase(issLine))) then
          AppName := Copy(issLine, pos('=',issLine)+1, 100);
        if (0 < pos('appversion=', lowercase(issLine))) then
          AppVersion := Copy(issLine, pos('=',issLine)+1, 100);
        if (0 < pos('appvername=', lowercase(issLine))) then
          AppVerName := Copy(issLine, pos('=',issLine)+1, 100);
        if (0 < pos('defaultdirname=', lowercase(issLine))) then
          DefaultDirName := Copy(issLine, pos('=',issLine)+1, 100);
        if (0 < pos('architecturesinstallin64bitmode=', lowercase(issLine))) then
          ArchitecturesInstallIn64BitMode := Copy(issLine, pos('=',issLine)+1, 100);
        ReadLn(fISS,issLine);
      end;

      // get AppVersion from AppVerName (AppVerName = AppName + AppVersion ?)
      if (AppVersion = '') and (AppVerName <> '') then begin
         AppVersion := StringReplace (AppVerName, AppName, '', []);
         AppVersion := StringReplace (AppVersion, ' ', '', [rfReplaceAll,rfIgnoreCase]);
      end;

      CloseFile(fISS);
    end;

    resultForm1.EditInnoFilename.Text := myfilename;
    mywrite('........');
    resultForm1.EditInnoProductID.Text := resultForm1.getPacketIDShort(AppName);
    resultForm1.EditInnoProductName.Text := AppName;
    resultForm1.EditInnoProductVersion.Text := AppVersion;
    if AppVerName = '' then  begin
       if ((AppName <> '') and (AppVersion <> '')) then
          AppVerName := AppName + ' ' +AppVersion
       else
          AppVerName := AppName + AppVersion
    end;
    if (0 < pos('x64', lowercase(ArchitecturesInstallIn64BitMode))) then
       DefaultDirName := StringReplace (DefaultDirName, '{pf}', '%ProgramFilesSysnativeDir%', [rfReplaceAll,rfIgnoreCase])
    else
       DefaultDirName := StringReplace (DefaultDirName, '{pf}', '%ProgramFiles32Dir%', [rfReplaceAll,rfIgnoreCase]);
    DefaultDirName := StringReplace (DefaultDirName, '{sd}', '%Systemdrive%', [rfReplaceAll,rfIgnoreCase]);
    resultForm1.EditInnoInstallDir.Text := DefaultDirName;  //+'\unins000.exe'

    resultForm1.MemoInnoDescription.Clear;
    resultForm1.MemoInnoDescription.Append(AppVerName);

    fsize := fileutil.FileSize(myfilename);
    fsizemb :=  fsize /(1024*1024);
    sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
    sReqSize := FormatFloat('###0',fsizemb*6)+' MB';

    mywrite('Setup file size is: ' + sFileSize);
    mywrite('Estimated required space is: ' + sReqSize);
    mywrite('........');

    if fsizemb < 1 then
       fsizemb := 1;

    sFileSize := FormatFloat('#',fsizemb)+' MB';
    sReqSize := FormatFloat('#',fsizemb*6)+' MB';

    resultForm1.EditInnoRequiredSpace.Text := sReqSize;
    resultForm1.EditInnoFileSize.Text := sFileSize;

    resultForm1.setDefaultParametersInno;

    mywrite('get_inno_info finished');
    mywrite('Inno Setup detected');

  end;
  if showInnoSetup then
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetInnoSetup;
end;


procedure get_installshield_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;

begin
  Mywrite('Analyzing InstallShield-Setup:');

  resultForm1.EditInstallShieldFilename.Text := myfilename;

  product := ExtractFileNameOnly(myfilename);
  resultForm1.EditInstallShieldProductID.Text := resultForm1.getPacketIDShort(product);
  resultForm1.EditInstallShieldProductName.Text := product;
  resultForm1.EditInstallShieldProductVersion.Text := product;
  if (test) then
    resultForm1.EditInstallShieldInstallDir.Text := '%ProgramFilesSysnativeDir%' +DirectorySeparator+ product;
  resultForm1.MemoInstallShieldDescription.Append(product);

  fsize := fileutil.FileSize(myfilename);
  fsizemb :=  fsize /(1024*1024);
  sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
  sReqSize  := FormatFloat('###0',fsizemb*6)+' MB';

  if fsizemb < 1 then
     fsizemb := 1;

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  resultForm1.EditInstallShieldRequiredSpace.Text := sReqSize;
  resultForm1.EditInstallShieldFileSize.Text := sFileSize;

  mywrite('get_InstallShield_info finished');
  mywrite('InstallShield Setup detected');

  if showInstallShield then
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetInstallShield;
end;


procedure get_installshieldmsi_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;
  FileInfo : TSearchRec;
  exefile: string;
  smask: string;

begin
  Mywrite('Analyzing InstallShield+MSI Setup: ' + myfilename);
  resultForm1.EditInstallShieldMSIFilename.Text := myfilename;

  if DirectoryExists(opsitmp) then
     DeleteDirectory(opsitmp, true);
  if not DirectoryExists(opsitmp) then createdir(opsitmp);
  if not DirectoryExists(opsitmp) then
     mywrite('Error: could not create directory: '+opsitmp);

  // extract and analyze MSI from setup

  Mywrite('Analyzing MSI from InstallShield Setup '+myfilename);

  myoutlines := TStringList.Create;
  // myBatch := 'cmd.exe /C '+ExtractFilePath(paramstr(0))+'extractMSI.cmd "'+myfilename+'"'; // (does not work with spaces in EXE path)
  myBatch := '"'+ExtractFilePath(paramstr(0))+'extractMSI.cmd" "'+myfilename+'"'; // dropped cmd.exe
  Mywrite(myBatch);
  Mywrite('!! PLEASE WAIT !!');
  Mywrite('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
  Mywrite('!! PLEASE WAIT !!');

  product := ExtractFileNameOnly(myfilename);
  resultForm1.EditInstallShieldMSIProductID.Text := product;
  resultForm1.EditInstallShieldMSIProductName.Text := product;
  resultForm1.EditInstallShieldMSIProductVersion.Text := product;
  resultForm1.EditInstallShieldMSIProductCode.Text := 'XXX';

  if not RunCommandAndCaptureOut(myBatch, true,myoutlines,myreport, SW_SHOWMINIMIZED, myexitcode) then
  begin
     mywrite('Failed to to run "' + myBatch +'": '+myreport);
  end
  else
  begin
     smask := opsitmp + '*.msi';
     Mywrite(smask);
     if SysUtils.FindFirst (smask,faAnyFile,FileInfo)=0 then
     begin
       resultform1.EditMSI_file.Text := opsitmp + FileInfo.Name;

       // analyze the extracted MSI
       resultform1.OpenDialog1.InitialDir := opsitmp;
       resultform1.OpenDialog1.FileName := FileInfo.Name;
       get_msi_info(resultform1.EditMSI_file.Text);

       // and use the parameters from get_msi_info (MSI analyze)
       product := resultForm1.EditMSI_opsiProductID.Text;
       resultForm1.EditInstallShieldMSIProductID.Text := product;
       resultForm1.EditInstallShieldMSIProductName.Text := resultForm1.EditMSI_ProductName.Text;
       resultForm1.EditInstallShieldMSIProductVersion.Text := resultForm1.EditMSI_ProductVersion.Text;
       resultForm1.EditInstallShieldMSIProductCode.Text := resultForm1.EditMSI_ProductCode.Text;
       resultForm1.MemoInstallShieldMSIDescription.Append(resultForm1.EditMSI_ProductName.Text);

       // reset OpenDialog parameters
       resultform1.OpenDialog1.InitialDir := myfilename;
       resultform1.OpenDialog1.FileName := ExtractFileNameOnly(myfilename);

       // resultform1.setDefaultParametersMSI;
     end else begin
        Application.MessageBox(pchar(format(sErrExtractMSIfailed, [myfilename])), pchar(sMBoxHeader), MB_OK);
     end
  end;

  fsize := fileutil.FileSize(myfilename);
  fsizemb :=  fsize /(1024*1024);
  sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
  sReqSize  := FormatFloat('###0',fsizemb*6)+' MB';

  if fsizemb < 1 then
     fsizemb := 1;

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  resultForm1.EditInstallShieldMSIRequiredSpace.Text := sReqSize;
  resultForm1.EditInstallShieldMSIFileSize.Text := sFileSize;

  mywrite('get_InstallShield_info finished');
  mywrite('InstallShield+MSI Setup detected');

  if showInstallShieldMSI then
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetInstallShieldMSI;


end;



procedure get_advancedmsi_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;
  FileInfo : TSearchRec;
  exefile: string;
  smask: string;

begin
  Mywrite('Analyzing AdvancedMSI Setup: ' + myfilename);
  resultForm1.EditAdvancedMSIFilename.Text := myfilename;

  // extract and analyze MSI from setup

  Mywrite('Analyzing MSI from Setup '+myfilename);

  myoutlines := TStringList.Create;
  myBatch := 'cmd.exe /C "'+myfilename+'" /extract:'+opsitmp;
  Mywrite(myBatch);
  Mywrite('!! PLEASE WAIT !!');
  Mywrite('!! PLEASE WAIT !! Extracting and analyzing MSI ...');
  Mywrite('!! PLEASE WAIT !!');


  product := ExtractFileNameOnly(myfilename);
  resultForm1.EditAdvancedMSIProductID.Text := product;
  resultForm1.EditAdvancedMSIProductName.Text := product;
  resultForm1.EditAdvancedMSIProductVersion.Text := product;
  resultForm1.EditAdvancedMSIProductCode.Text := 'XXX';

  if DirectoryExists(opsitmp) then
     DeleteDirectory(opsitmp, true);
  if not DirectoryExists(opsitmp) then createdir(opsitmp);
  if not DirectoryExists(opsitmp) then
    mywrite('Error: could not create directory: '+opsitmp);

  if not RunCommandAndCaptureOut(myBatch, true,myoutlines,myreport, SW_SHOWMINIMIZED, myexitcode) then
  begin
    mywrite('Failed to extract MSI: '+myreport);
  end
  else
  begin
    smask := opsitmp+'*.msi';
    Mywrite(smask);
     if SysUtils.FindFirst (smask,faAnyFile,FileInfo)=0 then
     begin
       resultform1.EditMSI_file.Text := opsitmp+FileInfo.Name;

       // analyze the extracted MSI
       resultform1.OpenDialog1.InitialDir := opsitmp;
       resultform1.OpenDialog1.FileName := FileInfo.Name;
       get_msi_info(resultform1.EditMSI_file.Text);

       // and use the parameters from get_msi_info (MSI analyze)
       product := resultForm1.EditMSI_opsiProductID.Text;
       resultForm1.EditAdvancedMSIProductID.Text := product;
       resultForm1.EditAdvancedMSIProductName.Text := resultForm1.EditMSI_ProductName.Text;
       resultForm1.EditAdvancedMSIProductVersion.Text := resultForm1.EditMSI_ProductVersion.Text;
       resultForm1.EditAdvancedMSIProductCode.Text := resultForm1.EditMSI_ProductCode.Text;
       resultForm1.MemoAdvancedMSIDescription.Append(resultForm1.EditMSI_ProductName.Text);

       // reset OpenDialog parameters
       resultform1.OpenDialog1.InitialDir := myfilename;
       resultform1.OpenDialog1.FileName := ExtractFileNameOnly(myfilename);

       // resultform1.setDefaultParametersMSI;
     end;
  end;

  fsize := fileutil.FileSize(myfilename);
  fsizemb :=  fsize /(1024*1024);
  sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
  sReqSize  := FormatFloat('###0',fsizemb*6)+' MB';

  if fsizemb < 1 then
     fsizemb := 1;

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  resultForm1.EditAdvancedMSIRequiredSpace.Text := sReqSize;
  resultForm1.EditAdvancedMSIFileSize.Text := sFileSize;

  mywrite('get_AdvancedMSI_info finished');
  mywrite('Advancd Installer Setup (with embedded MSI) detected');

  if showAdvancedMSI then
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAdvancedMSI;

end;




procedure get_nsis_info(myfilename: string);
var
  myoutlines: TStringList;
  myreport : string;
  myexitcode : integer;
  i: integer;
  fsize : int64;
  fsizemb : double;
  sFileSize : string;
  sReqSize : string;
  sSearch : string;
  iPos : integer;
  destDir : string;
  myBatch: string;
  product: string;

begin
  Mywrite('Analyzing NSIS-Setup:');

  resultForm1.EditNsisFilename.Text := myfilename;

  product := ExtractFileNameOnly(myfilename);
  resultForm1.EditNsisProductID.Text := product;
  resultForm1.EditNsisProductName.Text := product;
  resultForm1.EditNsisProductVersion.Text := resultForm1.ExtractVersion(product);
  if (test) then
    resultForm1.EditNsisInstallDir.Text := '%ProgramFilesSysnativeDir%' +DirectorySeparator+ product;
  resultForm1.MemoNsisDescription.Append(product);

  fsize := fileutil.FileSize(myfilename);
  fsizemb :=  fsize /(1024*1024);
  sFileSize := FormatFloat('##0.0',fsizemb)+' MB';
  sReqSize  := FormatFloat('###0',fsizemb*6)+' MB';

  if fsizemb < 1 then
     fsizemb := 1;

  mywrite('Setup file size is: ' + sFileSize);
  mywrite('Estimated required space is: ' + sReqSize);
  mywrite('........');

  resultForm1.EditNsisRequiredSpace.Text := sReqSize;
  resultForm1.EditNsisFileSize.Text := sFileSize;

  mywrite('get_nsis_info finished');
  mywrite('NSIS (Nullsoft Install System) detected');

  if showNsis then
    resultForm1.PageControl1.ActivePage := resultForm1.TabSheetNsis;
end;



procedure stringsgrep(myfilename: string; verbose,skipzero: boolean);
var
  FileStream: TFileStream;
  CharIn: char;
  MinLen, MaxLen: integer;
  CurrValue: string;
  i: integer;
  size: longint;
  buffer: array [0 .. 2047] of char;
  charsread: longint;
  msg: string;

begin
  MinLen := 5;
  MaxLen := 512;
  CurrValue := '';
  setupType := '';

  mywrite('------------------------------------');
  Mywrite('Analyzing: ' + myfilename);
  msg := 'stringsgrep started (verbose:';
  if verbose=true then msg := msg+'true' else msg:=msg+'false';
  msg := msg + ', skipzero:';
  if skipzero=true then msg := msg+'true' else msg:=msg+'false';
  msg := msg+')';
  mywrite(msg);
  FileStream := TFileStream.Create(myfilename, fmOpenRead);
  markerEmbeddedMSI := false;
  markerInstallShield := false;
  try
    size := FileStream.Size;
    while (size > 0)  and (setupType = '') do
    begin
      charsread := FileStream.Read(buffer, sizeof(buffer));
      size := size - charsread;

      for i := 0 to charsread - 1 do
      begin
        charIn := buffer[i];

        // skipzero: handling of wide strings by ignoring zero byte
        if skipzero and (CharIn = #0) then
           continue;

        // if (CharIn in [' ','A'..'Z','a'..'z','0'..'9','<','>','.','/','_','-']) and (Length(CurrValue) < MaxLen) then
        if (CharIn in [#32..#126]) and (Length(CurrValue) < MaxLen) then
          CurrValue := CurrValue + CharIn;

        if (Length(CurrValue) < MaxLen) and (i < charsread-1) then
           continue;

        if (Length(CurrValue) >= MinLen) then
        begin
          if '.exe' = lowercase(ExtractFileExt(myfilename)) then
          begin
            if verbose then
               grepexe(CurrValue)
            else
               grepmarker(CurrValue);
          end
          else if '.msi' = lowercase(ExtractFileExt(myfilename)) then
          begin
            setupType := setupType_MSI;
            if verbose then
               grepmsi(CurrValue);
          end
          else
          begin
            grepexe(CurrValue);
            grepmsi(CurrValue);
          end;
          CurrValue := '';
        end;
      end;

    end;
    msg := 'stringsgrep completed (verbose:';
    if verbose=true then msg := msg+'true' else msg:=msg+'false';
    msg := msg + ', skipzero:';
    if skipzero=true then msg := msg+'true' else msg:=msg+'false';
    msg := msg+')';
    mywrite(msg);
    mywrite('------------------------------------');
  finally
    FileStream.Free;
  end;
end;


procedure Analyze(FileName:string);
begin
  clearAllTabs;
  setupType := '';
  if '.msi' = lowercase(ExtractFileExt(FileName)) then
    get_msi_info(FileName)
  else begin
    stringsgrep(FileName, false, false); // filename, verbose, skipzero
    if (setupType = SetupType_Inno) then
      get_inno_info(FileName)
    else if (setupType = SetupType_NSIS) then
      get_nsis_info(FileName)
    else if (setupType = SetupType_InstallShield) then
      get_installshield_info(FileName)
    else if (setupType =  SetupType_InstallShieldMSI) then
      get_installshieldmsi_info(FileName)
    else if (setupType = SetupType_AdvancedMSI) then
      get_advancedmsi_info(FileName)
    else begin
      resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
      stringsgrep(FileName, true, false); // filename, verbose, skipzero
      stringsgrep(FileName, true, true); // filename, verbose, skipzero
      mywrite('unknown Setup Type.');   /// XXX Probe-Installation anbieten
    end
  end
end;

*)

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

(*
procedure WriteLogfile;
var
  FileVar: TextFile;
  progname: string;
  msg: string;
begin
  progname := ExtractFileName(ParamStr(0));
  msg := 'Version: ' + myVersion + '  -  '+Revision + '  -  '+ RevDate;
  msg := StringReplace (msg, '$', '', [rfReplaceAll,rfIgnoreCase]);
  mywrite('Write Logfile: '+Logfile);
  if not DirectoryExists(opsitmp) then createdir(opsitmp);
  if not DirectoryExists(opsitmp) then begin
    mywrite('Error: could not create directory: '+opsitmp);
  end
  else begin
    AssignFile(FileVar, Logfile);
    {$I+} //use exceptions
    try
      Rewrite(FileVar);
      Writeln(FileVar,'opsi Setup Detector Logfile');
      Writeln(FileVar,msg);
      Writeln(FileVar,resultForm1.Memo1.Text);
      CloseFile(FileVar);
    except
      on E: EInOutError do
      begin
       Writeln('File handling error occurred. Details: '+E.ClassName+'/'+E.Message);
      end;
    end;
  end;
end;
*)

procedure main;
var
  ErrorMsg: string;
  FileVerInfo: TFileVersionInfo;
  lfilename : string;
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
  lfilename := ExtractFileNameWithoutExt(Application.ExeName);
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
  resultform1.LabelVersion.Caption := 'Version: ' +   myVersion;
  myExeDir := ExtractFileDir(ParamStr(0));
  registerForExplorer;
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
  configDir := GetAppConfigDir(true);   // configDir = "All Users\Appdata\opsi setup detector"
  if not DirectoryExists(configDir) then
     CreateDir(configDir);

  // read config file
  configFileName := configDir +'config.txt';
  AssignFile(fConfig, configFileName);
  if FileExists(configFileName) then
  begin
    Reset(fConfig);
    if not EOF(fConfig) then
       ReadLn(fConfig,packetBaseDir);
    CloseFile(fConfig);
  end;
  if (packetBaseDir = '') or (not DirectoryExists(packetBaseDir)) then
  begin
     packetBaseDir := 'C:' +DirectorySeparator+ 'opsi' +DirectorySeparator+ 'testproducts';
     if DirectoryExists(packetBaseDir) then begin
       Rewrite(fConfig);
       WriteLn(fConfig,packetBaseDir);
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
     resultform1.Edit_PacketbaseDir.text := packetBaseDir;
     resultForm1.PageControl1.ActivePage := resultForm1.TabSheetAnalyze;
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
      Analyze(myfilename)
    end
    else
       analyze_binary(myfilename,false,false);
  end
  else
  begin
    if showgui then begin
      resultForm1.Visible := True;
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
  packetBaseDir := Edit_PacketbaseDir.text;
  if (packetBaseDir <> '') and DirectoryExists(packetBaseDir) then
  begin
    Rewrite(fConfig);
    WriteLn(fConfig,packetBaseDir);
    CloseFile(fConfig);
  end;
  Application.Terminate;
end;

procedure TResultform1.BitBtn_PacketBasedirClick(Sender: TObject);
begin
  SelectPacketBaseDir.InitialDir:= packetBaseDir;
  if SelectPacketBaseDir.Execute then
  begin
    packetBaseDir := SelectPacketBaseDir.FileName;
    Resultform1.Edit_PacketbaseDir.Text := packetBaseDir; // MSI
    Rewrite(fConfig);
    WriteLn(fConfig,packetBaseDir);
    CloseFile(fConfig);
  end;
end;

procedure TResultform1.BitBtnAnalyzeClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 4;   // Any File
  if OpenDialog1.Execute then
  begin
    Analyze(OpenDialog1.FileName);
  end;
end;

procedure TResultform1.BitBtnMSIClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 2;   // MSI
  if OpenDialog1.Execute then
  begin
    Analyze(OpenDialog1.FileName);
  end;
end;

procedure TResultform1.BitBtnMSTClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 3;  // MST
  if OpenDialog1.Execute then begin
     EditMSI_MSTfile.Text := ExtractFileName(OpenDialog1.FileName);
     Checkbox2.Checked := true;
  end;
end;


procedure TResultform1.BitBtnInnoClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 1;   // EXE
  if OpenDialog1.Execute then
  begin
    Analyze(OpenDialog1.FileName);
  end;
end;

procedure TResultform1.BitBtnNsisClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 1;   // EXE
  if OpenDialog1.Execute then
  begin
    Analyze(OpenDialog1.FileName);
  end;
end;

procedure TResultform1.BitBtnInstallShieldClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 1;   // EXE
  if OpenDialog1.Execute then
  begin
    Analyze(OpenDialog1.FileName);
  end;
end;

procedure TResultform1.BitBtnAdvancedMSISetupClick(Sender: TObject);
begin
  OpenDialog1.FilterIndex := 1;   // EXE
  if OpenDialog1.Execute then
  begin
    Analyze(OpenDialog1.FileName);
  end;
end;


procedure TResultform1.setDefaultParametersMSI;
var
  fullName : string;
  opsiProductID : string;
  FileInfo : TSearchRec;
begin

  // set default opsiProductID from ProductName
  opsiProductID := '';
  fullName := Edit_ProductName.text;
  opsiProductID := getPacketIDfromFilename (fullName);
  opsiProductID := getPacketIDShort(opsiProductID);
  Edit_opsiProductID.Text := opsiProductID;
  aktProduct.productId:=opsiProductID;

  // check for MST-file
  mst32nameedit.Text := '';
  Checkbox2.Checked := false;
  If SysUtils.FindFirst (OpenDialog1.InitialDir +'*.mst',faAnyFile,FileInfo)=0 then
  begin
    mst32nameedit.Text := FileInfo.Name;
    aktProduct.mst32FileName:=ExtractFileName(FileInfo.Name);
    aktProduct.mst32FileNamePath:=ExtractFileDir(FileInfo.Name);
    Checkbox2.Checked := true;
  end;
  //MemoMSI.Clear;
  //MemoMSI.Append(EditMSI_ProductName.Text);
end;


procedure TResultform1.setDefaultParametersInno;
var
  fileName : string;
  product : string;
  opsiProductID : string;
  FileInfo : TSearchRec;
begin

  // set default opsiProductID from ProductName
  opsiProductID := Edit_opsiProductID.Text;
  //fileName := ExtractFileName(EditInnoFilename.text);
  filename := aktProduct.setup32FileName;
  product := StringReplace(fileName, '.exe', '',[rfReplaceAll, rfIgnoreCase]);
  product := StringReplace(product, 'setup', '',[rfReplaceAll, rfIgnoreCase]);
  product := StringReplace(product, 'install', '',[rfReplaceAll, rfIgnoreCase]);
  if (opsiProductID = '') then
  begin
    opsiProductID := getPacketIDfromFilename (product);
    opsiProductID := getPacketIDShort(opsiProductID);
    Edit_opsiProductID.Text := opsiProductID;
    aktProduct.productId:=opsiProductID;
  end;
  if EditInnoProductName.Text = '' then
    EditInnoProductName.Text := product;
  if EditInnoProductVersion.Text = '' then
    EditInnoProductVersion.Text := product;
end;



function TResultform1.checkFormParameters(installerId : TKnownInstaller) : Boolean;
var
  str : string;
  i : integer;
begin
   checkFormParameters := false;
   if showMSI then
     PageControl1.ActivePage := resultForm1.TabSheetDefault;

   if not FileExists(setup32NameEdit.text) then begin
     Application.MessageBox(pchar(sErrMSINotFound), pchar(sMBoxHeader), MB_ICONERROR);
     setup32NameEdit.setfocus;
     Exit;
   end;
   if Edit_opsiProductID.Text = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductIdEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     Edit_opsiProductID.SetFocus;
     Exit;
   end;
   if installerId = stMsi then
   begin
     if Edit_msiid.Text = '' then begin
       Application.MessageBox(pchar(sErrFldMsiProductCodeEmpty), pchar(sMBoxHeader), MB_ICONERROR);
       Edit_msiid.SetFocus;
       Exit;
     end;
     if Checkbox2.checked and not FileExists(ExtractFilePath(setup32NameEdit.text)+mst32NameEdit.text) then begin
       Application.MessageBox(pchar(sErrMSTNotFound), pchar(sMBoxHeader), MB_ICONERROR);
       mst32NameEdit.setfocus;
       Exit;
     end;
   end  // end msi
   else
   begin // start setup
     if InstallDirEdit.Text = '' then
     begin
       Application.MessageBox(pchar(sErrFldInstDirEmpty), pchar(sMBoxHeader), MB_ICONERROR);
       InstallDirEdit.SetFocus;
       // no Exit: packing without deinstall available
     end;
     str := Edit_ProductVersion.Text;
   if str = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductVersionEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     Edit_ProductVersion.SetFocus;
     Exit;
   end;
   for i := 1 to Length(str) do
   begin
      if not (str[i] in ['0'..'9','.']) then begin
        Application.MessageBox(pchar(sErrProductVersionInvalid), pchar(sMBoxHeader), MB_ICONERROR);
        Edit_ProductVersion.SetFocus;
        Exit;
      end;
   end;
   end; // end setup

   if not DirectoryExists(Edit_PacketbaseDir.text) then begin
     Application.MessageBox(pchar(sErrPacketBaseDirNotFound), pchar(sMBoxHeader), MB_ICONERROR);
     Edit_PacketbaseDir.setfocus;
     Exit;
   end;

   checkFormParameters := true;
end;



function TResultform1.checkFormParametersMSI : Boolean;

begin
   checkFormParametersMSI := false;
   if showMSI then
     PageControl1.ActivePage := resultForm1.TabSheetDefault;
     //PageControl1.ActivePage := resultForm1.TabSheetMSI;

     (*

   if EditMSI_file.text = '' then begin
     Application.MessageBox(pchar(sErrFldSetupEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     if PageControl1.ActivePage = TabSheetMSI then
        EditMSI_file.setfocus;
     Exit;
   end;
   *)

   if not FileExists(setup32NameEdit.text) then begin
     Application.MessageBox(pchar(sErrMSINotFound), pchar(sMBoxHeader), MB_ICONERROR);
     setup32NameEdit.setfocus;
     Exit;
   end;
   if Edit_opsiProductID.Text = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductIdEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     Edit_opsiProductID.SetFocus;
     Exit;
   end;
   if Edit_msiid.Text = '' then begin
     Application.MessageBox(pchar(sErrFldMsiProductCodeEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     Edit_msiid.SetFocus;
     Exit;
   end;
   if Checkbox2.checked and not FileExists(ExtractFilePath(setup32NameEdit.text)+mst32NameEdit.text) then begin
     Application.MessageBox(pchar(sErrMSTNotFound), pchar(sMBoxHeader), MB_ICONERROR);
     mst32NameEdit.setfocus;
     Exit;
   end;
   if not DirectoryExists(Edit_PacketbaseDir.text) then begin
     Application.MessageBox(pchar(sErrPacketBaseDirNotFound), pchar(sMBoxHeader), MB_ICONERROR);
     Edit_PacketbaseDir.setfocus;
     Exit;
   end;

   checkFormParametersMSI := true;
end;


function TResultform1.checkFormParametersInno : Boolean;

begin
   checkFormParametersInno := false;

   (*
   if showInnoSetup then
     PageControl1.ActivePage := TabSheetInnoSetup;
     *)

   if EditInnoFilename.Text = '' then begin
     Application.MessageBox(pchar(sErrFldSetupEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditInnoFilename.setfocus;
     Exit;
   end;

   if not FileExists(EditInnoFilename.Text) then begin
     Application.MessageBox(pchar(sErrSetupFileNotFound), pchar(sMBoxHeader), MB_ICONERROR);
     EditInnoFilename.setfocus;
     Exit;
   end;
   if EditInnoProductID.Text = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductIdEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditInnoProductID.SetFocus;
     Exit;
   end;
   if EditInnoInstallDir.Text = '' then begin
     Application.MessageBox(pchar(sErrFldInstDirEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditInnoInstallDir.SetFocus;
     Exit;
   end;
   if not DirectoryExists(Edit_PacketbaseDir.text) then begin
    Application.MessageBox(pchar(sErrPacketBaseDirNotFound), pchar(sMBoxHeader), MB_ICONERROR);
    Edit_PacketbaseDir.setfocus;
    Exit;
   end;

   checkFormParametersInno := true;
end;


function TResultform1.checkFormParametersNsis : Boolean;
var str: string = '';
      i: integer;
begin
   checkFormParametersNsis := false;
   if showNsis then
     PageControl1.ActivePage := TabSheetNsis;

   if EditNsisFilename.Text = '' then begin
     Application.MessageBox(pchar(sErrFldSetupEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditNsisFilename.setfocus;
     Exit;
   end;

   if not FileExists(EditNsisFilename.Text) then begin
     Application.MessageBox(pchar(sErrSetupFileNotFound), pchar(sMBoxHeader), MB_ICONERROR);
     EditNsisFilename.setfocus;
     Exit;
   end;
   if EditNsisProductID.Text = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductIdEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditNsisProductID.SetFocus;
     Exit;
   end;

   str := EditNsisProductVersion.Text;
   if str = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductVersionEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditNsisProductVersion.SetFocus;
     Exit;
   end;
   for i := 1 to Length(str) do begin
      if not (str[i] in ['0'..'9','.']) then begin
        Application.MessageBox(pchar(sErrProductVersionInvalid), pchar(sMBoxHeader), MB_ICONERROR);
        EditNsisProductVersion.SetFocus;
        Exit;
      end;
   end;

   if EditNsisInstallDir.Text = '' then begin
     Application.MessageBox(pchar(sWarnInstDirEmptyNoDeinstall), pchar(sMBoxHeader), MB_ICONWARNING);
     EditNsisInstallDir.SetFocus;
     // no Exit: packing without deinstall available
   end;
   if not DirectoryExists(Edit_PacketbaseDir.text) then begin
    Application.MessageBox(pchar(sErrPacketBaseDirNotFound), pchar(sMBoxHeader), MB_ICONERROR);
    Edit_PacketbaseDir.setfocus;
    Exit;
   end;
   checkFormParametersNsis := true;
end;

function TResultform1.checkFormParametersInstallShield : Boolean;
begin
   checkFormParametersInstallShield := false;
   if showInstallShield then
     PageControl1.ActivePage := TabSheetInstallShield;

   if EditInstallShieldFilename.Text = '' then begin
     Application.MessageBox(pchar(sErrFldSetupEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditInstallShieldFilename.setfocus;
     Exit;
   end;

   if not FileExists(EditInstallShieldFilename.Text) then begin
     Application.MessageBox(pchar(sErrSetupFileNotFound), pchar(sMBoxHeader), MB_ICONERROR);
     EditInstallShieldFilename.setfocus;
     Exit;
   end;
   if EditInstallShieldProductID.Text = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductIdEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditInstallShieldProductID.SetFocus;
     Exit;
   end;
   if EditInstallShieldInstallDir.Text = '' then begin
     Application.MessageBox(pchar(sWarnInstDirEmptyNoDeinstall), pchar(sMBoxHeader), MB_ICONWARNING);
     EditInstallShieldInstallDir.SetFocus;
     // no Exit: packing without deinstall available
   end;
   if not DirectoryExists(Edit_PacketbaseDir.text) then begin
    Application.MessageBox(pchar(sErrPacketBaseDirNotFound), pchar(sMBoxHeader), MB_ICONERROR);
    Edit_PacketbaseDir.setfocus;
    Exit;
   end;
   checkFormParametersInstallShield := true;
end;


function TResultform1.checkFormParametersInstallShieldMSI : Boolean;
begin
   checkFormParametersInstallShieldMSI := false;
   if showInstallShieldMSI then
     PageControl1.ActivePage := TabSheetInstallShieldMSI;

   if EditInstallShieldMSIFilename.Text = '' then begin
     Application.MessageBox(pchar(sErrFldSetupEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditInstallShieldMSIFilename.setfocus;
     Exit;
   end;

   if not FileExists(EditInstallShieldMSIFilename.Text) then begin
     Application.MessageBox(pchar(sErrSetupFileNotFound), pchar(sMBoxHeader), MB_ICONERROR);
     EditInstallShieldMSIFilename.setfocus;
     Exit;
   end;
   if EditInstallShieldMSIProductID.Text = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductIdEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditInstallShieldMSIProductID.SetFocus;
     Exit;
   end;
   if EditInstallShieldMSIProductCode.Text = '' then begin
     Application.MessageBox(pchar(sErrFldMsiProductCodeEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditInstallShieldMSIProductCode.SetFocus;
     Exit;
   end;
   if not DirectoryExists(Edit_PacketbaseDir.text) then begin
    Application.MessageBox(pchar(sErrPacketBaseDirNotFound), pchar(sMBoxHeader), MB_ICONERROR);
    Edit_PacketbaseDir.setfocus;
    Exit;
   end;
   checkFormParametersInstallShieldMSI := true;
end;


function TResultform1.checkFormParametersAdvancedMSI : Boolean;
begin
   checkFormParametersAdvancedMSI := false;
   if showAdvancedMSI then
     PageControl1.ActivePage := TabSheetAdvancedMSI;

   if EditAdvancedMSIFilename.Text = '' then begin
     Application.MessageBox(pchar(sErrFldSetupEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditAdvancedMSIFilename.setfocus;
     Exit;
   end;

   if not FileExists(EditAdvancedMSIFilename.Text) then begin
     Application.MessageBox(pchar(sErrSetupFileNotFound), pchar(sMBoxHeader), MB_ICONERROR);
     EditAdvancedMSIFilename.setfocus;
     Exit;
   end;
   if EditAdvancedMSIProductID.Text = '' then begin
     Application.MessageBox(pchar(sErrFldOpsiProductIdEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditAdvancedMSIProductID.SetFocus;
     Exit;
   end;
   if EditAdvancedMSIProductCode.Text = '' then begin
     Application.MessageBox(pchar(sErrFldMsiProductCodeEmpty), pchar(sMBoxHeader), MB_ICONERROR);
     EditAdvancedMSIProductCode.SetFocus;
     Exit;
   end;
   if not DirectoryExists(Edit_PacketbaseDir.text) then begin
    Application.MessageBox(pchar(sErrPacketBaseDirNotFound), pchar(sMBoxHeader), MB_ICONERROR);
    Edit_PacketbaseDir.setfocus;
    Exit;
   end;
   checkFormParametersAdvancedMSI := true;
end;


function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: Integer;
begin
  result := '';
  aPos := pos(A, Value);
  if aPos > 0 then begin
    aPos := aPos + Length(A);
    bPos := PosEx(B, Value, aPos);
    if bPos > 0 then begin
      result := Copy(Value, aPos, bPos - aPos);
    end;
  end;
end;

procedure TResultform1.removeOtherTypeSpecificSections (setupType, setupFile: string);
var
  infileHandle, outfileHandle: Text;
  Patchmarker: string = '**--** PATCH_';
  filterType: string = '';
  thisType: string = '';
  aktline: string = '';
  tmpfile: string;
  isMarkerLine : boolean;
begin
  mywrite('removing all other type specific sections from: '+setupfile);
  tmpfile := setupfile + '.tmp';

  {$I+} //use exceptions
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
             writeln(outfileHandle,aktline);
        until isMarkerLine or EOF(infileHandle);
        if not EOF(infileHandle) then
        begin
           thisType := lowercase(ExtractBetween (aktline, Patchmarker, ' '));
           // write lines (or skip lines if other type)
           repeat
             ReadLn(infileHandle, aktline);
             isMarkerLine := (0 < pos(Patchmarker, aktline));
             if (thisType = filterType) and not isMarkerLine then
               writeln(outfileHandle,aktline);
           until isMarkerLine or EOF(infileHandle);
        end
     end;

     CloseFile(infileHandle);
     CloseFile(outfileHandle);

     // delete setupfile and rename tmpfile to setupfile
     DeleteFile(PChar(setupfile));
     RenameFile(PChar(tmpfile),PChar(setupfile));

  except
     on E: EInOutError do
        mywrite('removeOtherTypeSpecificSections error: '+E.ClassName+'/'+E.Message);
  end;
end;


procedure TResultform1.ButtonCreatePacketClick(Sender: TObject);
var
  msg1 : string;
  description: string;
  buildCall: string = '';
  OpsiBuilderProcess: TProcess;
  packit: boolean = false;
  errorstate: boolean = false;
  notused: string = '(not used)';

begin

    if patchlist = NIL then
       patchlist := TStringList.Create
    else
       patchlist.Clear;
    myExeDir := ExtractFileDir(ParamStr(0));

(*
    if PageControl1.ActivePage = TabSheetAnalyze then begin
      if (setupType = SetupType_MSI) and showMSI then
         PageControl1.ActivePage := TabSheetMSI
      else
      if (setupType = SetupType_Inno) and showInnoSetup then
         PageControl1.ActivePage := TabSheetInnoSetup
      else
      if (setupType = SetupType_NSIS) and showNsis then
         PageControl1.ActivePage := TabSheetNsis
      else
      if (setupType = SetupType_InstallShield) and showInstallShield then
         PageControl1.ActivePage := TabSheetInstallShield
      else
      if (setupType = SetupType_InstallShieldMSI) and showInstallShieldMSI then
         PageControl1.ActivePage := TabSheetInstallShieldMSI
      else
      if (setupType = SetupType_AdvancedMSI) and showAdvancedMSI then
            PageControl1.ActivePage := TabSheetInstallShieldMSI
      else  begin
        if OpenDialog1.FileName = '' then
          Application.MessageBox(pchar(sErrFldSetupEmpty), pchar(sMBoxHeader), MB_OK)
        else
          Application.MessageBox(pchar(sErrSelectInstallType), pchar(sMBoxHeader), MB_OK);
        exit
      end
    end;
    *)
    (*

    if PageControl1.ActivePage = TabSheetMSI then begin
      setupType := SetupType_MSI;
      productId := EditMSI_opsiProductID.text;
      if checkFormParametersMSI then
         msg1 := format(sAskCreatePacket, [productId])
      else
         exit
    end;

    if PageControl1.ActivePage = TabSheetInnoSetup then begin
      setupType := SetupType_Inno;
      productId := EditInnoProductID.text;
      if checkFormParametersInno then
         msg1 := format(sAskCreatePacket, [productId])
      else
         exit
    end;

    if PageControl1.ActivePage = TabSheetNsis then begin
      setupType := SetupType_NSIS;
      productId := EditNsisProductID.text;
      if checkFormParametersNsis then
         msg1 := format(sAskCreatePacket, [productId])
      else
         exit
    end;

    if PageControl1.ActivePage = TabSheetInstallShield then begin
      setupType := SetupType_InstallShield;
      productId := EditInstallShieldProductID.text;
      if checkFormParametersInstallShield then
         msg1 := format(sAskCreatePacket, [productId])
      else
         exit
    end;

    if PageControl1.ActivePage = TabSheetInstallShieldMSI then begin
      setupType := SetupType_InstallShieldMSI;
      productId := EditInstallShieldMSIProductID.text;
      if checkFormParametersInstallShieldMSI then
         msg1 := format(sAskCreatePacket, [productId])
      else
         exit
    end;

    if PageControl1.ActivePage = TabSheetAdvancedMSI then begin
      setupType := SetupType_AdvancedMSI;
      productId := EditAdvancedMSIProductID.text;
      if checkFormParametersAdvancedMSI then
         msg1 := format(sAskCreatePacket, [productId])
      else
         exit
    end;
    *)

    if Application.MessageBox(pchar(msg1), pchar(sMBoxHeader), MB_ICONQUESTION or MB_YESNO) = IDNO then
       exit
    else
       packit := true;
    (*
    if packit then
    begin

      patchlist.Add('#@#-SETUPTYPE-#@#='+lowercase(setupType));


      //***************************************************************************
      if (setupType = SetupType_MSI) then
      begin
        processStatement.Caption:='patch product files ...';
        Panel9.Visible := True;
        Application.ProcessMessages;
        description := MemoMSI.Text;

        patchlist.Add('#@#-PRODUCTID-#@#='     + productId); // EditMSI_opsiProductID
        patchlist.Add('#@#-SETUPFILE-#@#='     + ExtractFileName(EditMSI_file.text));

        patchlist.Add('#@#-PRODUCTDESC-#@#='   + description);
        patchlist.Add('#@#-PRODUCTNAME-#@#='   + EditMSI_ProductName.text);
        patchlist.Add('#@#-PRODUCTSIZE-#@#='   + EditMSI_RequiredSpace.text);
        patchlist.Add('#@#-PRODUCTVER-#@#='    + EditMSI_ProductVersion.text);
        patchlist.Add('#@#-LICENSE-#@#='       + BoolToStr(CheckBox_MSI_License.Checked,'true','false'));

        patchlist.Add('#@#-SETUPSCRIPT-#@#='   + 'setup.opsiscript');
        patchlist.Add('#@#-UNINSSCRIPT-#@#='   + 'uninstall.opsiscript');

        patchlist.Add('#@#-MSIID-#@#='         + EditMSI_ProductCode.text);
        if CheckBox2.Checked then
           patchlist.Add('#@#-USE-MST-#@#=TRANSFORMS="%ScriptPath%' + DirectorySeparator + EditMSI_MSTfile.text +'"')
        else
           patchlist.Add('#@#-USE-MST-#@#=');
        patchlist.Add('#@#-INSTALLDIR-#@#='    + notused);

        patchlist.Add('#@#-TIMESTAMP-#@#='     + DateTimeToStr(Now));

        MSTfilename := EditMSI_file.text;
        MSIfilename := '';
        if Checkbox2.Checked then
           MSIfilename := ExtractFileDir(MSTfilename) + DirectorySeparator + EditMSI_MSTfile.text;

        processStatement.Caption:='create product ...';
        Application.ProcessMessages;
        if not createProductMSI then begin
          Panel9.Visible := False;
          exit
        end
      end;
      //***************************************************************************
      if (setupType = SetupType_Inno) then
      begin
        processStatement.Caption:='patch product files ...';
        Panel9.Visible := True;
        Application.ProcessMessages;

        patchlist.Add('#@#-PRODUCTID-#@#='     + productId);
        patchlist.Add('#@#-SETUPFILE-#@#='     + ExtractFileName(EditInnoFilename.text));

        patchlist.Add('#@#-PRODUCTDESC-#@#='   + MemoInnoDescription.text);
        patchlist.Add('#@#-PRODUCTNAME-#@#='   + EditInnoProductName.text);
        patchlist.Add('#@#-PRODUCTSIZE-#@#='   + EditInnoRequiredSpace.text);
        patchlist.Add('#@#-PRODUCTVER-#@#='    + EditInnoProductVersion.text);
        patchlist.Add('#@#-LICENSE-#@#='       + BoolToStr(CheckBox_Inno_License.Checked,'true','false'));

        patchlist.Add('#@#-SETUPSCRIPT-#@#='   + 'setup.opsiscript');
        patchlist.Add('#@#-UNINSSCRIPT-#@#='   + 'uninstall.opsiscript');

        patchlist.Add('#@#-MSIID-#@#=');
        patchlist.Add('#@#-USE-MST-#@#=');
        patchlist.Add('#@#-INSTALLDIR-#@#='    + EditInnoInstallDir.text);

        patchlist.Add('#@#-TIMESTAMP-#@#='     + DateTimeToStr(Now));

        SetupFilename := EditInnoFilename.Text;
        processStatement.Caption:='create product ...';
        Application.ProcessMessages;
        if not createProductInno then begin
          Panel9.Visible := False;
          exit
        end
      end;
      //***************************************************************************
      if (setupType = SetupType_NSIS) then
      begin
        processStatement.Caption:='patch product files ...';
        Panel9.Visible := True;
        Application.ProcessMessages;

        patchlist.Add('#@#-PRODUCTID-#@#='     + productId);
        patchlist.Add('#@#-SETUPFILE-#@#='     + ExtractFileName(EditNsisFilename.text));

        patchlist.Add('#@#-PRODUCTDESC-#@#='   + MemoNsisDescription.text);
        patchlist.Add('#@#-PRODUCTNAME-#@#='   + EditNsisProductName.text);
        patchlist.Add('#@#-PRODUCTSIZE-#@#='   + EditNsisRequiredSpace.text);
        patchlist.Add('#@#-PRODUCTVER-#@#='    + EditNsisProductVersion.text);
        patchlist.Add('#@#-LICENSE-#@#='       + BoolToStr(CheckBox_Nsis_License.Checked,'true','false'));

        patchlist.Add('#@#-SETUPSCRIPT-#@#='   + 'setup.opsiscript');
        patchlist.Add('#@#-UNINSSCRIPT-#@#='   + 'uninstall.opsiscript');

        patchlist.Add('#@#-MSIID-#@#=');
        patchlist.Add('#@#-USE-MST-#@#=');
        patchlist.Add('#@#-INSTALLDIR-#@#='    + EditNsisInstallDir.text);

        patchlist.Add('#@#-TIMESTAMP-#@#='     + DateTimeToStr(Now));

        SetupFilename := EditNsisFilename.Text;
        processStatement.Caption:='create product ...';
        Application.ProcessMessages;
        if not createProductNsis then begin
          Panel9.Visible := False;
          exit
        end
      end;
      //***************************************************************************
      if (setupType = SetupType_InstallShield) then
      begin
        processStatement.Caption:='patch product files ...';
        Panel9.Visible := True;
        Application.ProcessMessages;

        patchlist.Add('#@#-PRODUCTID-#@#='     + productId);
        patchlist.Add('#@#-SETUPFILE-#@#='     + ExtractFileName(EditInstallShieldFilename.text));

        patchlist.Add('#@#-PRODUCTDESC-#@#='   + MemoInstallShieldDescription.text);
        patchlist.Add('#@#-PRODUCTNAME-#@#='   + EditInstallShieldProductName.text);
        patchlist.Add('#@#-PRODUCTVER-#@#='    + EditInstallShieldProductVersion.text);
        patchlist.Add('#@#-PRODUCTSIZE-#@#='   + EditInstallShieldRequiredSpace.text);
        patchlist.Add('#@#-LICENSE-#@#='       + BoolToStr(CheckBox_InstallShield_License.Checked,'true','false'));

        patchlist.Add('#@#-SETUPSCRIPT-#@#='   + 'setup.opsiscript');
        patchlist.Add('#@#-UNINSSCRIPT-#@#='   + 'uninstall.opsiscript');

        patchlist.Add('#@#-MSIID-#@#=');
        patchlist.Add('#@#-USE-MST-#@#=');
        patchlist.Add('#@#-INSTALLDIR-#@#='    + EditInstallShieldInstallDir.text);

        patchlist.Add('#@#-TIMESTAMP-#@#='     + DateTimeToStr(Now));

        SetupFilename := EditInstallShieldFilename.Text;
        processStatement.Caption:='create product ...';
        Application.ProcessMessages;
        if not createProductInstallShield then begin
          Panel9.Visible := False;
          exit
        end
      end;
      //***************************************************************************
      if (setupType = SetupType_InstallShieldMSI) then
      begin
        processStatement.Caption:='patch product files ...';
        Panel9.Visible := True;
        Application.ProcessMessages;

        patchlist.Add('#@#-PRODUCTID-#@#='     + productId); // EditInstallShieldMSIProductID
        patchlist.Add('#@#-SETUPFILE-#@#=' + ExtractFileName(EditInstallShieldMSIFilename.text));

        patchlist.Add('#@#-PRODUCTDESC-#@#='   + MemoInstallShieldMSIDescription.text);
        patchlist.Add('#@#-PRODUCTNAME-#@#='   + EditInstallShieldMSIProductName.text);
        patchlist.Add('#@#-PRODUCTSIZE-#@#='   + EditInstallShieldMSIRequiredSpace.text);
        patchlist.Add('#@#-PRODUCTVER-#@#='    + EditInstallShieldMSIProductVersion.text);
        patchlist.Add('#@#-LICENSE-#@#='       + BoolToStr(CheckBox_InstallShieldMSI_License.Checked,'true','false'));

        patchlist.Add('#@#-SETUPSCRIPT-#@#='   + 'setup.opsiscript');
        patchlist.Add('#@#-UNINSSCRIPT-#@#='   + 'uninstall.opsiscript');

        patchlist.Add('#@#-MSIID-#@#='         + EditInstallShieldMSIProductCode.text);
        patchlist.Add('#@#-USE-MST-#@#=');
        patchlist.Add('#@#-INSTALLDIR-#@#='    + notused);

        patchlist.Add('#@#-TIMESTAMP-#@#='     + DateTimeToStr(Now));

        SetupFilename := EditInstallShieldMSIFilename.Text;
        processStatement.Caption:='create product ...';
        Application.ProcessMessages;
        if not createProductInstallShieldMSI then begin
          Panel9.Visible := False;
          exit
        end
      end;
      //****************************************************************************
      if (setupType = SetupType_AdvancedMSI) then
      begin
        processStatement.Caption:='patch product files ...';
        Panel9.Visible := True;
        Application.ProcessMessages;

        patchlist.Add('#@#-PRODUCTID-#@#='     + productId); // EditAdvancedMSIProductID
        patchlist.Add('#@#-SETUPFILE-#@#=' + ExtractFileName(EditAdvancedMSIFilename.text));

        patchlist.Add('#@#-PRODUCTDESC-#@#='   + MemoAdvancedMSIDescription.text);
        patchlist.Add('#@#-PRODUCTNAME-#@#='   + EditAdvancedMSIProductName.text);
        patchlist.Add('#@#-PRODUCTSIZE-#@#='   + EditAdvancedMSIRequiredSpace.text);
        patchlist.Add('#@#-PRODUCTVER-#@#='    + EditAdvancedMSIProductVersion.text);
        patchlist.Add('#@#-LICENSE-#@#='       + BoolToStr(CheckBox_AdvancedMSI_License.Checked,'true','false'));

        patchlist.Add('#@#-SETUPSCRIPT-#@#='   + 'setup.opsiscript');
        patchlist.Add('#@#-UNINSSCRIPT-#@#='   + 'uninstall.opsiscript');

        patchlist.Add('#@#-MSIID-#@#='         + EditAdvancedMSIProductCode.text);
        patchlist.Add('#@#-USE-MST-#@#=');
        patchlist.Add('#@#-INSTALLDIR-#@#='    + notused);

        patchlist.Add('#@#-TIMESTAMP-#@#='     + DateTimeToStr(Now));

        SetupFilename := EditAdvancedMSIFilename.Text;
        processStatement.Caption:='create product ...';
        Application.ProcessMessages;
        if not createProductAdvancedMSI then begin
          Panel9.Visible := False;
          exit
        end
      end;
      //****************************************************************************
      *)

      Panel9.Visible := False;
      // execute opsiPacketBuilder

      if RadioButtonCreateOnly.Checked = true then begin
        Application.MessageBox(pchar(sInfoFinished), pchar(sMBoxHeader), MB_OK);
      end
      else begin  // execute OPSIPackageBuilder
        OpsiBuilderProcess := process.TProcess.Create(nil);
        buildCall := getSpecialFolder(CSIDL_PROGRAM_FILES)
           +DirectorySeparator+ 'OPSI PackageBuilder'
           +DirectorySeparator+ 'OPSIPackageBuilder.exe'
           + ' -p='+packetBaseDir;
        if AnsiLastChar(packetBaseDir) <> DirectorySeparator then
           buildCall := buildCall+DirectorySeparator;
        buildCall := buildCall + productId;
        if RadioButtonInteractive.Checked = true then begin
          OpsiBuilderProcess.ShowWindow := swoMinimize;
        end
        else  // auto
          if RadioButtonAuto.Checked = true then begin
            if CheckboxBuild.Checked = true then
              buildCall := buildCall + ' --build=rebuild';
            if CheckboxInstall.Checked = true then
              buildCall := buildCall + ' --install';
            if CheckboxQuiet.Checked = true then
              buildCall := buildCall + ' --quiet';
            OpsiBuilderProcess.ShowWindow := swoMinimize;
          end;

        mywrite ('invoke opsi package builder');
        mywrite (buildcall);

        try
          OpsiBuilderProcess.CommandLine := buildCall;
          OpsiBuilderProcess.Execute;
          if CheckboxQuiet.Checked = true then begin
            Panel9.Visible := True;
            processStatement.Caption:='invoke opsi package builder ...';
            Application.ProcessMessages;
            while OpsiBuilderProcess.Running do begin
              Application.ProcessMessages;
            end
          end;
        except
          errorstate := true;
          Application.MessageBox(pchar(sErrOpsiPackageBuilderStart), pchar(sMBoxHeader), MB_OK);
        end;

        Panel9.Visible := False;
        if (CheckboxQuiet.Checked = true) then begin
          if (errorstate = false) then begin
             if (OpsiBuilderProcess.ExitStatus = 0) then
                Application.MessageBox(pchar(sInfoFinished), pchar(sMBoxHeader), MB_OK)
             else
                Application.MessageBox(pchar(format(sErrOpsiPackageBuilderErrCode, [IntToStr(OpsiBuilderProcess.ExitStatus)])), pchar(sMBoxHeader), MB_OK)
          end
        end
      end   // execute OPSIPackageBuilder
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

procedure TResultform1.CheckBox3Change(Sender: TObject);
begin
  if checkbox3.Checked then
  begin
    mst32NameEdit.Enabled:=setup32NameEdit.Enabled;
    mst64NameEdit.Enabled:=setup64NameEdit.Enabled;
  end
  else
  begin
    mst32NameEdit.Enabled:=false;
    mst64NameEdit.Enabled:=false;
  end;
end;

procedure TResultform1.ComboBoxArchModeChange(Sender: TObject);
var
  modestr : string;
  mode :  TArchitectureMode;
begin
  modestr := TComboBox(sender).Text;
  mode := archModeStrToArchmode(modestr);
  case mode of
    am32only_fix:
      begin
        setup32NameEdit.Enabled := true;
        setup64NameEdit.Enabled := false;
      end;
    am64only_fix:
          begin
        setup32NameEdit.Enabled := false;
        setup64NameEdit.Enabled := true;
      end;
    amBoth_fix, amSystemSpecific_fix, amSelectable:
          begin
        setup32NameEdit.Enabled := true;
        setup64NameEdit.Enabled := true;
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
    OpenDialog1.FilterIndex := 3;  // MST
  if OpenDialog1.Execute then
  begin
     TFileNameEdit(sender).Text := ExtractFileName(OpenDialog1.FileName);
     TFileNameEdit(sender).Hint:=OpenDialog1.FileName;
     aktProduct.mst32FileNamePath:=OpenDialog1.FileName;
     aktProduct.mst32FileName:= ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TResultform1.setup32NameEditChange(Sender: TObject);
begin
   OpenDialog1.FilterIndex := 2;   // MSI
  if OpenDialog1.Execute then
  begin
    Analyze(OpenDialog1.FileName);
  end;
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
  msg := 'Version: ' +   myVersion;
  //msg := StringReplace (msg, '$', '', [rfReplaceAll,rfIgnoreCase]);

  Application.MessageBox(pchar(msg), pchar(sMBoxHeader), MB_OK);
end;

procedure TResultform1.PageControl1Change(Sender: TObject);
begin

end;

procedure TResultform1.Panel1Click(Sender: TObject);
begin

end;

procedure TResultform1.ProductIDChange(Sender: TObject);
var
  CurPos : LongInt;
  inputText, checkedText: string;
  myEdit : TEdit;
begin
  myEdit := Sender as TEdit;
  CurPos := myEdit.SelStart;
  inputText := myEdit.text;
  checkedText := getPacketIDfromFilename (inputText);

  myEdit.text := checkedText;
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
  i : integer;
begin
  mywrite('creating: '+outfile+' from: '+infile);

  {$I+} //use exceptions
  try
     AssignFile(infileHandle, infile);
     AssignFile(outfileHandle, outfile);
     reset(infileHandle);
     rewrite(outfileHandle);

     while not EOF(infileHandle) do
     begin
       ReadLn(infileHandle, aktline);
       for i := 0 to patchlist.Count - 1 do
         aktline := StringReplace(aktline, patchlist.Names[i], patchlist.ValueFromIndex[i], [rfReplaceAll,rfIgnoreCase]);
       writeln(outfileHandle,aktline);
     end;
     CloseFile(infileHandle);
     CloseFile(outfileHandle)
  except
     on E: EInOutError do
        mywrite('patchScript file error: '+E.ClassName+'/'+E.Message);
  end;
end;


function TResultform1.createProductMSI : Boolean;
var
  i: integer;
  productpath, datapath, opsipath, mydatapath, myopsipath : string;
begin
  createProductMSI := false;
  (*
  for i := 0 to patchlist.Count - 1 do
    mywrite(patchlist.Strings[i]);
  productpath := packetBaseDir+DirectorySeparator+productid;
  opsipath := productpath +DirectorySeparator+'OPSI';
  datapath := productpath +DirectorySeparator+'CLIENT_DATA';
  myopsipath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'OPSI';
  mydatapath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'CLIENT_DATA';

  // First delete old version
  if DirectoryExists(productpath) then begin
     if overwriteExistingProduct then
        DeleteDirectory(productpath, true)
     else begin
        Application.MessageBox(pchar(format(sErrPacketDirExists, [productpath])), pchar(sMBoxHeader), MB_OK);
        exit;
     end
  end;

  if not DirectoryExists(productpath) then createdir(productpath);
  if not DirectoryExists(productpath) then
    mywrite('Error: could not create directory: '+productpath)
  else
  begin
    mywrite(datapath);
    if not DirectoryExists(datapath) then createdir(datapath);
    mywrite(opsipath);
    if not DirectoryExists(opsipath) then createdir(opsipath);
    patchScript(myopsipath+DirectorySeparator+'control',opsipath+DirectorySeparator+'control');

    patchScript(mydatapath+DirectorySeparator+'setup.opsiscript',datapath+DirectorySeparator+'setup.opsiscript');
    removeOtherTypeSpecificSections (setupType, datapath+DirectorySeparator+'setup.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'delsub.opsiscript',datapath+DirectorySeparator+'delsub.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'uninstall.opsiscript',datapath+DirectorySeparator+'uninstall.opsiscript');

    copyFile(pchar(mydatapath+DirectorySeparator+'check_msi_exitcode.opsiscript'), pchar(datapath+DirectorySeparator+'check_msi_exitcode.opsiscript'), False);
    copyFile(pchar(mydatapath+DirectorySeparator+'opsi-template.png'), pchar(datapath+DirectorySeparator+productid+'.png'), False);

    copyFile(pchar(MSIfilename), pchar(datapath+DirectorySeparator+ExtractFileName(MSIfilename)), False);
    if Length(MSTfilename) > 0 then
       copyFile(pchar(MSTfilename), pchar(datapath+DirectorySeparator+ExtractFileName(MSTfilename)), False);

    createProductMSI := true;
  end;
  *)
end;


function TResultform1.createProductInno : Boolean;
var
  i: integer;
  productpath, datapath, opsipath, mydatapath, myopsipath : string;
begin
  createProductInno := false;
  (*
  for i := 0 to patchlist.Count - 1 do
    mywrite(patchlist.Strings[i]);
  productpath := packetBaseDir+DirectorySeparator+productid;
  opsipath := productpath +DirectorySeparator+'OPSI';
  datapath := productpath +DirectorySeparator+'CLIENT_DATA';
  myopsipath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'OPSI';
  mydatapath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'CLIENT_DATA';

  // First delete old version
  if DirectoryExists(productpath) then begin
     if overwriteExistingProduct then
        DeleteDirectory(productpath, true)
     else begin
        Application.MessageBox(pchar(format(sErrPacketDirExists, [productpath])), pchar(sMBoxHeader), MB_OK);
        exit;
     end
  end;

  if not DirectoryExists(productpath) then createdir(productpath);
  if not DirectoryExists(productpath) then
    mywrite('Error: could not create directory: '+productpath)
  else
  begin
    mywrite(datapath);
    if not DirectoryExists(datapath) then createdir(datapath);
    mywrite(opsipath);
    if not DirectoryExists(opsipath) then createdir(opsipath);
    patchScript(myopsipath+DirectorySeparator+'control',opsipath+DirectorySeparator+'control');

    // same handling for 32bit and 64 bit - differences are handled by Inno Setup
    patchScript(mydatapath+DirectorySeparator+'setup.opsiscript',datapath+DirectorySeparator+'setup.opsiscript');
    removeOtherTypeSpecificSections (setupType, datapath+DirectorySeparator+'setup.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'delsub.opsiscript',datapath+DirectorySeparator+'delsub.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'uninstall.opsiscript',datapath+DirectorySeparator+'uninstall.opsiscript');

    copyFile(pchar(mydatapath+DirectorySeparator+'check_inno_exitcode.opsiscript'), pchar(datapath+DirectorySeparator+'check_inno_exitcode.opsiscript'), False);
    copyFile(pchar(mydatapath+DirectorySeparator+'opsi-template.png'), pchar(datapath+DirectorySeparator+productid+'.png'), False);
    copyFile(pchar(SetupFilename), pchar(datapath+DirectorySeparator+ExtractFileName(SetupFilename)), False);
  end;
  createProductInno := true;
  *)
end;


function TResultform1.createProductNsis : Boolean;
var
  i: integer;
  productpath, datapath, opsipath, mydatapath, myopsipath : string;
begin
  createProductNsis := false;
  (*
  for i := 0 to patchlist.Count - 1 do
    mywrite(patchlist.Strings[i]);
  productpath := packetBaseDir+DirectorySeparator+productid;
  opsipath := productpath +DirectorySeparator+'OPSI';
  datapath := productpath +DirectorySeparator+'CLIENT_DATA';
  myopsipath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'OPSI';
  mydatapath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'CLIENT_DATA';

  // First delete old version
  if DirectoryExists(productpath) then begin
     if overwriteExistingProduct then
        DeleteDirectory(productpath, true)
     else begin
        Application.MessageBox(pchar(format(sErrPacketDirExists, [productpath])), pchar(sMBoxHeader), MB_OK);
        exit;
     end
  end;

  if not DirectoryExists(productpath) then createdir(productpath);
  if not DirectoryExists(productpath) then
    mywrite('Error: could not create directory: '+productpath)
  else
  begin
    mywrite(datapath);

    if not DirectoryExists(datapath) then createdir(datapath);
    mywrite(opsipath);
    if not DirectoryExists(opsipath) then createdir(opsipath);
    patchScript(myopsipath+DirectorySeparator+'control',opsipath+DirectorySeparator+'control');

    // same handling for 32bit and 64 bit - differences are handled by NSIS Setup
    patchScript(mydatapath+DirectorySeparator+'setup.opsiscript',datapath+DirectorySeparator+'setup.opsiscript');
    removeOtherTypeSpecificSections (setupType, datapath+DirectorySeparator+'setup.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'delsub.opsiscript',datapath+DirectorySeparator+'delsub.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'uninstall.opsiscript',datapath+DirectorySeparator+'uninstall.opsiscript');

    copyFile(pchar(mydatapath+DirectorySeparator+'check_nsis_exitcode.opsiscript'), pchar(datapath+DirectorySeparator+'check_nsis_exitcode.opsiscript'), False);
    copyFile(pchar(mydatapath+DirectorySeparator+'opsi-template.png'), pchar(datapath+DirectorySeparator+productid+'.png'), False);
    copyFile(pchar(SetupFilename), pchar(datapath+DirectorySeparator+ExtractFileName(SetupFilename)), False);
  end;
  createProductNsis := true;
  *)
end;


function TResultform1.createProductInstallShield : Boolean;
var
  i: integer;
  productpath, datapath, opsipath, mydatapath, myopsipath : string;
begin
  createProductInstallShield := false;
  (*
  for i := 0 to patchlist.Count - 1 do
    mywrite(patchlist.Strings[i]);
  productpath := packetBaseDir+DirectorySeparator+productid;
  opsipath := productpath +DirectorySeparator+'OPSI';
  datapath := productpath +DirectorySeparator+'CLIENT_DATA';
  myopsipath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'OPSI';
  mydatapath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'CLIENT_DATA';

  // First delete old version
  if DirectoryExists(productpath) then begin
     if overwriteExistingProduct then
        DeleteDirectory(productpath, true)
     else begin
        Application.MessageBox(pchar(format(sErrPacketDirExists, [productpath])), pchar(sMBoxHeader), MB_OK);
        exit;
     end
  end;

  if not DirectoryExists(productpath) then createdir(productpath);
  if not DirectoryExists(productpath) then
    mywrite('Error: could not create directory: '+productpath)
  else
  begin
    mywrite(datapath);
    if not DirectoryExists(datapath) then createdir(datapath);
    mywrite(opsipath);
    if not DirectoryExists(opsipath) then createdir(opsipath);
    patchScript(myopsipath+DirectorySeparator+'control',opsipath+DirectorySeparator+'control');

    patchScript(mydatapath+DirectorySeparator+'setup.opsiscript',datapath+DirectorySeparator+'setup.opsiscript');
    removeOtherTypeSpecificSections (setupType, datapath+DirectorySeparator+'setup.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'delsub.opsiscript',datapath+DirectorySeparator+'delsub.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'uninstall.opsiscript',datapath+DirectorySeparator+'uninstall.opsiscript');

    copyFile(pchar(mydatapath+DirectorySeparator+'check_InstallShield_exitcode.opsiscript'), pchar(datapath+DirectorySeparator+'check_InstallShield_exitcode.opsiscript'), False);
    copyFile(pchar(mydatapath+DirectorySeparator+'opsi-template.png'), pchar(datapath+DirectorySeparator+productid+'.png'), False);
    copyFile(pchar(SetupFilename), pchar(datapath+DirectorySeparator+ExtractFileName(SetupFilename)), False);
  end;
  createProductInstallShield := true;
  *)
end;


function TResultform1.createProductInstallShieldMSI : Boolean;
var
  i: integer;
  productpath, datapath, opsipath, mydatapath, myopsipath : string;
begin
  createProductInstallShieldMSI := false;
  (*
  for i := 0 to patchlist.Count - 1 do
    mywrite(patchlist.Strings[i]);
  productpath := packetBaseDir+DirectorySeparator+productid;
  opsipath := productpath +DirectorySeparator+'OPSI';
  datapath := productpath +DirectorySeparator+'CLIENT_DATA';
  myopsipath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'OPSI';
  mydatapath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'CLIENT_DATA';

  // First delete old version
  if DirectoryExists(productpath) then begin
     if overwriteExistingProduct then
        DeleteDirectory(productpath, true)
     else begin
        Application.MessageBox(pchar(format(sErrPacketDirExists, [productpath])), pchar(sMBoxHeader), MB_OK);
        exit;
     end
  end;

  if not DirectoryExists(productpath) then createdir(productpath);
  if not DirectoryExists(productpath) then
    mywrite('Error: could not create directory: '+productpath)
  else
  begin
    mywrite(opsipath);

    if not DirectoryExists(opsipath) then createdir(opsipath);
    patchScript(myopsipath+DirectorySeparator+'control',opsipath+DirectorySeparator+'control');

    if not DirectoryExists(datapath) then createdir(datapath);
    patchScript(mydatapath+DirectorySeparator+'setup.opsiscript',datapath+DirectorySeparator+'setup.opsiscript');
    removeOtherTypeSpecificSections (setupType, datapath+DirectorySeparator+'setup.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'delsub.opsiscript',datapath+DirectorySeparator+'delsub.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'uninstall.opsiscript',datapath+DirectorySeparator+'uninstall.opsiscript');

    copyFile(pchar(mydatapath+DirectorySeparator+'check_installshieldmsi_exitcode.opsiscript'), pchar(datapath+DirectorySeparator+'check_installshieldmsi_exitcode.opsiscript'), False);
    copyFile(pchar(mydatapath+DirectorySeparator+'opsi-template.png'), pchar(datapath+DirectorySeparator+productid+'.png'), False);
    copyFile(pchar(SetupFilename), pchar(datapath+DirectorySeparator+ExtractFileName(SetupFilename)), False);
  end;
  createProductInstallShieldMSI := true;
  *)
end;



function TResultform1.createProductAdvancedMSI : Boolean;
var
  i: integer;
  productpath, datapath, opsipath, mydatapath, myopsipath : string;
begin
  createProductAdvancedMSI := false;
  (*
  for i := 0 to patchlist.Count - 1 do
    mywrite(patchlist.Strings[i]);
  productpath := packetBaseDir+DirectorySeparator+productid;
  opsipath := productpath +DirectorySeparator+'OPSI';
  datapath := productpath +DirectorySeparator+'CLIENT_DATA';
  myopsipath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'OPSI';
  mydatapath := myexedir +DirectorySeparator+'files2opsi'+DirectorySeparator+'CLIENT_DATA';

  // First delete old version
  if DirectoryExists(productpath) then begin
     if overwriteExistingProduct then
        DeleteDirectory(productpath, true)
     else begin
        Application.MessageBox(pchar(format(sErrPacketDirExists, [productpath])), pchar(sMBoxHeader), MB_OK);
        exit;
     end
  end;

  if not DirectoryExists(productpath) then createdir(productpath);
  if not DirectoryExists(productpath) then
    mywrite('Error: could not create directory: '+productpath)
  else
  begin
    mywrite(opsipath);

    if not DirectoryExists(opsipath) then createdir(opsipath);
    patchScript(myopsipath+DirectorySeparator+'control',opsipath+DirectorySeparator+'control');

    if not DirectoryExists(datapath) then createdir(datapath);
    patchScript(mydatapath+DirectorySeparator+'setup.opsiscript',datapath+DirectorySeparator+'setup.opsiscript');
    removeOtherTypeSpecificSections (setupType, datapath+DirectorySeparator+'setup.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'delsub.opsiscript',datapath+DirectorySeparator+'delsub.opsiscript');
    patchScript(mydatapath+DirectorySeparator+'uninstall.opsiscript',datapath+DirectorySeparator+'uninstall.opsiscript');

    copyFile(pchar(mydatapath+DirectorySeparator+'check_advancedmsi_exitcode.opsiscript'), pchar(datapath+DirectorySeparator+'check_advancedmsi_exitcode.opsiscript'), False);
    copyFile(pchar(mydatapath+DirectorySeparator+'opsi-template.png'), pchar(datapath+DirectorySeparator+productid+'.png'), False);
    copyFile(pchar(SetupFilename), pchar(datapath+DirectorySeparator+ExtractFileName(SetupFilename)), False);
  end;
  createProductAdvancedMSI := true;
  *)
end;



procedure TResultform1.RadioButtonBuildModeChange(Sender: TObject);
var RadioButtonName: string;
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

function TResultform1.getPacketIDfromFilename(str: string) : string;
var strnew: string;
    i: integer;
    myChar : char;
begin
  strnew := '';
  for i:=1 to Length(str) do
  begin
    myChar := str[i];
    if myChar in ['A'..'Z', 'a'..'z', '0'..'9','_','-'] then
       strnew := strnew + myChar
    else
       if (myChar <> #195) then
          strnew := strnew + '-'
  end;
  getPacketIDfromFilename := strnew;
end;


function TResultform1.getPacketIDShort(str: string) : string;
var strnew: string;
    i: integer;
    myChar : char;
    preChar: char = ' ';
begin
  strnew := '';
  str := lowercase(str);
  for i:=1 to Length(str) do
  begin
    myChar := str[i];
    if myChar in ['a'..'z', '0'..'9','_','-'] then begin
       strnew := strnew + myChar;
       preChar := myChar;
    end
    else begin
       if ((myChar <> #195)
         and (i>1)
         and (strnew[Length(strnew)-1] <> '_')
         and (strnew[Length(strnew)-1] <> '-')
         and (i < Length(str))
         ) then
            if (preChar <> '-') then begin
               strnew := strnew + '-';
               preChar := '-';
            end
    end
  end;
  getPacketIDShort := strnew;
end;


function TResultform1.ExtractVersion(str: string) : string;
var i: integer;
    outstr: string = '';
begin
   str := StringReplace (str, 'w32', '', [rfReplaceAll,rfIgnoreCase]);
   str := StringReplace (str, 'w64', '', [rfReplaceAll,rfIgnoreCase]);
   str := StringReplace (str, 'win32', '', [rfReplaceAll,rfIgnoreCase]);
   str := StringReplace (str, 'win64', '', [rfReplaceAll,rfIgnoreCase]);
   for i:=1 to Length(str) do
   begin
      if str[i] in ['0'..'9','.'] then begin
         if (Length(outstr)>0) and (not (str[i-1] in ['0'..'9','.'])) then
           outstr := '';
         outstr := outstr + str[i]
      end
      else
   end;
   ExtractVersion := outstr;
end;

procedure TResultform1.SBtnOpenClick(Sender: TObject);
begin
    OpenDialog1.FilterIndex := 1;   // EXE
  if OpenDialog1.Execute then
  begin
    Analyze(OpenDialog1.FileName);
  end;
end;

procedure TResultform1.SBtnExitClick(Sender: TObject);
begin
    packetBaseDir := Edit_PacketbaseDir.text;
  if (packetBaseDir <> '') and DirectoryExists(packetBaseDir) then
  begin
    Rewrite(fConfig);
    WriteLn(fConfig,packetBaseDir);
    CloseFile(fConfig);
  end;
  Application.Terminate;
end;


procedure TResultform1.TabSheetMSIEnter(Sender: TObject);
begin
  if EditMSI_file.text <> '' then begin
    // set default path
    resultform1.OpenDialog1.InitialDir := ExtractFilePath(EditMSI_file.text);
    resultform1.OpenDialog1.FileName := ExtractFileName(EditMSI_file.text);
  end;
end;

procedure TResultform1.TabSheetInstallShieldMSIEnter(Sender: TObject);
begin
  if EditInstallShieldMSIFilename.text <> '' then begin
    // set default path
    resultform1.OpenDialog1.InitialDir := ExtractFilePath(EditInstallShieldMSIFilename.text);
    resultform1.OpenDialog1.FileName := ExtractFileName(EditInstallShieldMSIFilename.text);
  end;
end;

procedure TResultform1.TabSheetAdvancedMSIEnter(Sender: TObject);
begin
  if EditAdvancedMSIFilename.text <> '' then begin
    // set default path
    resultform1.OpenDialog1.InitialDir := ExtractFilePath(EditAdvancedMSIFilename.text);
    resultform1.OpenDialog1.FileName := ExtractFileName(EditAdvancedMSIFilename.text);
  end;
end;

end.

