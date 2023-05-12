unit ocrbasedata;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLProc, LResources, TypInfo, Forms, Controls, Graphics,
  Dialogs, StdCtrls,
  strutils,
  {$IFDEF WINDOWS}
  //osdhelper,
  shlobj,
  winpeimagereader,
  {$ENDIF WINDOWS}
  LazFileUtils,
  lazutf8,
  fileinfo,
  fpjsonrtti,
  fpjson,
  oslog,
  //RTTICtrls,
  osjson,
  osregex,
  //lcltranslator,
  oscrypt,
  osparserhelper;

type


  TConfiguration = class(TPersistent)
  private
    Fconfig_version: string;
    { help to detect and handle changes of config file structure }
    Fworkbench_Path: string;
    Fconfig_filled: boolean;
    FService_URL: string;
    FService_user: string;
    FService_pass: string;
    procedure SetWorkbench_path(const AValue: string);
  published
    property config_version: string read Fconfig_version write Fconfig_version;
    property workbench_Path: string read Fworkbench_Path write SetWorkbench_path;
    property config_filled: boolean read Fconfig_filled write Fconfig_filled;
    property Service_URL: string read FService_URL write FService_URL;
    property Service_user: string read FService_user write FService_user;
    property Service_pass: string read FService_pass write FService_pass;

    procedure writeconfig;
    procedure readconfig;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;


(*
function archModeStrToArchmode(modestr: string): TArchitectureMode;
function installerToInstallerstr(installerId: TKnownInstaller): string;
function instIdToint(installerId: TKnownInstaller): integer;
function installerstrToInstaller(installerstr: string): TKnownInstaller;
procedure initaktproduct;
procedure makeProperties;
procedure freebasedata;
procedure activateImportMode;
procedure deactivateImportMode;
function cleanOpsiId(opsiid: string): string; // clean up productId
*)
const
  CONFVERSION = '4.2.0.0';

var
  myconfiguration: TConfiguration;
  myVersion: string;
  aktconfigfile: string;
    lfilename: string;
  (*
  aktProduct: TopsiProduct;
  aktSetupFile: TSetupFile;
  osdsettings: TOSDSettings;
  knownInstallerList: TStringList;
  architectureModeList: TStringList;
  templateChannelList: TStringList;
  installerArray: TInstallers;
  counter: integer;
  myconfiguration: TConfiguration;
  //useRunMode: TRunMode;
  myVersion: string;
  lfilename: string;
  aktconfigfile: string;
  forceProductId: string = ''; // by cli parameter
  forceTargetOS: TTargetOS = osWin; // by cli parameter
  globimportMode: boolean = False;
  defaultIconFullFileName: string;
  *)
resourcestring

  // new for 4.1.0.2 ******************************************************************
  rsworkbench_Path = 'Path to the opsi_workbench';
  //rsPreInstallLines = 'opsi-script code, that will be included before the start of the installation.';
  rsworkbench_mounted =
    'Automatically detected. Is the opsi workbench reachable at workbench_Path.';
  rsconfig_filled = 'Automatically detected. Do we have all needed configurations';
  rsregisterInFilemanager =
    'Should this program be registred to the Filemanger (Explorer) context menu ?';
  rsemail_address = 'Your email address, used for the changelog entry';
  rsfullName = 'Your full name, used for the changelog entry';
  rsimport_libraries = 'List of opsi-script libraries that have to be imported.' +
    LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'myinstallhelperlib.opsiscript';
  rspreInstallLines =
    'List of opsi-script code lines that should be included before the installation starts. '
    + LineEnding + 'One per line. May be empty. Example: ' + LineEnding +
    'comment "Start the installation ..."';
  rspostInstallLines =
    'List of opsi-script code lines that should be included after the installation finished.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Installation finished..."';
  rspreUninstallLines =
    'List of opsi-script code lines that should be included before the uninstallation starts.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Start the uninstallation ..."';
  rspostUninstallLines =
    'List of opsi-script code lines that should be included after the uninstallation finished.'
    + LineEnding + 'One per line. May be empty. Example:' + LineEnding +
    'comment "Uninstall finished..."';
  rspathToOpsiPackageBuilder =
    'Path to the OpsiPackageBuilder. OpsiPackageBuilder is used to build the opsi packages via ssh. see: https://forum.opsi.org/viewtopic.php?f=22&t=7573';
  rscreateRadioIndex = 'selects the Create mode Radiobutton.';
  rsBuildRadioIndex = 'selects the Build mode Radiobutton.';
  rsCnfdTitle = 'Edit your configuration here.' + LineEnding +
    'Click on a line to get help ' + LineEnding +
    'in the yellow field at the bottom.';
  rsService_URL = 'The URL of the opsi webservice (like: https://<opsi-server>:4447)';
  rsService_user = 'The user name used to connect to the opsi webservice';
  rsService_pass = 'The password of the given user used to connect to the opsi webservice'
    + LineEnding + 'ATTENTION SECURITY RISK: Even it is stored encrypted,' +
    LineEnding + 'it is easy to decrypt be analyzing the source code.' +
    LineEnding + 'If empty you will be asked if the connection is started.';
  //rsUseService = 'Should this program try to connect to the opsi web service';
  rsConfigVersion = 'Do not change. Version of the config structure.';
  rsReadme_txt_templ = 'Path to the text file that is used as Read-me template';
  rsInternalSet = 'Do not change here - Internally set by a dialog.';
  rsUsePropDesktopicon =
    'Should we create a "DesktopIcon" property and add code to handle desktop icons ?';
  rsPropDesktopiconDescription = 'Should there be a desktop icon ?';
  rsUsePropLicenseOrPool =
    'Should we create a "LicenseOrPool" property and add code handle license keys ?';
  rsPropLicenseOrPoolDescription = 'License key or opsi license pool';
  rsPropInstallFromLocalDescription =
    'Determines if the installation files will be copied locally';
  rsPropInstallArchDescription = 'Which architecture (32 / 64 Bit) has to be installed?';
  rsPreferSilent =
    'Should really silent installs (with no output) be preferred ? Default is false = unattended is preferred.';


implementation


var
  FileVerInfo: TFileVersionInfo;


// TConfiguration ************************************

constructor TConfiguration.Create;
begin
  inherited;
  Fworkbench_Path := 'missing';
  Fconfig_filled := False;
  Fconfig_version := myVersion;
end;

destructor TConfiguration.Destroy;
begin
  inherited;
end;


procedure TConfiguration.SetWorkbench_path(const AValue: string);
begin
  Fworkbench_Path := IncludeTrailingPathDelimiter(AValue);
end;

procedure TConfiguration.writeconfig;
var
  Streamer: TJSONStreamer;
  JSONString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: utf8string;
  //myfile: TextFile;

  // http://wiki.freepascal.org/File_Handling_In_Pascal
  // SaveStringToFile: function to store a string of text into a diskfile.
  //   If the function result equals true, the string was written ok.
  //   If not then there was some kind of error.
  function SaveStringToFile(theString, filePath: ansistring): boolean;
  var
    fsOut: TFileStream;
  begin
    // By default assume the writing will fail.
    Result := False;

    // Write the given string to a file, catching potential errors in the process.
    try
      fsOut := TFileStream.Create(filePath, fmCreate);
      fsOut.Write(theString[1], length(theString));
      fsOut.Free;

      // At his point it is known that the writing went ok.
      Result := True

    except
      on E: Exception do
        LogDatei.log('String could not be written. Details: ' +
          E.ClassName + ': ' + E.Message, LLError);
    end;
  end;

begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start writeconfig', LLDebug);
    configDir := '';
  {$IFDEF Windows}
    SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, configDir);
    configDir := configDir + PathDelim + 'opsi.org' + PathDelim;
    configDirUtf8 := WinCPToUTF8(configDir);
  {$ELSE}
    configDir := GetAppConfigDir(False);
    configDirUtf8 := configDir;
  {$ENDIF WINDOWS}
  (*
    configDirUtf8 := StringReplace(configDirUtf8, 'opsi-config-rules-manager',
      'opsi.org', [rfReplaceAll]);
    configDirUtf8 := StringReplace(configDirUtf8, 'opsisetupdetector',
      'opsi.org', [rfReplaceAll]);
      *)
    myfilename := configDirUtf8 + PathDelim + 'opsi-config-rules-manager.cfg';
    myfilename := ExpandFileName(myfilename);
    if Assigned(logdatei) then
      logdatei.log('writeconfig to: ' + myfilename, LLDebug);
    if not DirectoryExists(configDirUtf8) then
      if not ForceDirectories(configDirUtf8) then
        if Assigned(logdatei) then
          LogDatei.log('failed to create configuration directory: ' +
            configDirUtf8, LLError);
     (*
    if (Femail_address = 'missing') or (FFullName = 'missing') or
      (Fworkbench_Path = 'missing') then
      //or (FPathToOpsiPackageBuilder = 'missing') then
      Fconfig_filled := False
    else
      Fconfig_filled := True;
     *)
    Fconfig_version := CONFVERSION;

    FService_pass := encryptStringBlow('opsi-config-rules-manager' +
      FService_user, FService_pass);
    // http://wiki.freepascal.org/Streaming_JSON
    Streamer := TJSONStreamer.Create(nil);
    try
      Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
      // Save strings as JSON array
      // JSON convert and output
      JSONString := Streamer.ObjectToJSONString(myconfiguration);
      logdatei.log('Config: ' + JSONString, LLDebug);
      if not SaveStringToFile(JSONString, myfilename) then
        if Assigned(logdatei) then
          LogDatei.log('failed save configuration', LLError);
    finally
      //CloseFile(myfile);
      Streamer.Destroy;
      FService_pass := decryptStringBlow('opsi-config-rules-manager' +
        FService_user, FService_pass);
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished writeconfig', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
        LogDatei.log('Configuration could not be written. Details: ' +
          E.ClassName + ': ' + E.Message, LLError);
  end;
end;


procedure TConfiguration.readconfig;
var
  DeStreamer: TJSONDeStreamer;
  //Streamer: TJSONStreamer;
  JSONString: string;
  myfilename: string;
  configDir: array[0..MaxPathLen] of char; //Allocate memory
  configDirUtf8: utf8string;
  configDirstr: string;
  myfile: Text;
  oldconfigDir, oldconfigFileName, tmpstr: string;
  fConfig: Text;

  // http://wiki.freepascal.org/File_Handling_In_Pascal
  // LoadStringFromFile: function to load a string of text from a diskfile.
  //   If the function result equals true, the string was load ok.
  //   If not then there was some kind of error.
  function LoadStringFromFile(theString, filePath: ansistring): boolean;
  var
    fsOut: TFileStream;
  begin
    // By default assume the writing will fail.
    Result := False;

    // Write the given string to a file, catching potential errors in the process.
    try
      fsOut := TFileStream.Create(filePath, fmOpenRead);
      fsOut.Read(theString[1], length(theString));
      fsOut.Free;

      // At his point it is known that the writing went ok.
      Result := True

    except
      on E: Exception do
        if Assigned(logdatei) then
          LogDatei.log('String could not be read. Details: ' +
            E.ClassName + ': ' + E.Message, LLError)
        else
          ShowMessage('readconfig: String could not be read. Details: ' +
            E.ClassName + ': ' + E.Message);
    end;
  end;

begin
  try
    if Assigned(logdatei) then
      logdatei.log('Start readconfig', LLDebug);
    configDir := '';
  {$IFDEF Windows}
    SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, configDir);
    configDir := configDir + PathDelim + 'opsi.org' + PathDelim;
    configDirUtf8 := WinCPToUTF8(configDir);
  {$ELSE}
    configDir := GetAppConfigDir(False);
    configDirUtf8 := configDir;
  {$ENDIF WINDOWS}
  (*
    configDirUtf8 := StringReplace(configDirUtf8, 'opsi-setup-detector',
      'opsi.org', [rfReplaceAll]);
    configDirUtf8 := StringReplace(configDirUtf8, 'opsisetupdetector',
      'opsi.org', [rfReplaceAll]);
      *)
    myfilename := configDirUtf8 + PathDelim + 'opsi-config-rules-manager.cfg';
    myfilename := ExpandFileName(myfilename);
    if Assigned(logdatei) then
      logdatei.log('readconfig from: ' + myfilename, LLDebug);
    if FileExists(myfilename) then
    begin
      aktconfigfile := myfilename;
      AssignFile(myfile, myfilename);
      Reset(myfile);
      readln(myfile, JSONString);
      //// http://wiki.freepascal.org/Streaming_JSON
      // DeStreamer object create
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        // Load JSON data in the object
        DeStreamer.JSONToObject(JSONString, myconfiguration);
        // Cleanup
      finally
        DeStreamer.Destroy;
        CloseFile(myfile);
        FService_pass := decryptStringBlow('opsi-config-rules-manager' +
          FService_user, FService_pass);
      end;
      if Assigned(logdatei) then
        logdatei.log('read config: ' + JSONString, LLDebug)
      else
        ShowMessage('read config: ' + JSONString);
    end
    else
    begin
      tmpstr := '';
      // check for old config
      // get global ConfigDir
      oldconfigDir := GetAppConfigDir(True);
      oldconfigFileName := oldconfigDir + 'config.txt';
      if FileExists(oldconfigFileName) then
      begin
        AssignFile(fConfig, oldconfigFileName);
        Reset(fConfig);
        if not EOF(fConfig) then
          ReadLn(fConfig, tmpstr);
        CloseFile(fConfig);
      end;
    end;
    if Assigned(logdatei) then
      logdatei.log('Finished readconfig', LLDebug2);

  except
    on E: Exception do
      if Assigned(logdatei) then
        LogDatei.log('read config exception. Details: ' + E.ClassName +
          ': ' + E.Message, LLError)
      else
        ShowMessage('read config exception. Details: ' + E.ClassName +
          ': ' + E.Message);
  end;
end;


procedure freebasedata;
var
  i: integer;
begin
  if Assigned(myconfiguration) then
  begin
    myconfiguration.writeconfig;
    myconfiguration.Destroy;
  end;
end;





//initialize unit

begin

  myconfiguration := TConfiguration.Create;


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
  LogDatei.log('opsi-config-rules-manager Version: ' + myVersion, LLEssential);
  LogDatei.LogLevel := 9;

  myconfiguration.readconfig;
  LogDatei.log('Finished initialize basedata ', LLInfo);
end.
