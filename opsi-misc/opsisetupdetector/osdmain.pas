unit osdmain;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF WINDOWS}
  lazfileutils,
  FileUtil,
  LCLType,
  LclIntf,
  StrUtils,
  typinfo,
  fileinfo,
  winpeimagereader,
  osversioninfo,
  //{$IFNDEF WINDOWS}
  {$IFDEF OSDGUI}
  Forms,
  Dialogs,
  Controls,
  Graphics,
  osddlgnewdependency,
  osddlgnewproperty,
  {$ELSE OSDGUI}
  CustApp,
  {$ENDIF OSDGUI}
  //{$ENDIF WINDOWS}
  osdanalyzewin,
  osdanalyzelin,
  osdanalyzemac,
  lcltranslator,
  oslog,
  osdbasedata,
  osdcreate,
  fpjsonrtti,
  osparserhelper,
  osddatamod,
  Contnrs,
  oswebservice;

{$IFNDEF OSDGUI}
type
  { TOSD }

  TOSD = class(TCustomApplication)
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    //procedure WriteHelp; virtual;
  published
    procedure DoRun; override;
  end;

var
  Application: TOSD;

{$ENDIF OSDGUI}




procedure main;
function checkAktProduct: boolean;
procedure WriteHelp;
procedure write_log_and_memo(line: string); overload;
procedure write_log_and_memo(line: string; loglevel: integer); overload;
//procedure checkWorkbench;
procedure procmess;
procedure AppTerminate;
function startOpsiServiceConnection: boolean;

implementation

{$IFDEF OSDGUI}
uses
  osdform;

{$ENDIF OSDGUI}

{$IFNDEF OSDGUI}
(*
type
{ TOSD }

  TOSD = class(TCustomApplication)
protected
  procedure DoRun; override;
public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
  //procedure WriteHelp; virtual;
end;

var
Application: TOSD;
*)

{ TOSD }
constructor TOSD.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TOSD.Destroy;
begin
  inherited Destroy;
end;



procedure initLogging;
var
  logfilename: string;
begin
  logdatei := TLogInfo.Create;
  //logfilename := ExtractFileNameOnly(ParamStr(0)) + '.log';
  logfilename := logdatei.StandardMainLogPath + 'opsi-script-gui-test.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile := False;
  LogDatei.WriteHistFile := False;
  LogDatei.WriteComponentFile := False;
  ;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := 8;
  //LogDatei.debug_prog:=true;
  logdatei.log('opsi-script version: ' + getversioninfo, LLessential);
  logdatei.log('Called as: ' + ExtractFileNameOnly(ParamStr(0)), LLessential);
  logdatei.log('Compiled with FPC: ' + {$i %FPCVERSION%} + ' for: ' +
    {$i %FPCTARGETOS%} + '-' + {$i %FPCTARGETCPU%}, LLessential);
end;

{$IFDEF UNIX}
procedure TOSD.DoRun;
var
  silent: boolean;
  filepath: string;
  i: integer;
  param: string;
  paramdel: string;
  startgui: boolean;
begin
  silent := False;
  paramdel := '-';
  startgui := False;
  initLogging;
  filePath := ExtractFilePath(ParamStr(0));
  filepath := IncludeTrailingPathDelimiter(filepath);
  if not FileExists(filePath + 'opsi-script-gui') then
    if which('opsi-script-gui', filepath) then
      filePath := ExtractFilePath(filepath);
  if not FileExists(filePath + 'opsi-script-gui') then
    filepath := '/usr/local/bin/';
  if not FileExists(filePath + 'opsi-script-gui') then
    filepath := '/Applications/opsi-script.app/Contents/MacOS/';
  if not FileExists(filePath + 'opsi-script-gui') then
    filepath := '/usr/bin/';
  //logdatei.log('Launch: paramcount ' + Paramcount.ToString, LLessential);
  i := 1;
  while (i <= Paramcount) do
  begin
    param := ParamStr(i);
    if LowerCase(param) = paramdel + 'silent' then
      silent := True;
    Inc(i);
  end;
  //logdatei.log('Found Params: '+executeparamlist.Text,LLnotice);
  if not silent then
  begin
    if check_gui_startable() then
    begin
      logdatei.log('gui ok ... ', LLnotice);
      startgui := True;
      {$IFDEF DARWIN}
      if getLoggedInUser = '' then
      begin
        startgui := False;
        logdatei.log('No logon at macos -  continue with nogui... ', LLnotice);
      end;
      {$ENDIF DARWIN}
      if startgui then
      begin
        if FileExists(filePath + 'opsi-script-gui') then
          logdatei.log('starting opsi-script-gui ... ', LLnotice)
        else
          logdatei.log('No opsi-script-gui: ' + filePath +
            'opsi-script-gui' + '  -  continue with nogui... ', LLnotice);
        logdatei.Close;
        if FileExists(filePath + 'opsi-script-gui') then
        begin
          fpExecV(filePath + 'opsi-script-gui', argv);
          opsiscriptProcName := 'opsi-script-gui';
        end;
      end;
    end
    else
    begin
      logdatei.log('no gui access - continue with nogui ... ', LLnotice);
      logdatei.Close;
    end;
  end
  else
  begin
    logdatei.log('Parameter silent found - continue with nogui ... ', LLnotice);
    logdatei.Close;
  end;
  osdmain.main;
end;

{$ENDIF UNIX}
{$IFDEF WINDOWS}
procedure TOSD.DoRun;
begin
  osdmain.main;
end;

{$ENDIF WINDOWS}
{$ENDIF OSDGUI}

procedure procmess;
begin
  {$IFDEF OSDGUI}
  Application.ProcessMessages;
  {$ENDIF OSDGUI}
end;

procedure AppTerminate;
begin
  Application.Terminate;
end;

procedure write_log_and_memo(line: string);
begin
  write_log_and_memo(line, LLNotice);
end;

procedure write_log_and_memo(line: string; loglevel: integer);
begin
  {$IFDEF OSDGUI}
  if osdsettings.showgui then
  begin
    resultform1.memoadd(line);
  end;
  {$ENDIF OSDGUI}
  LogDatei.log(line, loglevel);
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


procedure WriteHelp;
var
  progname: string;
  helplist: TStringList;
begin
  progname := ExtractFileName(ParamStr(0));
  if osdsettings.showgui then
  begin
    {$IFDEF OSDGUI}
    helplist := TStringList.Create;
    helplist.Append(ParamStr(0));
    helplist.Append(progname);
    helplist.Append('Version ' + myVersion);
    //helplist.Append(myerror);
    helplist.Append('Usage:');
    helplist.Append(progname + '[Options]');
    helplist.Append('Options:');
    helplist.Append(' --help -> write this help and exit');
    helplist.Append('  -h -> write this help and exit');
    helplist.Append(' --filename=<path\filename> -> file to analyze)');
    helplist.Append(' --f <path\filename> -> file to analyze)');
    helplist.Append(' --nogui -> do not show interactive output window)');
    helplist.Append(' --n -> do not show interactive output window)');
    helplist.Append(
      ' --targetOS=<os> -> Analyze for target where <os> is on of (win,lin,mac)');
    helplist.Append(' --t <os> -> Analyze for target where <os> is on of (win,lin,mac)');
    helplist.Append(' --productID=<id> -> Create product with productID <id>');
    helplist.Append(' --p <id> -> Create product with productID <id>');
    helplist.Append(
      ' --mode=<mode> -> Define tho run mode <mode> (default=singleAnalyzeCreate)');
    helplist.Append(' --m <mode> -> Define tho run mode <mode> (default=singleAnalyzeCreate)');
    helplist.Append(
      '     possible modes are: singleAnalyzeCreate, createTemplate');
    helplist.Append(
      ' --template-channel=<channel> -> Create product from template channel <channel>');
    helplist.Append(' --c <channel> -> Create product from template channel <channel>');
    helplist.Append('     possible channels are: training, default, structured, custom');
    ShowMessage(helplist.Text);
    FreeAndNil(helplist);
    {$ENDIF OSDGUI}
  end
  else
  begin
    writeln(ParamStr(0));
    writeln(progname);
    writeln('Version ' + myVersion);
    writeln('Usage:');
    writeln(progname + '[Options]');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    writeln('  -h -> write this help and exit');
    writeln(' --filename=<path\filename> -> file to analyze)');
    writeln(' --f <path\filename> -> file to analyze)');
    writeln(' --nogui -> do not show interactive output window)');
    writeln(' --n -> do not show interactive output window)');
    writeln(' --targetOS=<os> -> Analyze for target where <os> is on of (win,lin,mac)');
    writeln(' --t <os> -> Analyze for target where <os> is on of (win,lin,mac)');
    writeln(' --productID=<id> -> Create product with productID <id>');
    writeln(' --p <id> -> Create product with productID <id>');
    writeln(' --mode=<mode> -> Define tho run mode <mode> (default=singleAnalyzeCreate)');
    writeln(' --m <mode> -> Define tho run mode <mode> (default=singleAnalyzeCreate)');
    writeln('     possible modes are: singleAnalyzeCreate, createTemplate');
    writeln(' --template-channel=<channel> -> Create product from template channel <channel>');
    writeln(' --c <channel> -> Create product from template channel <channel>');
    writeln('     possible channels are: training, default, structured, custom');
  end;

  Application.Terminate;
  halt(-1);
  Exit;
end;


function checkAktProduct: boolean;
begin
  Result := True;
  if not Assigned(osdbasedata.aktProduct.SetupFiles[0]) then
  begin
    LogDatei.log('Error: setupfile1 not initalized', LLCritical);
    system.ExitCode := 1;
    Result := False;
  end;
  if not Assigned(osdbasedata.aktProduct.SetupFiles[1]) then
  begin
    LogDatei.log('Error: setupfile2 not initalized', LLCritical);
    system.ExitCode := 1;
    Result := False;
  end;
  if not Assigned(osdbasedata.aktProduct.productdata) then
  begin
    LogDatei.log('Error: productdata not initalized', LLCritical);
    system.ExitCode := 1;
    Result := False;
  end;
  if Result = False then
  begin
    LogDatei.Close;
    AppTerminate;
    //Application.Terminate;
  end;
end;

function startOpsiServiceConnection: boolean;
var
  i: integer;
  //passwordToUse: string; is a global var
  strlist: TStringList;
  sessionid: string;
  //localservicedataConnected : boolean = false;
begin
  try
    strlist := TStringList.Create;
    result := true;

    if not localservicedataConnected then
    begin
      if localservicedata <> nil then FreeAndNil(localservicedata);
      if localservicedata = nil then
      localservicedata := TOpsi4Data.Create;
      if (myconfiguration.Service_URL <> '') and
        (myconfiguration.Service_user <> '') then
      begin
        if passwordToUse = '' then
          passwordToUse := myconfiguration.Service_pass;
        {$IFDEF OSDGUI}
        if passwordToUse = '' then
          passwordToUse :=
            PasswordBox('service: ' + myconfiguration.Service_URL +
            ' user: ' + myconfiguration.Service_user, 'Password for opsi web service');
        Screen.Cursor := crHourGlass;
        procmess;
        {$ENDIF OSDGUI}
        localservicedata.initOpsiConf(trim(myconfiguration.Service_URL),
          myconfiguration.Service_user,
          passwordToUse);
        opsiserviceversion := oswebservice.getOpsiServerVersion(
          myconfiguration.Service_URL, myconfiguration.Service_user,
          passwordToUse, sessionid);
        if localservicedata.isConnected then
        begin
          LogDatei.log('Service connection initialized to :' +
            myconfiguration.Service_URL + ' version: ' + opsiserviceversion, LLinfo);
          {$IFDEF OSDGUI}
          FNewDepDlg.LabelConnect.Caption := rsServiceConnected;
          FNewDepDlg.LabelConnect.Font.Color := clGreen;
          resultForm1.StatusBar1.Panels.Items[1].Text :=
             rsServiceConnected +': ' + myconfiguration.Service_URL;
          FNewDepDlg.Repaint;
          procmess;
          LogDatei.log('Service connection initialized to :' +
            myconfiguration.Service_URL + ' version: ' + opsiserviceversion, LLinfo);
          localservicedataConnected := true;
          // fetch produtIds from service
          strlist.Text := localservicedata.getLocalbootProductIds.Text;
          strlist.Sort;
          for i := 0 to strlist.Count - 1 do
            FNewDepDlg.ComboBoxproductIds.Items.Add(
              opsiunquotestr2(strlist.strings[i], '""'));
          {$ENDIF OSDGUI}
        end
        else
        begin
          // service not connected
          result := false;
          LogDatei.log('Service connection not possible: Url, user or password wrong.',
            LLwarning);
          {$IFDEF OSDGUI}
          MessageDlg('opsi-setup-detctor', rsServiceConnectionFailed,
            mtError, [mbOK], '');
          FNewDepDlg.LabelConnect.Caption := rsServiceNotConnected;
          FNewDepDlg.LabelConnect.Font.Color := clRed;
          resultForm1.StatusBar1.Panels.Items[1].Text := rsServiceNotConnected;
          {$ENDIF OSDGUI}
          // reset password from input dialog to empty in order to ask again on reconnect
          if myconfiguration.Service_pass = '' then passwordToUse := '';
        end;
      end
      else
      begin
        // service data missing
        result := false;
        LogDatei.log('Service connection not possible: Url or user missing.', LLwarning);
        {$IFDEF OSDGUI}
        FNewDepDlg.LabelConnect.Caption := rsServiceNotConnected;
        FNewDepDlg.LabelConnect.Font.Color := clRed;
        resultForm1.StatusBar1.Panels.Items[1].Text := rsServiceNotConnected;
        {$ENDIF OSDGUI}
        if localservicedata <> nil then
          FreeAndNil(localservicedata);
      end;
    end;
  finally
    FreeAndNil(strlist);
      {$IFDEF OSDGUI}
    Screen.Cursor := crDefault;
      {$ENDIF OSDGUI}
  end;
end;


procedure main;
var
  ErrorMsg: string;
  i: integer;
  //mylang: string;
  myparamstring: string;
  myparamcount: integer;
  allowedOS: TStringList;
  tmpstr: string;
  anaoutfile: Text;
  myExeDir: string;
  myerror: string;
  myfilename: string;
  optionlist: TStringList;
begin
  osdsettings.startupfinished := True; //avoid calling main on every show event

  // initialize language
  osdsettings.mylocaledir := '';
  {$IFDEF DARWIN}
  osdsettings.mylocaledir := ExtractFileDir(Application.ExeName) +
    PathDelim + '../Resources/locale';
  {$ENDIF DARWIN}
  {$IFDEF LINUX}
  osdsettings.mylocaledir := ExtractFileDir(Application.ExeName) + PathDelim + 'locale';
  if not DirectoryExists(osdsettings.mylocaledir) then
    osdsettings.mylocaledir := '';
  {$ENDIF LINUX}
  //mylang := GetDefaultLang;  // depricated
  osdsettings.mylang := SetDefaultLang('');
  SetDefaultLang(osdsettings.mylang, osdsettings.mylocaledir);

    {$IFDEF WINDOWS}
  // initate console while windows gui
  // https://stackoverflow.com/questions/20134421/can-a-windows-gui-program-written-in-lazarus-create-a-console-and-write-to-it-at
  //AllocConsole;      // in Windows unit
  //IsConsole := True; // in System unit
  //SysInitStdIO;      // in System unit
  // Now you can do Writeln, DebugLn,
  {$ENDIF WINDOWS}

  //check parameters
  myparamcount := ParamCount;
  //myparamcount := Application.ParamCount;
  //writeln('paramcount = '+inttostr(myparamcount));
  myparamstring := '';
  for i := 1 to myparamcount do
    myparamstring := myparamstring + ' ' + Application.Params[i];
  LogDatei.log('Called as: ' + myparamstring, LLEssential);
  myExeDir := ExtractFileDir(ParamStr(0));
  osdsettings.myexitcode := 0;
  myerror := '';
  osdsettings.showgui := True;
  osdsettings.runmode := gmUnknown;
  opsitmp := GetTempDir(False) + 'opsitmp' + PathDelim;
  optionlist := TStringList.Create;
  optionlist.Append('help');
  optionlist.Append('filename::');
  optionlist.Append('nogui');
  optionlist.Append('lang::');
  optionlist.Append('targetOS::');
  optionlist.Append('productId::');
  optionlist.Append('mode::');
  optionlist.Append('template-channel::');

  // quick check parameters
  ErrorMsg := Application.CheckOptions('hfnltpmc', optionlist);
  if ErrorMsg <> '' then
  begin
    LogDatei.log('Exception while handling parameters.', LLcritical);
    ErrorMsg := ErrorMsg + ' with params: ' + myparamstring;
    LogDatei.log(ErrorMsg, LLcritical);
    system.ExitCode := 1;
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
    osdsettings.mylang := Application.GetOptionValue('l', 'lang');
    SetDefaultLang(osdsettings.mylang, osdsettings.mylocaledir);
    LogDatei.log('Found Parameter lang: ' + osdsettings.mylang, LLInfo);
    LogDatei.log('Active lang: ' + osdsettings.mylang, LLInfo);
  end
  else
  begin
    //mylang := GetDefaultLang;  // depricated
    osdsettings.mylang := SetDefaultLang('');
    {$IFDEF WINDOWS}
    if LowerCase(osdsettings.mylang) = '' then
      osdsettings.mylang := LowerCase(
        copy(GetSystemDefaultLocale(LOCALE_SABBREVLANGNAME), 1, 2));
    {$ENDIF WINDOWS}
    SetDefaultLang(osdsettings.mylang, osdsettings.mylocaledir);
    LogDatei.log('Detected default lang: ' + osdsettings.mylang, LLInfo);
    //LogDatei.log('Detected default lang: ' + SetDefaultLang(''), LLInfo);
  end;


  if Application.HasOption('n', 'nogui') then
  begin
    osdsettings.showgui := False;
     {$IFDEF WINDOWS}
    // initate console while windows gui
    // https://stackoverflow.com/questions/20134421/can-a-windows-gui-program-written-in-lazarus-create-a-console-and-write-to-it-at
    //AllocConsole;      // in Windows unit
    //IsConsole := True; // in System unit
    //SysInitStdIO;      // in System unit
    // Now you can do Writeln, DebugLn,
  {$ENDIF WINDOWS}
  end;

  if osdsettings.showgui then
  begin

  end;

  if Application.HasOption('t', 'targetOS') then
  begin
    tmpstr := lowercase(trim(Application.GetOptionValue('t', 'targetOS')));
    allowedOS := TStringList.Create;
    allowedOS.CommaText := 'win,lin,mac';
    if allowedOS.IndexOf(tmpstr) = -1 then
    begin
      myerror := 'Error: Given targetOS: ' + tmpstr +
        ' is not valid. Should be on of win,lin,mac';
      system.ExitCode := 1;
      {$IFNDEF WINDOWS}
      writeln(myerror);
      {$ENDIF WINDOWS}
      LogDatei.log(myerror, LLCritical);
      WriteHelp;
      Application.Terminate;
      Exit;
    end;
    try
      forceTargetOS := TTargetOS(GetEnumValue(TypeInfo(TTargetOS), 'os' + tmpstr))
    except
      myerror := 'Error: Failed to convert: ' + tmpstr + ' to targetOS.';
      {$IFNDEF WINDOWS}
      writeln(myerror);
      {$ENDIF WINDOWS}
      LogDatei.log(myerror, LLCritical);
      system.ExitCode := 1;
      WriteHelp;
      Application.Terminate;
      Exit;
    end;
    LogDatei.log('Will use as targetOS: ' + GetEnumName(
      TypeInfo(TTargetOS), Ord(forceTargetOS)), LLInfo);
    FreeAndNil(allowedOS);
  end;


  if Application.HasOption('m', 'mode') then
  begin
    tmpstr := trim(Application.GetOptionValue('m', 'mode'));
    try
      osdsettings.runmode := TRunMode(GetEnumValue(TypeInfo(TRunMode), tmpstr));
      LogDatei.log('Will use as mode: ' + GetEnumName(TypeInfo(TRunMode),
        Ord(osdsettings.runmode)), LLInfo);
    except
      myerror := 'Error: Given mode: ' + tmpstr +
        ' is not valid. Should be on of singleAnalyzeCreate, createTemplate';
      {$IFNDEF WINDOWS}
      writeln(myerror);
      {$ENDIF WINDOWS}
      LogDatei.log(myerror, LLCritical);
      system.ExitCode := 1;
      WriteHelp;
      Application.Terminate;
      Exit;
    end;
  end;


  initaktproduct;
  {$IFDEF OSDGUI}
  resultform1.updateGUI;
  {$ENDIF OSDGUI}

  if Application.HasOption('p', 'productId') then
  begin
    forceProductId := trim(Application.GetOptionValue('p', 'productId'));
    LogDatei.log('Will use as productId: ' + forceProductId, LLInfo);
    forceProductId := cleanOpsiId(forceProductId);
    LogDatei.log('Will use as productId: ' + forceProductId, LLInfo);
  end;

  if Application.HasOption('c', 'template-channel') then
  begin
    tmpstr := trim(Application.GetOptionValue('c', 'template-channel'));
    try
      aktProduct.productdata.channelDir :=
        templChannelStrings[TTemplateChannels(
        GetEnumValue(TypeInfo(TTemplateChannels), tmpstr))];
      LogDatei.log('Will use as channelDir: ' +
        aktProduct.productdata.channelDir, LLInfo);
    except
      myerror := 'Error: Given mode: ' + tmpstr +
        ' is not valid. Should be on of: training, default, structured, custom';
      {$IFNDEF WINDOWS}
      writeln(myerror);
      {$ENDIF WINDOWS}
      LogDatei.log(myerror, LLCritical);
      system.ExitCode := 1;
      WriteHelp;
      Application.Terminate;
      Exit;
    end;
  end;

  if Application.HasOption('f', 'filename') then
  begin
    myfilename := trim(Application.GetOptionValue('f', 'filename'));
    if not FileExists(myfilename) then
    begin
      myerror := 'Error: Given filename: ' + myfilename + ' does not exist.';
      LogDatei.log(myerror, LLCritical);
      system.ExitCode := 1;
      WriteHelp;
      Application.Terminate;
      Exit;
    end;
    LogDatei.log('Got command line parameter filename with existing: ' +
      myfilename, LLInfo);
    if osdsettings.runmode = gmUnknown then
      osdsettings.runmode := analyzeOnly;
    LogDatei.log('Will use as mode: ' + GetEnumName(TypeInfo(TRunMode),
      Ord(osdsettings.runmode)), LLInfo);
    LogDatei.log('Will use as targetOS: ' + GetEnumName(
      TypeInfo(TTargetOS), Ord(forceTargetOS)), LLInfo);
    LogDatei.log('Will use as productId: ' + forceProductId, LLInfo);
    if osdsettings.showgui then
    begin
      {$IFDEF OSDGUI}
      LogDatei.log('Start GUI mode: ', LLInfo);
      initGUI;
      with resultform1 do
      begin
        Show;
        setRunMode;
        resultform1.MemoAnalyze.Clear;
        PageControl1.ActivePage := TabSheetAnalyze;
        LogDatei.log('Wait for GUI  ', LLInfo);
        repeat
          Application.ProcessMessages;
          sleep(100);
        until (resultform1.Showing = True) and osdsettings.startupfinished;
      end;
      LogDatei.log('Start Analyze in GUI mode: ', LLInfo);
      Analyze(myfilename, aktProduct.SetupFiles[0], True);
      {$ENDIF OSDGUI}
    end
    else
    begin
      LogDatei.log('Start NOGUI mode: ', LLnotice);
      case osdsettings.runmode of

        analyzeOnly:
        begin
          LogDatei.log('Start Analyze in NOGUI mode: ', LLInfo);
          case forceTargetOS of
            osWin: Analyze(myfilename, aktProduct.SetupFiles[0], False);
            osLin: AnalyzeLin(myfilename, aktProduct.SetupFiles[0], False);
            osMac: AnalyzeMac(myfilename, aktProduct.SetupFiles[0], False);
          end;
        end;
        singleAnalyzeCreate, analyzeCreateWithUser:
        begin
          LogDatei.log('Start Analyze + Create in NOGUI mode: ', LLnotice);
          // was done before and should not be overwritten here
          //initaktproduct;
          aktProduct.SetupFiles[0].copyCompleteDir := False;
          makeProperties;
          {$IFDEF OSDGUI}
          resultform1.updateGUI;
          {$ENDIF OSDGUI}
          aktProduct.SetupFiles[0].active := True;
          case forceTargetOS of
            osWin:
            begin
              aktProduct.productdata.targetOSset := [osWin];
              aktProduct.SetupFiles[0].targetOS := osWin;
              Analyze(myfilename, aktProduct.SetupFiles[0], False);
            end;
            osLin:
            begin
              aktProduct.productdata.targetOSset := [osLin];
              aktProduct.SetupFiles[0].targetOS := osLin;
              AnalyzeLin(myfilename, aktProduct.SetupFiles[0], False);
            end;
            osMac:
            begin
              aktProduct.productdata.targetOSset := [osMac];
              aktProduct.SetupFiles[0].targetOS := osMac;
              AnalyzeMac(myfilename, aktProduct.SetupFiles[0], False);
            end;
          end;
        end;
        createTemplate:
        begin
          LogDatei.log('Start createTemplate in NOGUI mode: ', LLnotice);
          case forceTargetOS of
            osWin:
            begin
              aktProduct.productdata.targetOSset := [osWin];
              aktProduct.SetupFiles[0].targetOS := osWin;
            end;
            osLin:
            begin
              aktProduct.productdata.targetOSset := [osLin];
              aktProduct.SetupFiles[0].targetOS := osLin;
            end;
            osMac:
            begin
              aktProduct.productdata.targetOSset := [osMac];
              aktProduct.SetupFiles[0].targetOS := osMac;
            end;
          end;
        end;
      end;
      // write osd-analyze-result.txt
      AssignFile(anaoutfile, 'c:\opsi.org\applog\osd-analyze-result.txt');
      Rewrite(anaoutfile);
      writeln(anaoutfile, 'installertype=' + installerToInstallerstr(
        aktProduct.SetupFiles[0].installerId));
      writeln(anaoutfile, 'SoftwareVersion=' +
        aktProduct.SetupFiles[0].SoftwareVersion);
      writeln(anaoutfile, 'installCommandLine=' +
        aktProduct.SetupFiles[0].installCommandLine);
      writeln(anaoutfile, '');
      writeln(anaoutfile, '');
      writeln(anaoutfile, '');
      writeln(anaoutfile, '');
      writeln(anaoutfile, '');
      writeln(anaoutfile, '');
      CloseFile(anaoutfile);
      if forceProductId <> '' then
        aktProduct.productdata.productId := forceProductId;
      if osdsettings.runmode <> analyzeOnly then
      begin
        LogDatei.log('Start createProductStructure in NOGUI mode: ', LLnotice);
        createProductStructure;
      end;

      if (osdsettings.runmode <> analyzeOnly) and
        (osdsettings.CreateModeIndex > 0)
      // is the same as: (not RadioButtonCreateOnly.Checked)
      then
      begin
        LogDatei.log('Start callServiceOrPackageBuilder in NOGUI mode: ', LLnotice);
        LogDatei.log('Start callServiceOrPackageBuilder with build + install: ',
          LLnotice);
        callServiceOrPackageBuilder;
      end;
    end;
  end
  else
  begin
    if osdsettings.showgui then
    begin
      LogDatei.log('Start GUI mode: ', LLInfo);
      {$IFDEF OSDGUI}
      initGUI;
      {$ENDIF OSDGUI}
    end
    else
    begin
      myerror := 'Error: No filename given but nogui';
      LogDatei.log(myerror, LLCritical);
      system.ExitCode := 1;
      WriteHelp;
      Application.Terminate;
      Exit;
    end;
  end;

  // stop program loop
  if not osdsettings.showgui then
  begin
    LogDatei.log('Finished and terminate regular in NOGUI mode. ', LLnotice);
    freebasedata;
    Application.Terminate;
  end;
  LogDatei.log('Finished main ', LLInfo);
end;



end.
