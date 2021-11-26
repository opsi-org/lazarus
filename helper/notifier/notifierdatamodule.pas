unit notifierdatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms,
  LazFileUtils,
  lazutils,
  oslog,
  //IdTCPClient,
  Variants,
  fileinfo,
  winpeimagereader,
  Dialogs, ExtCtrls,
  notifier_base,
  notifierguicontrol,
  {$IFDEF WINDOWS}
  jwawinbase,
  jwawinuser,
  jwawintype,
  jwawtsapi32,
  Windows,
  {$ENDIF WINDOWS}
  notifierform;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    TimerClose: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerCloseTimer(Sender: TObject);
    //procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure WriteHelp;
    procedure ProcessMess;
    procedure createNform;
    procedure hideMainForm;
    //procedure queryend(var Cancel: boolean);
  end;


var
  DataModule1: TDataModule1;
  myport: integer;
  mynotifierkind: string;
  myconfigpath, myconfigfile: string;
  myexepath: string;
  myVersion: string;
  showtest: boolean = False;
//stopped: boolean;


{$IFDEF WINDOWS}
function getMyWinDesktopName: string;
function getMyWinSessionName: string;
{$ENDIF WINDOWS}

implementation

//uses
//  notifierguicontrol;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.ProcessMess;
begin
  Application.ProcessMessages;
end;

procedure TDataModule1.createNform;
begin
  Application.CreateForm(TNForm, NForm);
end;

procedure TDataModule1.WriteHelp;
var
  filename: string;
  msg: string;
begin
  { add your help code here }
  filename := ExtractFileName(Application.ExeName);
  msg := 'This is ' + filename + ' version: ' + myVersion +
    ' (c) uib gmbh, AGPLv3' + LineEnding;
  msg := msg + 'Usage: ' + filename + ' Options' + LineEnding;
  msg := msg + 'Options:' + LineEnding;
  msg := msg + ' --help -> write this help and exit' + LineEnding;
  msg := msg + ' -h -> write this help and exit' + LineEnding;
  msg := msg + ' --port=<port> -> tcp port for communication with opsiclientd; required'
    + LineEnding;
  msg := msg + ' -p <port> -> tcp port for communication with opsiclientd; required' +
    LineEnding;
  msg := msg + ' --skinconfigfile=<path> -> relative path to config file; required' +
    LineEnding;
  msg := msg + ' -s <path> -> relative path to config file; required' + LineEnding;
  msg := msg + ' --idevent=<event> -> running event; required' + LineEnding;
  msg := msg + ' -i <event> -> id of the event; required' + LineEnding;
  msg := msg + ' --test or -t (use with -s ; shows notifier for some seconds' +
    LineEnding;
  msg := msg + ' --loglevel=<loglevel> -> loglevel (1 - 9, default=6)' + LineEnding;
  msg := msg + ' -l <loglevel> -> loglevel (1 - 9, default=6)';
  ShowMessage(msg);
end;

procedure TDataModule1.hideMainForm;
begin
  //hideNForm;
end;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
  ErrorMsg: string;
  optionlist: TStringList;
  FileVerInfo: TFileVersionInfo;
  preloglist: TStringList;
  i: integer;
  lfilename: string;
  logAndTerminate: boolean = False;
  mynotifierConfPath: string;
  fullparam: string;
  myDeskName: string = '';
  myloglevel: integer;
  mySession: string = '';
  tmpstr: string;
begin
  //writeln('start');
  preloglist := TStringList.Create;
  preloglist.Add('PreLog for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now));
  //writeln('PreLog for: ' + Application.exename + ' opend at : ' +
  //  DateTimeToStr(now));
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
  preloglist.Add('Application version: ' + myVersion);
  for i := 0 to ParamCount do
    fullparam := fullparam + ParamStr(i) + ' ';
  preloglist.Add('called: ' + fullparam);

  myexepath := ExtractFilePath(Application.ExeName);
  {$IFDEF WINDOWS}
  mynotifierConfPath := myexepath;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  mynotifierConfPath := '/usr/share/opsi-client-agent/';
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  mynotifierConfPath := '/usr/local/share/opsi-client-agent/';
  {$ENDIF DARWIN}
  //myport := 44003;
  myport := 0;
  myloglevel := 6;
  //myloglevel := 8;
  //stopped := False;


  // quick check parameters
  optionlist := TStringList.Create;
  optionlist.Add('help');
  optionlist.Add('port:');
  optionlist.Add('skinconfigfile:');
  optionlist.Add('idevent:');
  optionlist.Add('test');
  optionlist.Add('loglevel:');
  ErrorMsg := Application.CheckOptions('thp:s:i:l:', optionlist);
  if ErrorMsg <> '' then
  begin
    preloglist.Add(ErrorMsg);
    //logdatei.Close;
    logAndTerminate := True;
    Application.ShowException(Exception.Create(ErrorMsg));
    //Application.Terminate;
    //Exit;
  end;

  // parse parameters
  if Application.HasOption('h', 'help') then
  begin
    preloglist.Add('Found Parameter help: show and exit');
    WriteHelp;
    logAndTerminate := True;
    //Application.Terminate;
    //Exit;
  end;

  if Application.HasOption('p', 'port') then
  begin
    preloglist.Add('Found Parameter port');
    tmpstr := Application.GetOptionValue('p', 'port');
    if TryStrToInt(tmpstr, myport) then
      preloglist.Add('Found Parameter port: ' + IntToStr(myport))
    else
    begin
      preloglist.Add('Error: Given port is not an integer: ' + tmpstr);
      logAndTerminate := True;
    end;
  end;

  if Application.HasOption('s', 'skinconfigfile') then
  begin
    preloglist.Add('Found Parameter skinconfigfile');
    myconfigpath := Application.GetOptionValue('s', 'skinconfigfile');
    preloglist.Add('Found Parameter skinconfigfile: ' + myconfigpath);
    preloglist.Add('Test: skinconfig file: ' + myconfigpath);
    if FileExists(myexepath + myconfigpath) then // for debug
      myconfigfile := myexepath + myconfigpath
    else
      myconfigfile := mynotifierConfPath + myconfigpath;
    if not FileExists(myconfigfile) then
    begin
      preloglist.Add('Error: Given skinconfig file not found: ' + myconfigfile);
      logAndTerminate := True;
      //logdatei.Close;
      //Application.Terminate;
      //Exit;
    end
    else
      preloglist.Add('Will use skinconfigfile: ' + myconfigfile);
  end
  else
  begin
    preloglist.Add('Error: No skin config file given. Is required ');
    logAndTerminate := True;
    //logdatei.Close;
    //Application.Terminate;
    //Exit;
  end;

  if Application.HasOption('i', 'idevent') then
  begin
    preloglist.Add('Found Parameter idevent');
    mynotifierkind := Application.GetOptionValue('i', 'idevent');
    // opsiclientd bug: popup comes with %id%
    // shoud be fixed in opsi 4.2 opsiclientd
    if mynotifierkind = '%id%' then
      mynotifierkind := 'popup';
    preloglist.Add('Found Parameter idevent: ' + mynotifierkind);
  end;

  if Application.HasOption('l', 'loglevel') then
  begin
    preloglist.Add('Found Parameter loglevel');
    tmpstr := Application.GetOptionValue('l', 'loglevel');
    if TryStrToInt(tmpstr, myloglevel) then
      preloglist.Add('Found Parameter loglevel: ' + IntToStr(myloglevel))
    else
    begin
      preloglist.Add('Error: Given loglevel is not an integer: ' + tmpstr);
      logAndTerminate := True;
    end;
  end;

  if Application.HasOption('t', 'test') then
  begin
    preloglist.Add('Found Parameter test');
    showtest := True;
  end;

  {$IFDEF WINDOWS}
  myDeskName := getMyWinDesktopName;
  preloglist.Add('Found Windows Desktop: ' + myDeskName);
  mySession := getMyWinSessionName;
  preloglist.Add('Found Windows Session: ' + mySession);
  {$ENDIF WINDOWS}

  // Initialize logging
  LogDatei := TLogInfo.Create;
  lfilename := ExtractFileNameOnly(Application.ExeName);
  // use different filenames for different instances

  if myconfigpath <> '' then
    lfilename := lfilename + '_' + ExtractFileNameWithoutExt(
      ExtractFileName(myconfigpath))
  else
  if mynotifierkind <> '' then
    lfilename := lfilename + '_' + mynotifierkind;

  if myDeskName <> '' then
    lfilename := lfilename + '_' + myDeskName;

  if mySession <> '' then
    lfilename := lfilename + '_S' + mySession;


  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile := False;
  LogDatei.WriteHistFile := False;
  LogDatei.LogLevel := myloglevel;

  //LogDatei.StandardPartLogFilename := lfilename+ '-part';
  {$IFDEF UNIX}
  WriteLn('Will using log: ' + LogDatei.StandardLogPath + LogDatei.StandardLogFilename);
  {$ENDIF UNIX}
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
  LogDatei.LogLevel := myloglevel;

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
  LogDatei.log('Log for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now), LLinfo);


  //Application.OnQueryEndSession := @queryend;

  // call main procedure
  main;

end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(mythread) then
    mythread.Terminate;
  //if not inHideNForm then hideNForm
  //else sleep(5000);
  // stop program loop
  logdatei.log('Program regulary finished (killed)', LLnotice);
  logdatei.Close;
  Application.Terminate;
  halt;
end;

procedure TDataModule1.TimerCloseTimer(Sender: TObject);
begin
  mythread.Terminate;
  logdatei.log('We are in popup, button close clicked and not terminated after ' +
    IntToStr(TimerClose.Interval div 1000) + ' seconds: terminate', LLInfo);
  notifierguicontrol.hideNForm;
  DataModule1.DataModuleDestroy(nil);
  Halt;
end;

(*
procedure TDataModule1.Timer1Timer(Sender: TObject);
begin
  TimerClose.Enabled := False;
  //hideNForm;
  self.Destroy;
  //Application.Terminate;
end;
*)

{$IFDEF WINDOWS}
function getMyWinDesktopName: string;
var
  mydesk: HDESK;
  szb: array [0..80] of char;
  needed: DWord;
begin
  Result := '';
  mydesk := GetThreadDesktop(GetCurrentThreadId);
  if mydesk = 0 then
  begin
    Result := '';
  end
  else
  begin
    if GetUserObjectInformation(mydesk, UOI_NAME, @szb, SizeOf(szb), needed) then
    begin
      Result := string(szb);
    end
    else
    begin
      Result := '';
    end;
  end;
end;

// https://www.delphipraxis.net/201869-terminalserver-session-id.html
function getMyWinSessionName: string;
var
  pid: dword;
  sessionid: dword;
  nByteCount: DWORD;
  acNameBuff: Pointer;
  tmpName: string;
  sessionnumber: ULong;
  psessionnumber: ^ULong;

begin
  Result := '';
  ;
  pid := GetCurrentProcessId;
  if ProcessIdToSessionId(pid, sessionid) then
  begin
    if sessionid > 0 then
    begin
      if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE, sessionid,
        WTSSessionId, psessionnumber, nByteCount) then
      begin
        sessionnumber := psessionnumber^;
        Result := IntToStr(sessionnumber);
        (*
        tmpName := StrPas(PChar(acNameBuff));
        // Da bei Vista kein Name vorhanden ist, auf Leerstring prüfen!
        if tmpName <> '' then
          result := tmpName;
        *)
        WTSFreeMemory(psessionnumber);
      end;
    end;
  end;
end;

{$ENDIF WINDOWS}

end.
