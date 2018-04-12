unit notifierdatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms,
  LazFileUtils,
  oslog,
  //IdTCPClient,
  Variants,
  fileinfo,
  winpeimagereader,
  Dialogs, ExtCtrls,
  notifier_base,
  notifierform;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
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
//stopped: boolean;

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
  msg := msg + ' -i <event> -> id of the event; required';
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
begin
  preloglist := TStringList.Create;
  preloglist.Add('PreLog for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now));
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    myVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
  preloglist.Add('Application version: ' + myVersion);


  myexepath := ExtractFilePath(Application.ExeName);
  //myport := 44003;
  myport := 0;
  //stopped := False;


  // quick check parameters
  optionlist := TStringList.Create;
  optionlist.Add('help');
  optionlist.Add('port:');
  optionlist.Add('skinconfigfile:');
  optionlist.Add('idevent:');
  ErrorMsg := Application.CheckOptions('hp:s:i:', optionlist);
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
    myport := StrToInt(Application.GetOptionValue('p', 'port'));
    preloglist.Add('Found Parameter port: ' + IntToStr(myport));
  end;

  if Application.HasOption('s', 'skinconfigfile') then
  begin
    preloglist.Add('Found Parameter skinconfigfile');
    myconfigpath := Application.GetOptionValue('s', 'skinconfigfile');
    preloglist.Add('Found Parameter skinconfigfile: ' + myconfigpath);
    myconfigfile := myexepath + myconfigpath;
    if not FileExists(myconfigfile) then
    begin
      preloglist.Add('Error: Given skinconfig file not found: ' + myconfigfile);
      logAndTerminate := True;
      //logdatei.Close;
      //Application.Terminate;
      //Exit;
    end;
  end
  else
  begin
    preloglist.Add('Error: No skin config file given. I s required ');
    logAndTerminate := True;
    //logdatei.Close;
    //Application.Terminate;
    //Exit;
  end;

  if Application.HasOption('i', 'idevent') then
  begin
    preloglist.Add('Found Parameter idevent');
    mynotifierkind := Application.GetOptionValue('i', 'idevent');
    // opsiclientd bug: pupup comes with %id%
    if mynotifierkind = '%id%' then mynotifierkind := 'popup';
    preloglist.Add('Found Parameter idevent: ' + mynotifierkind);
  end;

  // Initialize logging
  LogDatei := TLogInfo.Create;
  lfilename := ExtractFileNameOnly(Application.ExeName);
  // use different filenames for different instances
  if mynotifierkind <> '' then
    lfilename := lfilename + '_' + mynotifierkind;
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;

  //LogDatei.StandardPartLogFilename := lfilename+ '-part';
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
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

  LogDatei.LogLevel := 8;

  //Application.OnQueryEndSession := @queryend;

  // call main procedure
  main;

end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
  mythread.Terminate;
  //if not inHideNForm then hideNForm
  //else sleep(5000);
  // stop program loop
  logdatei.log('Program regulary finished (killed)', LLInfo);
  logdatei.Close;

  Application.Terminate;
end;

(*
procedure TDataModule1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  //hideNForm;
  self.Destroy;
  //Application.Terminate;
end;
*)

end.
