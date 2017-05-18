unit notifierdatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms,
  LazFileUtils,
  oslog,
  IdTCPClient,
  Variants,
  fileinfo,
  winpeimagereader,
  Dialogs, ExtCtrls,
  notifier_base,
  notifierform;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    Timer1: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure WriteHelp;
    procedure ProcessMess;
    procedure createNform;
    procedure hideMainForm;
    procedure queryend(var Cancel: Boolean);
  end;


var
  DataModule1: TDataModule1;
  myport: integer;
  myevent: string;
  myconfigpath, myconfigfile: string;
  myexepath: string;
  myVersion: string;
  //stopped: boolean;

implementation

uses
  notifierguicontrol;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.queryend(var Cancel: Boolean);
begin
  //hideNForm;
  //Nform.Close;
  self.Destroy;
  Cancel := false;
end;

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
    (*
    writeln('This is ', filename, ' version: ' + myVersion + ' (c) uib gmbh, AGPLv3');
    writeln('Usage: ', filename, ' Options');
    writeln('Options:');
    writeln(' --help -> write this help and exit');
    writeln(' -h -> write this help and exit');
    writeln(' --port=<port> -> tcp port for communication with opsiclientd; required');
    writeln(' -p <port> -> tcp port for communication with opsiclientd; required');
    writeln(' --showconfigfile=<path> -> relative path to config file; required');
    writeln(' -s <path> -> relative path to config file; required');
    writeln(' --inputevent=<event> -> running event; required');
    writeln(' -i <event> -> running event; required');
    *)
  msg := 'This is ' + filename + ' version: ' + myVersion +
    ' (c) uib gmbh, AGPLv3' + LineEnding;
  msg := msg + 'Usage: ' + filename + ' Options' + LineEnding;
  msg := msg + 'Options:' + LineEnding;
  msg := msg + ' --help -> write this help and exit' + LineEnding;
  msg := msg + ' -h -> write this help and exit' + LineEnding;
  msg := msg + ' --port=<port> -> tcp port for communication with opsiclientd; required'
    +
    LineEnding;
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
  hideNForm;
end;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
  ErrorMsg: string;
  optionlist: TStringList;
  FileVerInfo: TFileVersionInfo;
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
  LogDatei.FileName := ExtractFileNameOnly(Application.ExeName);
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := ExtractFileNameOnly(Application.ExeName);
  LogDatei.StandardPartLogFilename :=
    ExtractFileNameOnly(Application.ExeName) + '-part';
  LogDatei.CreateTheLogfile(ExtractFileNameOnly(Application.ExeName) + '.log', True);
  LogDatei.log('Log for: ' + Application.exename + ' version: ' +
    myVersion + ' opend at : ' + DateTimeToStr(now), LLinfo);
  LogDatei.LogLevel:=8;

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
    logdatei.log(ErrorMsg, LLcritical);
    logdatei.Close;
    Application.ShowException(Exception.Create(ErrorMsg));
    Application.Terminate;
    Exit;
  end;

  // parse parameters
  if Application.HasOption('h', 'help') then
  begin
    logdatei.log('Found Parameter help: show and exit', LLInfo);
    WriteHelp;
    logdatei.Close;
    Application.Terminate;
    Exit;
  end;

  if Application.HasOption('p', 'port') then
  begin
    logdatei.log('Found Parameter port', LLDebug);
    myport := StrToInt(Application.GetOptionValue('p', 'port'));
    logdatei.log('Found Parameter port: ' + IntToStr(myport), LLInfo);
  end;

  if Application.HasOption('s', 'skinconfigfile') then
  begin
    logdatei.log('Found Parameter skinconfigfile', LLDebug);
    myconfigpath := Application.GetOptionValue('s', 'skinconfigfile');
    logdatei.log('Found Parameter skinconfigfile: ' + myconfigpath, LLInfo);
    myconfigfile := myexepath + myconfigpath;
    if not FileExists(myconfigfile) then
    begin
      logdatei.log('Error: Given skinconfig file not found: ' + myconfigfile, LLCritical);
      logdatei.Close;
      Application.Terminate;
      Exit;
    end;
  end
  else
  begin
      logdatei.log('Error: No skin config file given. I s required ', LLCritical);
      logdatei.Close;
      Application.Terminate;
      Exit;
  end;

  if Application.HasOption('i', 'idevent') then
  begin
    logdatei.log('Found Parameter idevent', LLDebug);
    myevent := Application.GetOptionValue('i', 'idevent');
    logdatei.log('Found Parameter idevent: ' + myevent, LLInfo);
  end;

  Application.OnQueryEndSession:=@queryend;

  // call main procedure
  main;

  (*
  // stop program loop
  logdatei.log('Program regulary finished', LLInfo);
  logdatei.Close;
  //writeln('Program regulary finished');
  //while not mythread.CheckTerminated do
  //begin
  //  Sleep(100);
  //end;
  Application.Terminate;
  *)
end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
  mythread.Terminate;
  if not inHideNForm then hideNForm
  else sleep(5000);
  // stop program loop
  logdatei.log('Program regulary finished (killed)', LLInfo);
  logdatei.Close;

  Application.Terminate;
end;

procedure TDataModule1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=false;
  //hideNForm;
  self.Destroy;
  //Application.Terminate;
end;

end.
