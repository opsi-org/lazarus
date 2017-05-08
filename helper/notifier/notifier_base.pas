unit notifier_base;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  LazFileUtils,
  oslog,
  IdTCPClient,
  Variants,
  fileinfo,
  winpeimagereader;

type

Tmythread = class(TThread)
public
  procedure Execute; override;
  constructor Create(CreateSuspended : boolean);
end;


(*
type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;
*)
procedure Main;

var
  myport: integer;
  myevent: string;
  myconfigpath, myconfigfile: string;
  myexepath: string;
  myVersion: string;



implementation


var
  myTCPClient: TIdTCPClient;
  stopped: boolean;
  mythread: Tmythread;
 // myApplication: TMyApplication;
  //Application: TMyApplication;

  (*
  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  var
    filename: string;
  begin
    { add your help code here }
    filename := ExtractFileName(ExeName);
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
  end;
*)



  constructor TMyThread.Create(CreateSuspended : boolean);
  begin
    FreeOnTerminate := True;
    inherited Create(CreateSuspended);
  end;

  procedure Tmythread.Execute;
  var
    receiveline: string;
    i: integer;

  begin
    if not Terminated then
    begin
      //myFreeze := TIdAntiFreeze.Create;
      //myFreeze.Active:=true;
      myTCPClient := TIdTCPClient.Create;
      myTCPClient.Port := myport;
      myTCPClient.Host := '127.0.0.1';
      myTCPClient.ReadTimeout := 1000;

      //Application.ProcessMessages;
      repeat
        try
          myTCPClient.Connect;
        except
        end;
      until myTCPClient.Connected;
      i := 1;
      while (not Terminated) and (i < 160) do
      begin
        ;
        //Application.ProcessMessages;
        receiveline := myTCPClient.Socket.ReadLn();
        logdatei.log(receiveline, LLInfo);
        logdatei.log(IntToStr(i) +
          ' --------------------------------------------------', LLInfo);
        Inc(i);
      end;
    end
    else stopped := true;
  end;



  procedure Main;
  var
    ErrorMsg: string;
    optionlist: TStringList;
    FileVerInfo: TFileVersionInfo;

  begin
    (*

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
    LogDatei.FileName := ExtractFileNameOnly(myApplication.ExeName);
    LogDatei.StandardLogFileext:='.log';
    LogDatei.StandardLogFilename := ExtractFileNameOnly(myApplication.ExeName);
    LogDatei.StandardPartLogFilename := ExtractFileNameOnly(myApplication.ExeName) + '-part';
    LogDatei.CreateTheLogfile(ExtractFileNameOnly(myApplication.ExeName)+'.log', True);
    LogDatei.log('Log for: ' + myApplication.exename +
      ' version: ' + myVersion + ' opend at : ' + DateTimeToStr(now), LLinfo);

    myexepath := ExtractFilePath(myApplication.ExeName);
    myport := 44003;
    stopped := false;

    // quick check parameters
    optionlist := TStringList.Create;
    optionlist.Add('help');
    optionlist.Add('port:');
    optionlist.Add('showconfigfile:');
    optionlist.Add('inputevent:');
    ErrorMsg := myApplication.CheckOptions('hp:s:i:', optionlist);
    if ErrorMsg <> '' then
    begin
      logdatei.log(ErrorMsg, LLcritical);
      logdatei.Close;
      //ShowException(Exception.Create(ErrorMsg));
      myApplication.Terminate;
      Exit;
    end;

    // parse parameters
    if myApplication.HasOption('h', 'help') then
    begin
      logdatei.log('Found Parameter help: show and exit', LLInfo);
      myApplication.WriteHelp;
      logdatei.Close;
      myApplication.Terminate;
      Exit;
    end;

    { add your program here }
    if myApplication.HasOption('p', 'port') then
    begin
      logdatei.log('Found Parameter port', LLDebug);
      myport := StrToInt(myApplication.GetOptionValue('p', 'port'));
      logdatei.log('Found Parameter port: ' + IntToStr(myport), LLInfo);
    end;

    if myApplication.HasOption('s', 'showconfigfile') then
    begin
      logdatei.log('Found Parameter showconfigfile', LLDebug);
      myconfigpath := myApplication.GetOptionValue('s', 'showconfigfile');
      logdatei.log('Found Parameter port: ' + IntToStr(myport), LLInfo);
      myconfigfile := myexepath + myconfigpath;
      if not FileExists(myconfigfile) then
      begin
        logdatei.log('Error: Given config file not found: ' + myconfigfile, LLCritical);
        logdatei.Close;
        writeln('Error: Given config file not found: ' + myconfigfile);
        myApplication.Terminate;
        Exit;
      end;
    end;

    if myApplication.HasOption('i', 'inputevent') then
    begin
      logdatei.log('Found Parameter inputevent', LLDebug);
      myevent := myApplication.GetOptionValue('i', 'inputevent');
      logdatei.log('Found Parameter inputevent: ' + myevent, LLInfo);
    end;
     *)


    stopped := False;
    mythread := Tmythread.Create(False);

    while not stopped  do
    begin
      Sleep(100);
    end;
     (*
    // stop program loop
    logdatei.log('Program regulary finished', LLInfo);
    logdatei.Close;
    writeln('Program regulary finished');
    //while not mythread.CheckTerminated do
    //begin
    //  Sleep(100);
    //end;
    myApplication.Terminate;
    *)
  end;


end.

