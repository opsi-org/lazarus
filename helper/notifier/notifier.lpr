program notifier;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  LazFileUtils,
  oslog,
  IdTCPClient,
  Variants,
  fileinfo,
  winpeimagereader, notifier_base;


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

(*
  Tmythread = class(TThread)
  public
    procedure Execute; override;
    constructor Create(CreateSuspended : boolean);
  end;
*)

var
 Application: TMyApplication;

 (*


var
  myport: integer;
  myevent: string;
  myconfigpath, myconfigfile: string;
  myexepath: string;
  myVersion: string;
  myTCPClient: TIdTCPClient;
  stopped: boolean;
  mythread: Tmythread;
  Application: TMyApplication;


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
*)


  { TMyApplication }

  procedure TMyApplication.DoRun;
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
    LogDatei.FileName := ExtractFileNameOnly(ExeName);
    LogDatei.StandardLogFileext:='.log';
    LogDatei.StandardLogFilename := ExtractFileNameOnly(ExeName);
    LogDatei.StandardPartLogFilename := ExtractFileNameOnly(ExeName) + '-part';
    LogDatei.CreateTheLogfile(ExtractFileNameOnly(ExeName)+'.log', True);
    LogDatei.log('Log for: ' + exename +
      ' version: ' + myVersion + ' opend at : ' + DateTimeToStr(now), LLinfo);

    myexepath := ExtractFilePath(ExeName);
    myport := 44003;

    // quick check parameters
    optionlist := TStringList.Create;
    optionlist.Add('help');
    optionlist.Add('port:');
    optionlist.Add('showconfigfile:');
    optionlist.Add('inputevent:');
    ErrorMsg := CheckOptions('hp:s:i:', optionlist);
    if ErrorMsg <> '' then
    begin
      logdatei.log(ErrorMsg, LLcritical);
      logdatei.Close;
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      logdatei.log('Found Parameter help: show and exit', LLInfo);
      WriteHelp;
      logdatei.Close;
      Terminate;
      Exit;
    end;

    { add your program here }
    if HasOption('p', 'port') then
    begin
      logdatei.log('Found Parameter port', LLDebug);
      myport := StrToInt(GetOptionValue('p', 'port'));
      logdatei.log('Found Parameter port: ' + IntToStr(myport), LLInfo);
    end;

    if HasOption('s', 'showconfigfile') then
    begin
      logdatei.log('Found Parameter showconfigfile', LLDebug);
      myconfigpath := GetOptionValue('s', 'showconfigfile');
      logdatei.log('Found Parameter port: ' + IntToStr(myport), LLInfo);
      myconfigfile := myexepath + myconfigpath;
      if not FileExists(myconfigfile) then
      begin
        logdatei.log('Error: Given config file not found: ' + myconfigfile, LLCritical);
        logdatei.Close;
        writeln('Error: Given config file not found: ' + myconfigfile);
        Terminate;
        Exit;
      end;
    end;

    if HasOption('i', 'inputevent') then
    begin
      logdatei.log('Found Parameter inputevent', LLDebug);
      myevent := GetOptionValue('i', 'inputevent');
      logdatei.log('Found Parameter inputevent: ' + myevent, LLInfo);
    end;

    main;
    (*
    stopped := False;
    mythread := Tmythread.Create(False);

    while not stopped  do
    begin
      Sleep(100);
    end;
    *)
    // stop program loop
    logdatei.log('Program regulary finished', LLInfo);
    logdatei.Close;
    writeln('Program regulary finished');
    //while not mythread.CheckTerminated do
    //begin
    //  Sleep(100);
    //end;
    Terminate;
  end;




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


  {$R *.res}

begin
  Application := TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.


