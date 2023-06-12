unit osstartproc_cp;

//{$mode delphi}
{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  jwatlhelp32,
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  baseunix,
  fptimer, // Timer for the wait for ProcessWaitingFor end
  {$ENDIF UNIX}
  {$IFDEF OPSISCRIPT}
  {$IFDEF GUI}
  osshowsysinfo,
  osGUIControl,
  //osbatchgui,
  //osinteractivegui,
  {$ELSE GUI}
  //osmain,
  {$ENDIF GUI}
  {$ENDIF OPSISCRIPT}
  Classes,
  SysUtils,
  process,
  //utf8process,
  osparserhelper,
  oslog,
  osprocesses,
  osprocessux,
  ostxstringlist,
  crt,
  strutils,
  lconvencoding,
  lcltype,
  LazUTF8,
  pipes;

const
  BytesarrayLength = 5000;
  PATHSEPARATOR = PathDelim;
  //READ_BYTES = 2048;
  READ_BYTES = 4096;

function StartProcess_cp(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutput: boolean; WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint; catchout: boolean;
  var output: TXStringList): boolean;

implementation

{$IFDEF OPSISCRIPT}
  {$IFDEF GUI}
uses
  osmain;

  {$ENDIF GUI}
  {$ENDIF OPSISCRIPT}

{$IFDEF UNIX}
// Here we have the global structures to catch the process end and exitcode
// to avoid zombies at linux
// we just use timers to wait for every process end
type
  // this is the wait object for one process
  TWaiterForProcessEnd = class(TCollectionItem)
  private
    FUsed: boolean; // is this object in use ?
  published
    Timer: TFPTimer;
    ProcessWaitingFor: TProcess;
    procedure CheckForProcessEnd(Sender: TObject);
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // this is the collection of wait objects
  // http://wiki.lazarus.freepascal.org/TCollection
  // https://stackoverflow.com/questions/6980401/collection-editor-does-not-open-for-a-tcollection-property-in-a-tpersistent-prop
  TWaiterForProcessEndCollection = class(TOwnedCollection)
  public
    function ExistProcess(Handle: THandle): boolean;
  end;

var
  WaiterForProcessEnd: TWaiterForProcessEnd;
  WaiterForProcessEndCollection: TWaiterForProcessEndCollection;


constructor TWaiterForProcessEnd.Create;
begin
  inherited;
end;

procedure TWaiterForProcessEnd.Init;
begin
  Timer := TFPTimer.Create(nil);
  Timer.Enabled := False;
  Timer.Interval := 500;
  Timer.OnTimer := @CheckForProcessEnd;
  ProcessWaitingFor := TProcess.Create(nil);
  Fused := False;
end;

destructor TWaiterForProcessEnd.Destroy;
begin
  FreeAndNil(Timer);
  FreeAndNil(ProcessWaitingFor);
  inherited;
end;

// This is the OnTimer procedure
procedure TWaiterForProcessEnd.CheckForProcessEnd(Sender: TObject);
var
  Exitcode: longint = 0;
begin
  LogDatei.log_prog('Start CheckForProcessEnd', LLinfo);
  try
    if ProcessWaitingFor <> nil then
    begin
      if ProcessWaitingFor.Running = False then
      begin
        Timer.Enabled := False;
        Exitcode := ProcessWaitingFor.Exitcode;
        LogDatei.log('Process: ' + ProcessWaitingFor.Executable + ' terminated at: ' +
          DateTimeToStr(now) + ' with exitcode: ' + IntToStr(exitcode), LLinfo);
        //FreeAndNil(ProcessWaitingFor);
        FUsed := False;
      end
      else
      begin
        LogDatei.log_prog('Process: ' + ProcessWaitingFor.Executable + ' running', LLinfo);
      end;
    end;
  except
    on e: Exception do
    begin
      LogDatei.log('Exception in CheckForProcessEnd: ' + e.message, LLerror);
        {$IFDEF WINDOWS}
      LogDatei.log('Lasterror: ' + IntToStr(GetLastError) + ' (' +
        SysErrorMessage(GetLastError) + ')', LLWarning);
        {$ENDIF WINDOWS}
        {$IFDEF UNIX}
      LogDatei.log('Lasterror: ' + IntToStr(fpgeterrno) + ' (' +
        SysErrorMessage(fpgeterrno) + ')', LLWarning);
        {$ENDIF UNIX}
    end;
  end;
  Logdatei.log_prog('finished CheckForProcessEnd', LLinfo);
end;


function TWaiterForProcessEndCollection.ExistProcess(handle: THandle): boolean;
var
  index, i: integer;
begin
  index := Count;
  Result := False;
  for i := 0 to index - 1 do
    if handle = (Items[i] as TWaiterForProcessEnd).ProcessWaitingFor.Handle then
      Result := True;
end;

{$ENDIF UNIX}


//{$IFNDEF OPSISCRIPT}
{$IFDEF WINDOWS}
function FindFirstTask(ExeFileName: string; var processID: Dword;
  var parentProcessID: DWord; var info: string): boolean;
  // finds a task that executes ExeFilename
  // thereby retrieving its processID
var
  ContinueLoop: boolean;
  FSnapshotHandle: THandle = 0;
  FProcessEntry32: TProcessEntry32;
  Found: boolean = False;
  teststring: string = '';
begin
  //Logdatei.Log ('FindTask for ' + ExeFilename, LLinfo);
  info := '';
  Result := False;
  Found := False;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueLoop do
  begin

    teststring := UpperCase(ExtractFileName(FProcessEntry32.szExeFile));
    if (teststring = UpperCase(ExtractFileName(ExeFileName))) //'IRSETUP.EXE'
    //or (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ExeFileName))
    then
    begin
      Found := True;
      processID := FProcessEntry32.th32ProcessID;
      parentProcessID := FProcessEntry32.th32ParentProcessID;
      //Logdatei.Log (ExtractFileName(FProcessEntry32.szExeFile) + ', processId ' + #9 +
      //  IntToStr (FProcessEntry32.th32ProcessID) + #9 + 'parent process ' + #9 +
      //  IntToStr (FProcessEntry32.th32ParentProcessID), LLinfo);
    end;
    ContinueLoop := (not Found) and Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);

  Result := Found;
end;

{$ENDIF WINDOWS}
//{$ENDIF OPSISCRIPT}



function StartProcess_cp(CmdLinePasStr: string; ShowWindowFlag: integer;
  showoutput: boolean; WaitForReturn: boolean; WaitForWindowVanished: boolean;
  WaitForWindowAppearing: boolean; WaitForProcessEnding: boolean;
  waitsecsAsTimeout: boolean; Ident: string; WaitSecs: word;
  var Report: string; var ExitCode: longint; catchout: boolean;
  var output: TXStringList): boolean;

var
  ProcessStream: TMemoryStream;
  Buffer: string = '';
  BytesRead: longint;
  n: longint;
  FpcProcess: TProcess;
  WaitWindowStarted: boolean;
  desiredProcessStarted: boolean;
  WaitForProcessEndingLogflag: boolean;
  starttime, nowtime: TDateTime;

  line: string;
  filename: string;
  ParamStr: string;
  paramlist: TXStringlist;
  //dir: string;
  firstsplit: integer;
  //len: DWord;
  //functionresult: hinst;

  resultfilename: string;
  presultfilename: PChar;
  running: boolean;
  processID: Dword;
  parentProcessID: DWord;
  info: string;
  //lpExitCode: DWORD = 0;
  lpExitCode: longint = 0;
  //  var ProcessInfo: jwawinbase.TProcessInformation;
  mypid: dword = 0;
  ProcShowWindowFlag: TShowWindowOptions;
  i, k: integer;
  seccounter: integer;
  totalbytes: int64;
  seconds, hhword, ddword, mmword, msword: word;
  handle: dword;
  comparestr: string;
  winText: array [0..1024] of char;
  lengthvar: longint;

  function ReadStream(var Buffer: string; var ProcessWaitingFor: TProcess;
  var output: TXStringList; showoutput: boolean): longint;
  var
    tmp_buffer: array[0..READ_BYTES - 1] of char;//Buffer of 2048 char
    output_line: string = '';
    LineBreakPos: longint;
    BytesRead: longint;
  begin
    if ProcessWaitingFor.output.NumBytesAvailable <= 0 then
      BytesRead := 0
    else
    begin
      tmp_buffer := '';
      BytesRead := ProcessWaitingFor.output.Read(tmp_buffer, READ_BYTES);
      {$IFDEF WINDOWS}
      OemToAnsiBuff(tmp_buffer, tmp_buffer, BytesRead);
      {$ENDIF WINDOWS}
      Buffer := Buffer + tmp_buffer;

      {$IFDEF WINDOWS}
      LineBreakPos := Pos(#13, Buffer);
      {$ELSE WINDOWS}
      LineBreakPos := Pos(#10, Buffer);
      {$ENDIF WINDOWS}

      while not (LineBreakPos = 0) do
      begin
        output_line := Copy(Buffer, 1, LineBreakPos - 1);
        {$IFDEF WINDOWS}
        output_line := WINCPToUTF8(output_line);
        {$ENDIF WINDOWS}
        output.Add(output_line);
        {$IFDEF GUI}
        if showoutput then
        begin
          SystemInfo.Memo1.Lines.Add(output_line);
          //ProcessMess;
        end;
        {$ENDIF GUI}

        // skip carriage return if present
        if (length(Buffer) > LineBreakPos) and (Buffer[LineBreakPos + 1] = #10) then
          Inc(LineBreakPos, 1);

        Buffer := Copy(Buffer, LineBreakPos + 1, READ_BYTES);

        {$IFDEF WINDOWS}
        LineBreakPos := Pos(#13, Buffer);
        {$ELSE WINDOWS}
        LineBreakPos := Pos(#10, Buffer);
        {$ENDIF WINDOWS}
      end;
    end;
     {$IFDEF OPSISCRIPT}
    //Application.ProcessMessages;
    {$IFDEF GUI}
    //osinteractivegui.ProcessMess;
    osmain.ProcessMess;
    {$ELSE GUI}
    //osmain.ProcessMess;
    {$ENDIF GUI}
    {$ENDIF OPSISCRIPT}
    Result := BytesRead;
  end;

const
  secsPerDay = 86400;
  //ReadBufferSize = 2048;

begin
  logdatei.log_prog('Start with StartProcess_cp', LLinfo);
  ParamStr := '';
  paramlist := TXStringlist.Create;
  seccounter := 0;
  totalbytes := 0;

  // do we have a quoted file name ?
  if CmdLinePasStr[1] = '"' then
  begin
    line := copy(CmdLinePasStr, 2, length(CmdLinePasStr));
    firstsplit := pos('"', line);
    if firstsplit <= 0 then
    begin
      Result := False;
      Report := 'No valid filename';
      exit;
    end;
    filename := copy(line, 1, firstsplit - 1);
    ParamStr := copy(line, firstsplit + 1, length(line));
  end
  else
  begin
    // no quotes - split at space
    firstsplit := pos(' ', CmdLinePasStr);
    if firstsplit > 0 then
    begin
      filename := copy(CmdLinePasStr, 1, firstsplit - 1);
      ParamStr := copy(CmdLinePasStr, firstsplit + 1, length(CmdLinePasStr));
    end
    else
    begin
      // no space found
      filename := CmdLinePasStr;
    end;
  end;

  stringsplitByWhiteSpace(trim(ParamStr), TStringList(paramlist));
  logdatei.log_prog('command: ' + CmdLinePasStr, LLinfo);
  logdatei.log_prog('ParamStr: ' + ParamStr, LLinfo);
  logdatei.log_prog('Filename from command: ' + filename + '=' +
    ExpandFileName(filename), LLInfo);
  logdatei.log_prog('Params from command: ' + TStringList(paramlist).Text, LLInfo);
  //writeln('>->->'+paramstr);
  //writeln('>->->'+CmdLinePasStr);
  try
    try
      Buffer := '';

      {$IFDEF UNIX}
      // initialize wait for ProcessWaitingFor end object
      // clear unused objects from the collection
      // https://stackoverflow.com/questions/6547760/delphi-remove-an-object-from-a-tobjectlist
      for k := pred(WaiterForProcessEndCollection.Count) downto 0 do
        if not (WaiterForProcessEndCollection.Items[k] as TWaiterForProcessEnd).Fused then
        begin
          WaiterForProcessEndCollection.Delete(k);
          logdatei.log_prog('deleted ProcessWaitingFor from collection by index: ' +
            IntToStr(k), LLinfo);
        end;
      // create and init wait for ProcessWaitingFor end object
      WaiterForProcessEnd := WaiterForProcessEndCollection.add as TWaiterForProcessEnd;
      WaiterForProcessEnd.init;
      // set FpcProcess to point to the ProcessWaitingFor of the wait for ProcessWaitingFor end object
      FpcProcess := WaiterForProcessEnd.ProcessWaitingFor;
      {$ENDIF UNIX}

      {$IFDEF WINDOWS}
      FpcProcess := TProcess.Create(nil);
      {$ENDIF WINDOWS}
      FpcProcess.Executable := filename;
      FpcProcess.Parameters := TStringList(paramlist);
      logdatei.log_prog('command: ' + FpcProcess.Executable, LLinfo);

      if not WaitForReturn then
        catchout := False;

      if catchout then
        FpcProcess.Options := FpcProcess.Options + [poUsePipes, poStdErrToOutPut];

      //FpcProcess.StartupOptions := [suoUseShowWindow, suoUseSize, suoUsePosition];

      case ShowWindowFlag of
        SW_HIDE: ProcShowWindowFlag := swoHIDE;
        SW_MINIMIZE: ProcShowWindowFlag := swoMinimize;
        SW_MAXIMIZE: ProcShowWindowFlag := swoMaximize;
        SW_NORMAL: ProcShowWindowFlag := swoShowNormal;   // swoNone ?
        SW_RESTORE: ProcShowWindowFlag := swoRestore;
        SW_SHOW: ProcShowWindowFlag := swoShow;
        //SW_SHOWMAXIMIZED : ProcShowWindowFlag := swoShowMaximized;
        SW_SHOWMINIMIZED: ProcShowWindowFlag := swoShowMinimized;
        SW_SHOWMINNOACTIVE: ProcShowWindowFlag := swoshowMinNOActive;
        SW_SHOWNA: ProcShowWindowFlag := swoShowNA;
        SW_SHOWNOACTIVATE: ProcShowWindowFlag := swoShowNoActivate;
          //SW_SHOWNORMAL : ProcShowWindowFlag := swoShowNormal;
        else
          ProcShowWindowFlag := swoShow;
      end;
      FpcProcess.ShowWindow := ProcShowWindowFlag;
      FpcProcess.Execute;
      //FillChar(processInfo, SizeOf(processInfo), 0);
      //CreateProcessElevated(lpApplicationName: PChar; lpCommandLine: String;
      //lpCurrentDirectory: PChar;Counter: Integer; var ProcessInfo: TProcessInformation): Boolean;
      //if not CreateProcessElevated(nil, CmdLinePasStr, PChar(GetCurrentDir),0, ProcessInfo) then
      //begin
      //  result := false;
      //  Logdatei.Log('Could not start process ', LLError);
      //end
      //else
      begin
        Result := True;
        logdatei.log('Started process "' + FpcProcess.Executable +
          '" with Opt: ' + FpcProcess.Parameters.Text, LLInfo);
        desiredProcessStarted := False;
        WaitForProcessEndingLogflag := True;
        setLength(resultfilename, 400);
        presultfilename := PChar(resultfilename);
        //mypid := FpcProcess.ProcessID;

        //FindExecutable(PChar(filename), nil, presultfilename);
        //FindFirstTask ( ExtractFilename(presultfilename), processID, parentProcessID, info);
        //allChildrenIDs := TStringList.create;
        //allChildrenIDs.add ( inttoStr(processID));
        {$IFDEF UNIX}
        // process started - start wait for ProcessWaitingFor end
        WaiterForProcessEnd.timer.Enabled := True; // start the timer
        WaiterForProcessEnd.Fused := True;  // mark object as used
        CheckSynchronize;  // used to help the timer thread
        {$ENDIF UNIX}
        if not WaitForReturn and (WaitSecs = 0) then
        begin
          Report := 'Process started:    ' + CmdLinePasStr;
        end
        else
        begin
          running := True;
          starttime := now;
          WaitWindowStarted := False;
          {$IFDEF GUI}
          if waitsecsAsTimeout and (WaitSecs > 5) then
          begin
            FBatchOberflaeche.SetElementVisible(True, eProgressBar);
            // showProgressBar(True);
            //FBatchOberflaeche.setProgress(0);
          end;
          {$ENDIF GUI}

          while running do
          begin
            {$IFDEF UNIX}
            CheckSynchronize;  // used to help the timer thread
            {$ENDIF UNIX}
            nowtime := now;
            running := False;
            DecodeTime((nowtime - starttime), hhword, mmword, seconds, msword);

            if catchout then
            begin
              repeat
                n := ReadStream(Buffer, FPCProcess, output, showoutput);
                totalbytes := totalbytes + n;
                //LogDatei.log('read from process bytes: '+IntToStr(n),LLinfo);
              until n <= 0;
            end;

            //wait for task vanished
            {$IFDEF WINDOWS}
            if WaitForWindowAppearing then
            begin
              //waiting condition 0:
              //wait until a window is appearing
              //if FindWindowEx(0, 0, nil, PChar(Ident)) = 0 then
              handle := FindWindow(nil, PChar(Ident));
              //logdatei.log('LastError: '+IntToStr(GetLastError)
              //  + ' (' + SysErrorMessage(GetLastError) + ')',LLinfo);;
              if (handle = Invalid_Handle_Value) or (handle = 0) or
                (GetLastError <> 0) then
              begin
                if seconds > seccounter then
                begin
                  logdatei.log('Wait for appear Window: "' + Ident +
                    '" not found.', LLDebug);
                end;
                if WaitSecs = 0 then
                  running := True
                else
                begin //time out given
                  if ((nowtime - starttime) < waitSecs / secsPerDay) then
                  begin
                    running := True;
                  end
                  else
                  begin
                    logdatei.log('Wait for appear Window "' + ident +
                      '" stopped - time out ' + IntToStr(waitSecs) + ' sec', LLInfo);
                  end;
                end;
              end
              else
              begin
                if (GetWindowText(handle, winText, SizeOf(winText)) >
                  0) and (winText = ident) then
                  logdatei.log('Wait for appear Window: "' + Ident +
                    '" found.', LLDebug)
                else
                begin
                  running := True;
                  if seconds > seccounter then
                  begin
                    logdatei.log('Wait for appear Window: "' + Ident +
                      '" not found. Found: ' + winText, LLDebug);
                  end;
                end;
              end;
            end

            else if WaitForWindowVanished and not WaitWindowStarted then
            begin
              //waiting condition 1:
              //we are waiting for a window that will later vanish
              //but this window did not appear yet
              //if FindWindowEx(0, 0, nil, PChar(Ident)) <> 0 then
              if FindWindow(nil, PChar(Ident)) <> 0 then
              begin
                WaitWindowStarted := True;
                logdatei.log('Wait for vanish Window: "' + Ident + '" found.', LLDebug);
              end;

              if not WaitWindowStarted or WaitForWindowVanished then
                // in case WaitForWindowVanished we are not yet ready
                // but have to check waiting condition 3
                if WaitSecs = 0 then
                  running := True
                else
                begin //time out given
                  if ((nowtime - starttime) < waitSecs / secsPerDay) then
                  begin
                    running := True;
                  end
                  else
                  begin
                    logdatei.log('Wait for vanish Window "' + ident +
                      '" stopped - time out ' + IntToStr(waitSecs) + ' sec', LLInfo);
                  end;
                end;
            end

            else
           {$ENDIF WINDOWS}
            if not waitsecsAsTimeout and (WaitSecs > 0) and
              ((nowtime - starttime) < waitSecs / secsPerDay) then
            begin
              // waiting condition 2 : we shall observe a waiting time
              // and it has not finished
              running := True;
            end

            else
            //{$IFDEF WINDOWS}
            if WaitForProcessEnding and not desiredProcessStarted then
            begin
              //waiting condition 3a : we wait that some other process will come into existence
              if WaitForProcessEndingLogflag then
              begin
                if seconds > seccounter then
                  logdatei.log('Waiting for start of "' + ident + '"', LLInfo);
                WaitForProcessEndingLogflag := False;
              end;
              {$IFDEF WINDOWS}
              desiredProcessStarted :=
                FindFirstTask(PChar(Ident), processID, parentProcessID, info);
              {$ELSE}
              desiredProcessStarted := ProcessIsRunning(Ident);
              {$ENDIF}

              if WaitSecs = 0 then
                running := True
              else
              begin //time out given
                if ((nowtime - starttime) < waitSecs / secsPerDay) then
                begin
                  running := True;
                end
                else
                begin
                  logdatei.log('Waiting for "' + ident +
                    '" stopped - time out ' + IntToStr(waitSecs) + ' sec', LLInfo);
                end;
              end;

            end

            else if WaitForProcessEnding and desiredProcessStarted then
            begin
              //waiting condition 3b : now we continue waiting until the observed other process will stop
              {$IFDEF WINDOWS}
              running :=
                FindFirstTask(PChar(Ident), processID, parentProcessID, info);
              {$ELSE}
              running := ProcessIsRunning(Ident);
              {$ENDIF}


              if not WaitForProcessEndingLogflag and running then
              begin
                if seconds > seccounter then
                  logdatei.log('Waiting for process "' + ident +
                    '" ending', LLinfo);
                WaitForProcessEndingLogflag := True;
              end;

              if not running then
              begin
                logdatei.log('Process "' + ident + '" ended', LLinfo);
                // After the process we waited for has ended, the Parent may be still alive
                // in this case we have to wait for the end of the parent
                {$IFDEF WINDOWS}
                if GetExitCodeProcess(FpcProcess.ProcessHandle, longword(lpExitCode)) and
                  (lpExitCode = still_active) then
                begin
                  running := True;
                  WaitForProcessEnding := False;
                end;
                {$ENDIF WINDOWS}
                {$IFDEF UNIX}
                lpExitCode := FpcProcess.ExitCode;
                if FpcProcess.Running then
                begin
                  running := True;
                  WaitForProcessEnding := False;
                end
                else
                begin
                  lpExitCode := FpcProcess.ExitCode;
                  logdatei.log(
                    'Process : ' + FpcProcess.Executable + ' terminated at: ' +
                    DateTimeToStr(now) + ' exitcode is: ' +
                    IntToStr(lpExitCode), LLInfo);
                end;
                {$ENDIF LINUX}
              end;
              if running then
              begin

                if (waitSecs > 0) // we look for time out
                  and  //time out occured
                  ((nowtime - starttime) >= waitSecs / secsPerDay) then
                begin
                  running := False;
                  logdatei.log('Error: Timeout: Waiting for ending of "' +
                    ident + '" stopped - waitSecs out ' + IntToStr(waitSecs) +
                    ' sec', LLError);
                end;

              end;

            end
            else if not FpcProcess.Running then
            begin
              // waiting condition 4 :  Process has finished;
              //   we still have to look if WindowToVanish did vanish if this is necessary
              lpExitCode := FpcProcess.ExitCode;
              if seconds > seccounter then
                logdatei.log(
                  'Process terminated at: ' + DateTimeToStr(now) +
                  ' exitcode is: ' + IntToStr(lpExitCode), LLInfo);
              {$IFDEF WINDOWS}
              if WaitForWindowVanished then
              begin
                //if not (FindWindowEx(0, 0, nil, PChar(Ident)) = 0) then
                if not (FindWindow(nil, PChar(Ident)) = 0) then
                begin
                  running := True;
                end;
              end;
              {$ENDIF WINDOWS}

            end

            else if waitForReturn then
            begin
              //waiting condition 4 : Process is still active
              if waitsecsAsTimeout and
                (waitSecs > 0) // we look for time out
                and  //time out occured
                ((nowtime - starttime) >= waitSecs / secsPerDay) then
              begin
                running := False;
                logdatei.log('Error: Timeout: Waited for the end of started process"' +
                  ' - but time out reached after ' + IntToStr(waitSecs) +
                  ' sec.', LLError);
              end
              else
              begin
                running := True;
              end;
            end;

            if running then
            begin
              //ProcessMess;
              //sleep(1);
              sleep(10);
              //sleep(50);
              //sleep(1000);
              {$IFDEF UNIX}
              lpExitCode := FpcProcess.ExitCode;
              {$ENDIF LINUX}
              {$IFDEF WINDOWS}
              GetExitCodeProcess(FpcProcess.ProcessHandle, longword(lpExitCode));
              ProcessMess;
              {$ENDIF WINDOWS}
              //GetExitCodeProcess(ProcessInfo.hProcess, lpExitCode);
              {$IFDEF GUI}
              if waitsecsAsTimeout and (WaitSecs > 5) then
              begin
                FBatchOberflaeche.setProgress(round(
                  ((nowtime - starttime) / (waitSecs / secsPerDay)) * 100), pPercent);
              end;
              {$IFDEF WINDOWS}
              ProcessMess;
              {$ENDIF WINDOWS}
             {$ENDIF GUI}

              DecodeTime((nowtime - starttime), hhword, mmword, seconds, msword);
              if seconds > seccounter then
              begin
                logdatei.log('Waiting for ending at ' +
                  DateTimeToStr(now) + ' exitcode is: ' + IntToStr(lpExitCode) +
                  ' output bytes read: ' + IntToStr(totalbytes), LLDebug2);
                seccounter := seconds;
              end;
              {$IFDEF WINDOWS}
              if (lpExitCode = 259) and (not FpcProcess.Running) then
              begin
                logdatei.log('Strange: Process ended but exitcode 259 ',
                  LLinfo);
                //running := False;
              end;
              ProcessMess;
              {$ENDIF WINDOWS}
            end;
          end;

          {$IFDEF WINDOWS}
          ProcessMess;
          {$ENDIF WINDOWS}

          if catchout then
          begin
            // read remaining output
            repeat
              n := ReadStream(Buffer, FPCProcess, output, showoutput);
              //LogDatei.log('read from process bytes: '+IntToStr(n),LLinfo);
            until n <= 0;

            // add remainder of buffer as last line
            if Buffer <> '' then
              output.Add(Buffer);
          end;
          {$IFDEF WINDOWS}
          ProcessMess;
          {$ENDIF WINDOWS}

          exitCode := FpcProcess.ExitCode;
          Report := 'ExitCode ' + IntToStr(exitCode) + '    Executed process "' +
            CmdLinePasStr + '"';
        end;
        (*
        {$IFDEF WINDOWS}
        ProcessMess;
        {$ENDIF WINDOWS}

        exitCode := FpcProcess.ExitCode;
        Report := 'ExitCode ' + IntToStr(exitCode) + '    Executed process "' +
          CmdLinePasStr + '"';
        *)
      end;
    except
      on e: Exception do
      begin
        LogDatei.log('Exception in StartProcess_cp: ' + e.message, LLDebug);
        {$IFDEF WINDOWS}
        logdatei.log('Lasterror: ' + IntToStr(GetLastError) + ' (' +
          SysErrorMessage(GetLastError) + ')', LLWarning);
        {$ENDIF WINDOWS}
        {$IFDEF UNIX}
        logdatei.log('Lasterror: ' + IntToStr(fpgeterrno) + ' (' +
          SysErrorMessage(fpgeterrno) + ')', LLWarning);
        {$ENDIF UNIX}
        Report := 'Could not execute process "' + CmdLinePasStr + '"';
        exitcode := -1;
        Result := False;
      end;
    end;
  finally
     {$IFDEF UNIX}
    if not WaiterForProcessEnd.ProcessWaitingFor.Running then
    begin
      // process is not running so we do not need the wait object any more
      WaiterForProcessEnd.FUsed := False;
      WaiterForProcessEnd.Timer.Enabled := False;
    end;
    // FpcProcess is a copy of the process in the global wait object
    // so do not free it (but since we do not need it it could be nil)
    FpcProcess := nil;
    CheckSynchronize;  // used to help the timer thread
     {$ENDIF UNIX}
     {$IFDEF WINDOWS}
    FreeAndNil(FpcProcess);
     {$ENDIF WINDOWS}
     {$IFDEF GUI}
    FBatchOberflaeche.SetElementVisible(False, eProgressBar); //showProgressBar(False);
    {$ENDIF GUI}
  end;
end;

begin
  {$IFDEF UNIX}
  // initialize the collection of process wait objects
  WaiterForProcessEndCollection := TWaiterForProcessEndCollection.Create(nil,TWaiterForProcessEnd);
  {$ENDIF UNIX}
end.
