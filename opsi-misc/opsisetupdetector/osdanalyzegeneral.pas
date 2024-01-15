unit osdanalyzegeneral;

{$mode delphi}

interface

uses
   {$IFDEF WINDOWS}
  Windows,
  ShlObj,
  Registry,
  verinfo,
  {$ENDIF WINDOWS}
  Dialogs,
  LCLType,
  Classes,
  osdhelper,
  Process,
  fileutil,
  lazfileutils,
  SysUtils,
  strutils,
  fileinfo,
  winpeimagereader,
  oslog,
  osdbasedata,
  oscheckbinarybitness,
  masks,
  osparserhelper;

function getPacketIDfromFilename(str: string): string;
function getPacketIDShort(str: string): string;
function ExtractVersion(str: string): string;
function ExtractVersionFromFilename(str: string): string;
procedure grepmsi(instring: string);
function grepexe(instring: string): string;
procedure analyze_binstr(instring: string; var mysetup: TSetupFile);
function analyze_binary(myfilename: string; verbose, skipzero: boolean;
  var mysetup: TSetupFile): TKnownInstaller;

implementation

{$IFDEF OSDGUI}
uses
  osdform,
  osdmain;
{$ELSE OSDGUI}
uses
  osdmain;
{$ENDIF OSDGUI}

function getPacketIDfromFilename(str: string): string;
var
  strnew: string;
  i: integer;
  myChar: char;
begin
  strnew := '';
  for i := 1 to Length(str) do
  begin
    myChar := str[i];
    if myChar in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-'] then
      strnew := strnew + myChar
    else
    if (myChar <> #195) then
      strnew := strnew + '-';
  end;
  Result := cleanOpsiId(strnew);
end;


function getPacketIDShort(str: string): string;
var
  strnew: string;
  i: integer;
  myChar: char;
  preChar: char = ' ';
begin
  strnew := '';
  str := lowercase(str);
  for i := 1 to Length(str) do
  begin
    myChar := str[i];
    if myChar in ['a'..'z', '0'..'9', '_', '-'] then
    begin
      strnew := strnew + myChar;
      preChar := myChar;
    end
    else
    begin
      if ((myChar <> #195) and (i > 1) and (strnew[Length(strnew) - 1] <> '_') and
        (strnew[Length(strnew) - 1] <> '-') and (i < Length(str))) then
        if (preChar <> '-') then
        begin
          strnew := strnew + '-';
          preChar := '-';
        end;
    end;
  end;
  Result := cleanOpsiId(strnew);
end;


function ExtractVersion(str: string): string;
var
  i: integer;
  outstr: string = '';
begin
  str := StringReplace(str, 'w32', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'w64', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'win32', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'win64', '', [rfReplaceAll, rfIgnoreCase]);
  for i := 1 to Length(str) do
  begin
    if str[i] in ['0'..'9', '.'] then
    begin
      if (Length(outstr) > 0) and (not (str[i - 1] in ['0'..'9', '.'])) then
        outstr := '';
      outstr := outstr + str[i];
    end
    else;
  end;
  Result := outstr;
end;

function ExtractVersionFromFilename(str: string): string;
var
  i: integer;
  outstr: string = '';
begin
  // try to strip productId from filename
  str := StringReplace(str, 'w32', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'w64', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'win32', '', [rfReplaceAll, rfIgnoreCase]);
  str := StringReplace(str, 'win64', '', [rfReplaceAll, rfIgnoreCase]);
  str := copy(str, pos('-', str), length(str));
  str := StringReplace(str, '_', '.', [rfReplaceAll, rfIgnoreCase]);
  for i := 1 to Length(str) do
  begin
    if str[i] in ['0'..'9', '.'] then
    begin
      if (Length(outstr) > 0) and (not (str[i - 1] in ['0'..'9', '.'])) then
        outstr := '';
      outstr := outstr + str[i];
    end
    else;
  end;
  Result := outstr;
end;



function grepexe(instring: string): string;
var
  lowerstring: string;
begin
  Result := '';
  lowerstring := lowercase(instring);
  if (0 < pos('installshield', lowerstring)) or (0 < pos('inno', lowerstring)) or
    (0 < pos('wise', lowerstring)) or (0 < pos('nullsoft', lowerstring)) or
    (0 < pos('wixquery', lowerstring)) or
    (0 < pos('product_build_number{', lowerstring)) or
    (0 < pos('productcode{', lowerstring)) or (0 < pos('msiexec', lowerstring)) or
    (0 < pos('extract', lowerstring)) or
    // (0 < pos('setup', lowerstring)) or
    (0 < pos('installer', lowerstring)) then
    Result := instring;
end;

procedure grepmsi(instring: string);
begin
  if (0 < pos('product_build_number{', lowercase(instring))) or
    (0 < pos('productcode{', lowercase(instring))) then
    write_log_and_memo(instring);
end;

function grepinstr(instring: string; searchstr: string): string;
var
  lowerstring: string;
begin
  Result := '';
  lowerstring := lowercase(instring);
  if (0 < pos(lowercase(searchstr), lowerstring)) then
    Result := instring;
end;


function analyze_markerlist(var mysetup: TSetupFile): TKnownInstaller;
var
  i: integer;

begin
  try
    Result := stUnknown;
    for i := 0 to mysetup.markerlist.Count - 1 do
      LogDatei.log('marker: ' + mysetup.markerlist[i], LLnotice);
    for i := 0 to integer(stUnknown) - 1 do
    begin
      if not Assigned(installerArray[i].detected) then
        LogDatei.log('No check implemented for: ' +
          installerToInstallerstr(TKnownInstaller(i)), LLWarning)
      else
      begin
        LogDatei.log('Check markerlist for: ' + installerToInstallerstr(
          TKnownInstaller(i)), LLInfo);
        if installerArray[i].detected(TClass(installerArray[i]),
          mysetup.markerlist) then
        begin
          Result := TKnownInstaller(i);
          LogDatei.log('Detected: ' + installerToInstallerstr(Result), LLnotice);
          osdsettings.DetectCount := osdsettings.DetectCount +1;
        end;
      end;
    end;

  except
    on E: Exception do
    begin
      LogDatei.log('Exception in analyze_markerlist', LLcritical);
      LogDatei.log('Error: Message: ' + E.message, LLcritical);
      system.ExitCode := 1;
    end;
  end;
end;

procedure analyze_binstr(instring: string; var mysetup: TSetupFile);
var
  lowerstring: string;
  counter: integer;
  aktId: TKnownInstaller;


  procedure check_line_for_installer(line: string; instId: TKnownInstaller;
  var mysetup: TSetupFile);
  var
    i: integer;
    aktpattern: string;
  begin
    for i := 0 to installerArray[integer(instId)].patterns.Count - 1 do
    begin
      aktpattern := LowerCase(installerArray[integer(instId)].patterns[i]);
      if 0 <> pos(aktpattern, line) then
      begin
        mysetup.markerlist.add(installerArray[integer(instId)].patterns[i]);
        LogDatei.log('For: ' + installerToInstallerstr(instId) +
          ' found: ' + LowerCase(installerArray[integer(instId)].patterns[i]), LLNotice);
      end;
    end;
    for i := 0 to installerArray[integer(instId)].infopatterns.Count - 1 do
    begin
      aktpattern := LowerCase(installerArray[integer(instId)].infopatterns[i]);
      if 0 <> pos(aktpattern, line) then
      begin
        mysetup.markerlist.add(installerArray[integer(instId)].infopatterns[i]);
        LogDatei.log('Infolevel for: ' + installerToInstallerstr(instId) +
          ' found: ' + LowerCase(installerArray[integer(instId)].infopatterns[i]), LLNotice);
      end;
    end;
    for i := 0 to installerArray[integer(instId)].notpatterns.Count - 1 do
    begin
      aktpattern := LowerCase(installerArray[integer(instId)].notpatterns[i]);
      if 0 <> pos(aktpattern, line) then
      begin
        mysetup.markerlist.add(installerArray[integer(instId)].notpatterns[i]);
        LogDatei.log('Against: ' + installerToInstallerstr(instId) +
          ' found: ' + LowerCase(installerArray[integer(instId)].notpatterns[i]), LLNotice);
      end;
    end;
  end;

  procedure check_line_for_infoline(line: string; instId: TKnownInstaller;
  var mysetup: TSetupFile);
  var
    i: integer;
  begin
    for i := 0 to installerArray[integer(instId)].infopatterns.Count - 1 do
    begin
      if 0 <> pos(LowerCase(installerArray[integer(instId)].infopatterns[i]), line) then
      begin
        mysetup.infolist.add(line);
        LogDatei.log('For: ' + installerToInstallerstr(instId) +
          ' found info: ' + line, LLinfo);
      end;
    end;
  end;

begin
  lowerstring := lowercase(instring);
  for counter := 0 to knownInstallerList.Count - 1 do
  begin
    aktId := installerArray[counter].installerId;
    if aktId <> stUnknown then
    begin
      check_line_for_installer(lowerstring, aktId, mysetup);
      check_line_for_infoline(lowerstring, aktId, mysetup);
    end;
  end;
end;



function analyze_binary(myfilename: string; verbose, skipzero: boolean;
  var mysetup: TSetupFile): TKnownInstaller;
var
  FileStream: TFileStream;
  CharIn: char;
  MinLen, MaxLen: integer;
  CurrValue: string;
  i: integer;
  size, fullsize: int64;
  buffer: array [0 .. 2047] of char;
  charsread: int64;
  msg: string;
  setuptype: TKnownInstaller;
  progress, lastprogress: int64;
  fileextension: string;

begin
  MinLen := 5;
  MaxLen := 512;
  CurrValue := '';
  setupType := stUnknown;
  Result := stUnknown;

  write_log_and_memo('------------------------------------');
  write_log_and_memo('Analyzing: ' + myfilename);
  msg := 'stringsgrep started (verbose:';
  if verbose = True then
    msg := msg + 'true'
  else
    msg := msg + 'false';
  msg := msg + ', skipzero:';
  if skipzero = True then
    msg := msg + 'true'
  else
    msg := msg + 'false';
  msg := msg + ')';
  write_log_and_memo(msg);
  FileStream := TFileStream.Create(myfilename, fmOpenRead);
  try
    {$IFDEF OSDGUI}
    resultForm1.ProgressBarAnalyze.Position := 0;
    procmess;
        {$ENDIF OSDGUI}
    fullsize := FileStream.Size;
    size := fullsize;
    lastprogress := 0;
    progress := 0;
    while (size > 0) and (setupType = stUnknown) do
    begin
      charsread := FileStream.Read(buffer, sizeof(buffer));
      size := size - charsread;
       {$IFDEF OSDGUI}
      progress := 100 - trunc((size / fullsize) * 100);
      if progress > lastprogress then
      begin
        resultForm1.ProgressBarAnalyze.Position := progress;
        procmess;
        LogDatei.log('AnaProgess: ' + IntToStr(progress), LLDebug);
        lastprogress := progress;
      end;
       {$ENDIF OSDGUI}

      for i := 0 to charsread - 1 do
      begin

        charIn := buffer[i];

        // skipzero: handling of wide strings by ignoring zero byte
        if skipzero and (CharIn = #0) then
          continue;

        // if (CharIn in [' ','A'..'Z','a'..'z','0'..'9','<','>','.','/','_','-']) and (Length(CurrValue) < MaxLen) then
        if (CharIn in [#32..#126]) and (Length(CurrValue) < MaxLen) then
          CurrValue := CurrValue + CharIn;

        if (Length(CurrValue) < MaxLen) and (i < charsread - 1) then
          continue;

        if (Length(CurrValue) >= MinLen) then
        begin
          fileextension := lowercase(ExtractFileExt(myfilename));

          //if MatchesMaskList(myfilename,'.exe;.bin;.sh; .run') then
          if not ('.msi' = fileextension) then
          begin
            if verbose then
            begin
              //grepexe(CurrValue);
              analyze_binstr(CurrValue, mysetup);
              logdatei.log(CurrValue, LLDebug2);
            end
            else
              analyze_binstr(CurrValue, mysetup);
          end
          else if '.msi' = fileextension then
          begin
            setupType := stMsi;
            if verbose then
            begin
              grepmsi(CurrValue);
              logdatei.log(CurrValue, LLDebug2);
            end;
          end
          else
          begin
            grepexe(CurrValue);
            grepmsi(CurrValue);
            logdatei.log(CurrValue, LLDebug2);
          end;
          CurrValue := '';
        end;
      end;

    end;
    msg := 'stringsgrep completed (verbose:';
    if verbose = True then
      msg := msg + 'true'
    else
      msg := msg + 'false';
    msg := msg + ', skipzero:';
    if skipzero = True then
      msg := msg + 'true'
    else
      msg := msg + 'false';
    msg := msg + ')';
    write_log_and_memo(msg);
    write_log_and_memo('------------------------------------');
  finally
    FileStream.Free;
  end;
  Result := analyze_markerlist(mysetup);
end;


end.
