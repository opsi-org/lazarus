unit osdhelper;

{$mode delphi}

interface

uses
  Classes,
  Windows,
  Registry,
  Process,
  ShlObj,
  SysUtils;

procedure registerForExplorer;
function getSpecialFolder(csidlValue: integer): string;
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
function grepexe(instring: string) : string;

implementation
uses
  resultform;


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





function grepexe(instring: string) : string;
var lowerstring: string;
begin
  result := '';
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
       result :=instring;
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


end.

