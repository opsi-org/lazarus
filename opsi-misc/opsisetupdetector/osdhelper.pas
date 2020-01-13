unit osdhelper;

{$mode delphi}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  ShlObj,
  Registry,
  DSIWin32,
  {$ENDIF WINDOWS}
  Classes,
  Process,
  SysUtils;

{$IFDEF WINDOWS}
procedure registerForWinExplorer(doregister: boolean);
function getSpecialFolder(csidlValue: integer): string;
{$ENDIF WINDOWS}
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
function grepexe(instring: string): string;

implementation

uses
  osdform;

{$IFDEF WINDOWS}
procedure registerForWinExplorer(doregister: boolean);
var
  myreg: Tregistry;
  IsAdmin:boolean;
begin
  myreg := TRegistry.Create(KEY_ALL_ACCESS);
  //myreg.RootKey := HKEY_CURRENT_USER;
  myreg.RootKey := HKEY_CLASSES_ROOT;
  //remove old registration
  myreg.DeleteKey('Software\Classes\*\shell\opsi setup detector\Command');
  myreg.DeleteKey('Software\Classes\*\shell\opsi setup detector');
  myreg.DeleteKey('*\shell\opsi setup detector\Command');
  myreg.DeleteKey('*\shell\opsi setup detector');
  myreg.DeleteKey('Software\Classes\exefile\shell\opsi setup detector\Command');
  myreg.DeleteKey('Software\Classes\exefile\shell\opsi setup detector');
  myreg.DeleteKey('Software\Classes\Msi.Package\shell\opsi setup detector\Command');
  myreg.DeleteKey('Software\Classes\Msi.Package\shell\opsi setup detector');
  //new registration
  isAdmin := DSiIsAdmin;
  if DSiIsAdmin then myreg.RootKey := HKEY_CLASSES_ROOT
  else myreg.RootKey := HKEY_CURRENT_USER;
  if doregister then
  begin
    myreg.OpenKey('exefile\shell\opsi setup detector', True);
    myreg.WriteString('', 'Analyze with opsi-setup-detector');
    myreg.CloseKey;
    myreg.OpenKey('exefile\shell\opsi setup detector\Command', True);
    myreg.WriteString('', '"' + ParamStr(0) + '" --filename="%1"');
    myreg.CloseKey;
    myreg.OpenKey('Msi.Package\shell\opsi setup detector', True);
    myreg.WriteString('', 'Analyze with opsi-setup-detector');
    myreg.CloseKey;
    myreg.OpenKey('Msi.Package\shell\opsi setup detector\Command', True);
    myreg.WriteString('', '"' + ParamStr(0) + '" --filename="%1"');
  end
  else
  begin
    myreg.DeleteKey('exefile\shell\opsi setup detector\Command');
    myreg.DeleteKey('exefile\shell\opsi setup detector');
    myreg.DeleteKey('Msi.Package\shell\opsi setup detector\Command');
    myreg.DeleteKey('Msi.Package\shell\opsi setup detector');
  end;
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

{$ENDIF WINDOWS}




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
        Mywrite('Exception in RunCommandAndCaptureOut: ' + e.message);
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




