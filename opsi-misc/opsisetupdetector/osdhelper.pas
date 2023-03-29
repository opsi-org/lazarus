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
  SysUtils,
  osparserhelper;

{$IFDEF WINDOWS}
procedure registerForWinExplorer(doregister: boolean);
function getSpecialFolder(csidlValue: integer): string;
{$ENDIF WINDOWS}
function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
function grepexe(instring: string): string;
function CompareDotSeparatedNumbers(s1, s2, s3: string): boolean;

implementation

uses
  osdform;

{$IFDEF WINDOWS}
procedure registerForWinExplorer(doregister: boolean);
var
  myreg: Tregistry;
  IsAdmin: boolean; //for testing
begin
  myreg := TRegistry.Create(KEY_ALL_ACCESS);
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
  isAdmin := DSiIsAdminLoggedOn;  // for testing
  if DSiIsAdminLoggedOn then
    myreg.RootKey := HKEY_CLASSES_ROOT
  else
    myreg.RootKey := HKEY_CURRENT_USER;
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
    end;
  end
  else
  begin
    if SUCCEEDED(SHGetFolderPath(0, csidlValue, 0, 0, namebuf)) then
      Result := StrPas(namebuf);
  end;
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
    (0 < pos('extract', lowerstring)) or (0 < pos('installer', lowerstring)) then
    Result := instring;
end;


function RunCommandAndCaptureOut
  (cmd: string; catchOut: boolean; var outlines: TStringList;
  var report: string; showcmd: integer; var ExitCode: longint): boolean;
const
  ReadBufferSize = 2048;

var
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
      while FpcProcess.Running do
      begin
        // stellt sicher, dass wir Platz haben
        M.SetSize(BytesRead + ReadBufferSize);

        // versuche, es zu lesen
        if FpcProcess.Output.NumBytesAvailable > 0 then
        begin
          n := FpcProcess.Output.Read((M.Memory + BytesRead)^, ReadBufferSize);
          if n > 0 then
          begin
            Inc(BytesRead, n);
          end;
        end
        else
        begin
          // keine Daten, warte 100 ms
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
          end;
        end
        else
          n := 0;
      until n <= 0;
      M.SetSize(BytesRead);

      S := TStringList.Create;
      S.LoadFromStream(M);
      for n := 0 to S.Count - 1 do
      begin
        outlines.Add(S[n]);
      end;
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

//####################################################################

function getDecimalCompareSign
  (const decimalString1, decimalString2: string; var sign: integer;
  var InfoSyntaxError: string; stringcomparison: boolean): boolean;

  function containsDigitsOnly(s: string): boolean;
  var
    c: char;
  begin
    Result := True;
    for  c in s do
    begin
      if not (c in ['0'..'9']) then
      begin
        Result := False;
        break;
      end;
    end;
  end;

  function leadingZero(s1: string; s2: string): boolean;
  begin
    if (length(s1) > 0) and (length(s2) > 0) then
    begin
      if (s1[1] = '0') or (s2[1] = '0') then
        Result := True
      else
        Result := False;
    end
    else
      Result := False;
  end;

  function tryDouble(var d1, d2: double; s1, s2: string): boolean;
  var
    str1, str2: string;
  begin
    Result := True;
    try
      str1 := '0' + DefaultFormatSettings.DecimalSeparator + s1;
      str2 := '0' + DefaultFormatSettings.DecimalSeparator + s2;
      d1 := StrToFloat(str1);
      d2 := StrToFloat(str2);
    except
      InfoSyntaxError := 'Expecting a sequence of "." and numbers';
      Result := False;
    end;
  end;

  function tryInteger(var i1, i2: integer; s1, s2: string): boolean;
  begin
    Result := True;
    try
      i1 := StrToInt(s1);
      i2 := StrToInt(s2);
    except
      InfoSyntaxError := 'Expecting a sequence of "." and numbers';
      Result := False;
    end;
  end;

  function extractNumbers(s: string): string;
  const
    n = ['0'..'9'];
  var
    i: integer;
  begin
    i := 1;
    Result := '';
    while i < s.Length + 1 do
    begin
      if s[i] in n then
        Result := Result + s[i];
      Inc(i);
    end;
  end;

  function extractNonNumbers(s: string): string;
  const
    n = ['0'..'9'];
  var
    i: integer;
  begin
    i := 1;
    Result := '';
    while i < s.Length + 1 do
    begin
      if not (s[i] in n) then
        Result := Result + s[i];
      Inc(i);
    end;
  end;

var
  decimals1: TStringList;
  decimals2: TStringList;
  i: integer;
  comparing: boolean;
  number1: integer;
  number2: integer;
  doublevalue1: double;
  doublevalue2: double;
  string1: string;
  string2: string;

begin

  Result := True;
  if (trim(decimalString1) = '') or (trim(decimalString2) = '') then
  begin
    Result := False;
    InfoSyntaxError := 'Missing value for comparing';
    exit;
  end;


  decimals1 := TStringList.Create;
  decimals2 := TStringList.Create;
  stringsplit(trim(decimalString1), '.', decimals1);
  stringsplit(trim(decimalString2), '.', decimals2);


  sign := 0; // we start with assumption equality
  comparing := True;
  i := 1;

  while Result and comparing do
  begin

    if (decimals1.Count >= i) and (decimals2.Count < i) then
    begin
      comparing := False;
      if sign = 0 // components were equal up to now
      then
        sign := 1;
    end
    else if (decimals1.Count < i) and (decimals2.Count >= i) then
    begin
      comparing := False;
      if sign = 0 then
        sign := -1;
    end
    else if (decimals1.Count < i) and (decimals2.Count < i) then
    begin
      comparing := False;
    end;

    // we continue comparing
    if comparing then
    begin
      if stringcomparison then
      begin
        sign := getCompareSignStrings(decimals1[i - 1], decimals2[i - 1]);
      end

      else
      begin
        if (containsDigitsOnly(decimals1[i - 1]) and
          containsDigitsOnly(decimals2[i - 1])) then
        begin
          if leadingZero(decimals1[i - 1], decimals2[i - 1]) then
          begin
            if tryDouble(doubleValue1, doubleValue2, decimals1[i - 1],
              decimals2[i - 1]) then
              sign := getCompareSignDouble(doubleValue1, doubleValue2);
          end
          else
          begin
            if tryInteger(number1, number2, decimals1[i - 1], decimals2[i - 1]) then
              sign := getCompareSign(number1, number2);
          end;
        end
        else // at least one string with non numbers
        begin
          // extract numbers and non numbers compare
          // hopefully non numbers will only be at the end ;-)
          // otherwise comparison will be weird
          string1 := extractNumbers(decimals1[i - 1]);
          string2 := extractNumbers(decimals2[i - 1]);
          if leadingZero(string1, string2) then
          begin
            if tryDouble(doubleValue1, doubleValue2, string1, string2) then
              sign := getCompareSignDouble(doubleValue1, doubleValue2);
          end
          else
          begin
            if tryInteger(number1, number2, string1, string2) then
              sign := getCompareSign(number1, number2);
          end;
          if (sign = 0) then // check only if numbers are equal
          begin
            string1 := extractNonNumbers(decimals1[i - 1]);
            string2 := extractNonNumbers(decimals2[i - 1]);
            sign := getCompareSignStrings(string1, string2);
          end;
        end;
      end;

      if sign <> 0 then
        comparing := False;
    end;

    Inc(i);
  end;

end;



function CompareDotSeparatedNumbers(s1, s2, s3: string): boolean;
var
  BooleanResult: boolean;
  intresult: integer;
  sx: string;
begin

  if getDecimalCompareSign(s1, s3, intresult, sx, False) then
  begin
    if s2 = '<' then
    begin
      case intresult of
        -1: BooleanResult := True;   // s1 < s3
        0: BooleanResult := False;   // s1 = s3
        1: BooleanResult := False;   // s1 > s3
      end;
    end
    else if (s2 = '<=') or (s2 = '=<') then
    begin
      case intresult of
        -1: BooleanResult := True;   // s1 < s3
        0: BooleanResult := True;   // s1 = s3
        1: BooleanResult := False;   // s1 > s3
      end;
    end
    else if s2 = '=' then
    begin
      case intresult of
        -1: BooleanResult := False;   // s1 < s3
        0: BooleanResult := True;   // s1 = s3
        1: BooleanResult := False;   // s1 > s3
      end;
    end
    else if (s2 = '>=') or (s2 = '=>') then
    begin
      case intresult of
        -1: BooleanResult := False;   // s1 < s3
        0: BooleanResult := True;   // s1 = s3
        1: BooleanResult := True;   // s1 > s3
      end;
    end
    else if s2 = '>' then
    begin
      case intresult of
        -1: BooleanResult := False;   // s1 < s3
        0: BooleanResult := False;   // s1 = s3
        1: BooleanResult := True;   // s1 > s3
      end;
    end;
  end;
  Result := BooleanResult;
end;


end.
