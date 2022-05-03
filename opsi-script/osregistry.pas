unit osregistry;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  registry,
  Windows,
  strings,
  LConvEncoding,
  osparserhelper,
  ostxstringlist,
  LAZUTF8;

const
  BytesarrayLength = 5000;
  PATHSEPARATOR = PathDelim;
  KEY_WOW64_64KEY = $0100;
  KEY_WOW64_32KEY = $0200;

type
  TuibRegDataType = (trdUnknown, trdDefaultString, trdString,
    trdExpandString, trdInteger,
    trdBinary, trdMultiString);

  {$IFDEF WINDOWS}
  TuibRegistry = class(TObject)
  private
    mykey: HKEY;
    keyopened: boolean;
    FBaseindentlevel: integer;
    force64: boolean;
    KeyOpenMode: cardinal;

  public
    constructor Create; overload;
    constructor Create(myforce64: boolean); overload;
    constructor Create(myforce64: boolean; ReadOnly: boolean); overload;
    destructor Destroy; override;

    function OpenExistingKey(const Key0, Key: string): boolean;
    function OpenKey(const Key0, Key: string): boolean;
    function CloseKey: boolean;
    function FlushKey: boolean;

    function GetAllSubkeys(const key0: string): TStringList;
    function GetAllVars: TStringList;

    function ReadEntry(const Name: string; var regType: TuibRegDataType;
      var Value: string): boolean;
    procedure WriteEntry(const Name: string; regType: TuibRegDataType;
      const Value: string);

    function ReadString(const Name: string): string;
    function ReadInteger(const Name: string): integer;
    procedure SupplementItems(Separator: char; const Name, Supplement: string);
    function RestoreFromFile(Filepath: string): boolean;

    function DeleteEntry(const Name: string): boolean;
    function DeleteSimpleSubKey(Key0, Key, subkey: string): boolean;
    function DeleteSubKey(const Key0, Key, subkey: string): boolean;
    function DeleteKey(const Key0, Key: string): boolean;
    function ReadBinary(Name: string; var Buffer; BufSize: integer): integer;

    //function GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    //procedure GetKeyNames(Strings: TStrings);


    function SetSecurity(TemplateKeyName, KeyName: string): boolean;

    property BaseIndentLevel: integer read FBaseIndentLevel write FBaseIndentLevel;
    property myKeyOpenMode: cardinal read KeyOpenMode write KeyOpenMode;
    property myforce64: boolean read force64 write force64;
  end;

{$ENDIF}

{$IFDEF WINDOWS}
function StrToBinary(const s: PChar; var lpdata: Pointer; var cbdata: DWord): boolean;
function GetHKEY(key: string; var outkey: HKEY): boolean;
function GetRegistrystringvalue(aktkey: string; ValueName: string;
  noredirect: boolean): string;
function GetRegistryKeyList(aktkey: string; noredirect: boolean): TStringList;
function GetRegistryVarList(aktkey: string; noredirect: boolean): TStringList;
function GetRegistryVarMap(aktkey: string; noredirect: boolean): TStringList;
function RegKeyExists(const regkey : string; noredirect: boolean): boolean;
function RegVarExists(const regkey : string; ValueName: string;
                       noredirect: boolean) : boolean;
{$ENDIF}



implementation

uses
  osfunc,
  oslog;

var
  //Ident: string = '';
  //Value: string = '';
  //LogSCommand: string = '';
  LogS: string = '';
//Logged: boolean;

function countChar(c: char; s: string): cardinal;
var
  i: cardinal = 0;
begin

  Result := 0;
  if s <> '' then
    // pascal string is 1 based (do 3.2019)
    for i := 1 to length(s)  do
      if s[i] = c then
        Inc(Result);

end;




function MakeMultiSz(const Text: string; var bytearray: TByteArray;
  const maxdatalength: DWord; var lpdata: Pointer; var cbdata: DWord;
  var errortext: string): boolean;
  //text wird aufgefasst als Stringliste, Strings getrennt mit #10
  //text muss enden mit doppeltem #10

var
  i: DWord = 0;
  //p: Pointer;
  p1, p2: Pointer;

begin
  //setlength (bytearray, length (text) + 2);
  // dynamische Arrays funktionieren nicht
  if length(Text) > maxdatalength then
  begin
    Result := False;
    errortext := 'MultiSz field too long';
  end
  else
  begin
    try
      p1 := @Text;
      p2 := @bytearray;
      cbdata := length(Text);
      for i := 1 to length(Text) do
        if Text[i] = #10 then
          bytearray[i - 1] := 0
        else
          bytearray[i - 1] := Ord(Text[i]);
      lpdata := p2;
      Result := True;
    except
      Result := False;
    end;
  end;
end;


(*
todo implement for widestring
function MakeMultiSz(const Text: widestring; var bytearray: TByteArray;
  const maxdatalength: DWord; var lpdata: Pointer; var cbdata: DWord;
  var errortext: string): boolean;
  //text wird aufgefasst als Stringliste, Strings getrennt mit #10
  //text muss enden mit doppeltem #10

var
  i: DWord = 0;
  //p: Pointer;
  p1, p2: Pointer;

begin
  //setlength (bytearray, length (text) + 2);
  // dynamic Arrays do not work
  if length(Text) * sizeOf(wchar) > maxdatalength then
  begin
    Result := False;
    errortext := 'MultiSz field too long';
  end
  else
  begin
    try
      p1 := @Text;
      p2 := @bytearray;
      cbdata := length(Text)  * sizeOf(wchar);
      for i := 1 to length(Text) do
        if Text[i] = #10 then
          bytearray[i - 1] := 0
        else
          bytearray[i - 1] := Ord(Text[i]);
      lpdata := p2;
      Result := True;
    except
      Result := False;
    end;
  end;
end;
*)

procedure MultiSzToText(const lpdata: Pointer; const cbdata: DWord; var Text: string);
//text wird aufgefasst als Stringliste, Strings zu trennen mit #13#10
var
  p: Pointer;
  len: DWord = 0;
  total: DWord = 0;
begin
  p := lpdata;
  // list.add (PChar (p));

  Text := PChar(p);
  len := StrLen(PChar(p));
  Text := Text + #13#10;
  total := len + 1;

  while (total < cbdata) do
  begin
    p := Pointer((DWord(p) + len + 1));
    Text := Text + PChar(p) + #13#10;
    len := StrLen(PChar(p));
    total := total + len + 1;
    ;
  end;
end;


function BinaryToStr(const lpdata: Pointer; const cbdata: DWord): string;
        (* erzeugt aus binaeren Daten einen String der Darstellung, wie er in der Registry
           verwendet wird *)
var
  i: dword = 0;
  binVal: byte = 0;
  Code: byte = 0;

begin
  i := 1;
  Result := '';


  while i <= cbdata do
  begin
    code := Ord(string(lpdata) [i]);

    binVal := Code div 16;

    if binVal < 10 then
      binVal := 48 + binVal
    else
      binVal := 97 + (binVal - 10);

    Result := Result + chr(binVal);

    binVal := Code mod 16;

    if binVal < 10 then
      binVal := 48 + binVal
    else
      binVal := 97 + (binVal - 10);

    Result := Result + chr(binVal);

    Inc(i);

    if i <= cbdata then
      Result := Result + ' ';
  end;

end;


function StrToBinary(const s: PChar; var lpdata: Pointer; var cbdata: DWord): boolean;
        (* erzeugt die binaeren Daten aus ihrer Stringdarstellung, wie sie in der Registry
           verwendet wird;
           Vorsicht: s^ wird geaendert und als Speicher fuer das Ergebnis verwendet,
           lpdata zeigt am Ende auf den Speicherbereich
           *)

var
  lenS: DWord = 0;
  i, j: integer;
  ErrorOccured: boolean;
  InDecoSpace, Pos10: boolean;
  Value10, Value: integer;

  s1, sResult: string;
  pResult: Pointer;

begin
  lpdata := nil;
  Result := True;

  lenS := StrLen(PChar(s));
  SetLength(sResult, lenS);


  s1 := UpperCase(string(PChar(s)));

  i := 1;
  j := 0;
  cbdata := 0;
  Pos10 := True;
  ErrorOccured := False;
  while (i <= lenS) and not ErrorOccured do
  begin
    InDecoSpace := False;
    if s1[i] in ['0'..'9'] then
      Value := Ord(s1[i]) - Ord('0')
    else if s1[i] in ['A'..'F'] then
      Value := Ord(s1[i]) - Ord('A') + 10
    else if s1[i] in [' ', #9, ','] then
      InDecoSpace := True
    else
      ErrorOccured := True;
    Inc(i);

    if not InDecoSpace then
    begin
      if Pos10 then
      begin
        Value10 := Value;
        Pos10 := False;
      end
      else
      begin
        Inc(j);
        cbdata := j;
        sResult[j] := chr(Value10 * 16 + Value);
        Pos10 := True;
      end;
    end;

  end;

  if ErrorOccured then
  begin
    cbdata := 0;
    Result := False;
  end
  else
  begin
    pResult := Pointer(sResult);
    System.Move(pResult^, s^, cbdata);

    lpdata := s;
    (* lpdata muss auf global verfuegbaren Speicher zeigen *);
  end;
end;

{$IFDEF WINDOWS}

function GetRegistrystringvalue(aktkey: string; ValueName: string;
  noredirect: boolean): string;
var
  StringResult: string = '';
  Regist: Tuibregistry;
  regType: TuibRegDataType;
  basekey, relkey, winencodekey, winencodevaluename: string;
begin
  Result := '';

  basekey := copy(aktkey, 0, Pos('\', aktkey) - 1);
  LogDatei.log('basekey : ' + basekey, LLdebug3);
  relkey := copy(aktkey, Pos('\', aktkey) + 1, length(aktkey));
  LogDatei.log('relkey : ' + relkey, LLdebug3);

  Regist := Tuibregistry.Create(noredirect, True);
  if Regist.OpenExistingKey(basekey, relkey) then
  begin
    try
      StringResult := Regist.ReadString(ValueName);
    except
      try
        Regist.ReadEntry(ValueName, RegType, StringResult);
      except
        on e: Exception do
        begin
          StringResult := '';
          LogDatei.log('Error: GetRegistryStringValue ' + e.message, LLError);
        end;
      end;
    end;
    Regist.CloseKey;
    Result := StringResult;
  end;
  Regist.Free;
end;


function GetRegistryKeyList(aktkey: string; noredirect: boolean): TStringList;
var
  basekey, relkey, winencodekey: string;
  Regist: Tregistry;
  //regType: TuibRegDataType;
  KeyOpenMode: longword;
  rkey: HKey;
  resultlist: TStringList;
  i: integer;
begin
  winencodekey := UTF8ToWinCP(aktkey);
  Result := TStringList.Create;
  resultlist := TStringList.Create;
  basekey := copy(winencodekey, 0, Pos('\', winencodekey) - 1);
  LogDatei.log('basekey : ' + basekey, LLdebug3);
  relkey := copy(winencodekey, Pos('\', aktkey) + 1, length(winencodekey));
  LogDatei.log('relkey : ' + relkey, LLdebug3);
  if noredirect then
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_64KEY;
    LogDatei.log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_32KEY;
    LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
  end;
  try
    try
      //LogDatei.log('finished1', LLdebug);
      Regist := Tregistry.Create(KeyOpenMode);
      //LogDatei.log('finished2  :'+copy(aktkey, 0, Pos('\', aktkey) - 1), LLdebug);
      if GetHKey(basekey, rkey) then
      begin
        regist.RootKey := rkey;
        if regist.KeyExists(copy(winencodekey, Pos('\', winencodekey) +
          1, length(winencodekey))) then
        begin
          regist.OpenKey(relkey, False);
          //LogDatei.log('finished3', LLdebug);
          regist.GetKeyNames(resultlist);
          //LogDatei.log('finished4', LLdebug);
          for i := 0 to resultlist.Count - 1 do
            Result.Add(WinCPToUTF8(resultlist.Strings[i]));
          //LogDatei.log('finished5', LLdebug);
        end;
        regist.CloseKey;
      end
      else
        LogDatei.log('Could not get root key from : ' + basekey, LLError);
    except
      on e: Exception do
      begin
        LogDatei.log('Error: ' + e.message, LLError);
      end;
    end
  finally
    regist.Free;
    resultlist.Free;
    //LogDatei.log('finished6', LLdebug);
  end;
end;


function GetRegistryVarList(aktkey: string; noredirect: boolean): TStringList;
var
  basekey, relkey, winencodekey: string;
  Regist: Tregistry;
  KeyOpenMode: longword;
  rkey: HKey;
  resultlist: TStringList;
  i: integer;
begin
  winencodekey := UTF8ToWinCP(aktkey);
  Result := TStringList.Create;
  resultlist := TStringList.Create;
  basekey := copy(winencodekey, 0, Pos('\', winencodekey) - 1);
  LogDatei.log('basekey : ' + basekey, LLdebug3);
  relkey := copy(winencodekey, Pos('\', winencodekey) + 1, length(winencodekey));
  LogDatei.log('relkey : ' + relkey, LLdebug3);
  if noredirect then
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_64KEY;
    LogDatei.log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_32KEY;
    LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
  end;
  try
    try
      Regist := Tregistry.Create(KeyOpenMode);
      if GetHKey(basekey, rkey) then
      begin
        regist.RootKey := rkey;
        if regist.KeyExists(copy(winencodekey, Pos('\', winencodekey) +
          1, length(winencodekey))) then
        begin
          regist.OpenKey(relkey, False);
          regist.GetValueNames(resultlist);
          for i := 0 to resultlist.Count - 1 do
            Result.Add(WinCPToUTF8(resultlist.Strings[i]));
        end;
        regist.CloseKey;
      end
      else
        LogDatei.log('Could not get root key from : ' + basekey, LLError);
    except
      on e: Exception do
      begin
        LogDatei.log('Error: ' + e.message, LLError);
      end;
    end
  finally
    regist.Free;
    resultlist.Free;
    //LogDatei.log('finished6', LLdebug);
  end;
end;

function GetRegistryVarMap(aktkey: string; noredirect: boolean): TStringList;
var
  basekey, relkey, myvalue, winencodekey: string;
  Regist: Tregistry;
  KeyOpenMode: longword;
  rkey: HKey;
  resultlist: TStringList;
  i: integer;
  dataType: TRegDataType;
begin
  winencodekey := UTF8ToWinCP(aktkey);
  Result := TStringList.Create;
  resultlist := TStringList.Create;
  basekey := copy(winencodekey, 0, Pos('\', winencodekey) - 1);
  LogDatei.log('basekey : ' + basekey, LLdebug3);
  relkey := copy(winencodekey, Pos('\', winencodekey) + 1, length(winencodekey));
  LogDatei.log('relkey : ' + relkey, LLdebug3);
  if noredirect then
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_64KEY;
    LogDatei.log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_32KEY;
    LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
  end;
  try
    try
      Regist := Tregistry.Create(KeyOpenMode);
      if GetHKey(basekey, rkey) then
      begin
        regist.RootKey := rkey;
        if regist.KeyExists(copy(winencodekey, Pos('\', winencodekey) +
          1, length(winencodekey))) then
        begin
          regist.OpenKey(relkey, False);
          regist.GetValueNames(resultlist);
          for i := 0 to resultlist.Count - 1 do
          begin
            // myvalue gets utf8 encoding
            myvalue := GetRegistrystringvalue(aktkey, resultlist.Strings[i],
              noredirect);
              (*
              regist.ReadString(resultlist.Strings[i]);
              dataType := regist.GetDataType(resultlist.Strings[i]);
              case dataType of
                rdString : myvalue := regist.ReadString(resultlist.Strings[i]);
                rdExpandString : myvalue := regist.ReadString(resultlist.Strings[i]);
                rdInteger : myvalue := IntToStr(regist.ReadInteger(resultlist.Strings[i]));
                rdBinary :  myvalue := regist.ReadString(resultlist.Strings[i]);
                rdUnknown : myvalue := 'Error : unknown data type';
              end;
              *)
            Result.Add(WinCPToUTF8(resultlist.Strings[i]) + '=' + myvalue);
          end;
        end;
        regist.CloseKey;
      end
      else
        LogDatei.log('Could not get root key from : ' + basekey, LLError);
    except
      on e: Exception do
      begin
        LogDatei.log('Error: ' + e.message, LLError);
      end;
    end
  finally
    regist.Free;
    resultlist.Free;
    //LogDatei.log('finished6', LLdebug);
  end;
end;

function RegKeyExists(const regkey : string; noredirect: boolean) : boolean;
var
  basekey, relkey, winencodekey: string;
  Regist: Tregistry;
  KeyOpenMode: longword;
  rkey: HKey;
  i: integer;
  dataType: TRegDataType;
begin
  winencodekey := UTF8ToWinCP(regkey);
  Result := false;
  basekey := copy(winencodekey, 0, Pos('\', winencodekey) - 1);
  LogDatei.log_prog('basekey : ' + basekey, LLdebug3);
  relkey := copy(winencodekey, Pos('\', winencodekey) + 1, length(winencodekey));
  LogDatei.log_prog('relkey : ' + relkey, LLdebug3);
  if noredirect then
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_64KEY;
    LogDatei.log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_32KEY;
    LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
  end;
  try
    try
      Regist := Tregistry.Create(KeyOpenMode);
      if GetHKey(basekey, rkey) then
      begin
        regist.RootKey := rkey;
        if regist.KeyExists(relkey) then result := true;
      end
      else
        LogDatei.log('Could not get root key from : ' + basekey, LLError);
    except
      on e: Exception do
      begin
        LogDatei.log('Error: ' + e.message, LLError);
      end;
    end
  finally
    regist.Free;
  end;
end;

function RegVarExists(const regkey : string; ValueName: string;
                       noredirect: boolean) : boolean;
var
  basekey, relkey, winencodekey: string;
  Regist: Tregistry;
  KeyOpenMode: longword;
  rkey: HKey;
  i: integer;
  dataType: TRegDataType;
begin
  winencodekey := UTF8ToWinCP(regkey);
  Result := false;
  basekey := copy(winencodekey, 0, Pos('\', winencodekey) - 1);
  LogDatei.log_prog('basekey : ' + basekey, LLdebug3);
  relkey := copy(winencodekey, Pos('\', winencodekey) + 1, length(winencodekey));
  LogDatei.log_prog('relkey : ' + relkey, LLdebug3);
  if noredirect then
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_64KEY;
    LogDatei.log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    KeyOpenMode := KEY_READ or KEY_WOW64_32KEY;
    LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
  end;
  try
    try
      Regist := Tregistry.Create(KeyOpenMode);
      if GetHKey(basekey, rkey) then
      begin
        regist.RootKey := rkey;
        if regist.KeyExists(relkey) then
        begin
          regist.OpenKey(relkey, False);
          result := regist.ValueExists(ValueName);
          regist.CloseKey;
        end;
      end
      else
        LogDatei.log('Could not get root key from : ' + basekey, LLError);
    except
      on e: Exception do
      begin
        LogDatei.log('Error: ' + e.message, LLError);
      end;
    end
  finally
    regist.Free;
  end;
end;


{$ENDIF WINDOWS}



(* TuibRegistry *)

{$IFDEF WINDOWS}
function GetHKEY(key: string; var outkey: HKEY): boolean;
begin
  Result := True;
  if (uppercase(key) = 'HKEY_CLASSES_ROOT') or (uppercase(key) = 'HKCR') then
    outkey := HKEY_CLASSES_ROOT
  else if (uppercase(key) = 'HKEY_CURRENT_USER') or (uppercase(key) = 'HKCU') then
    outkey := HKEY_CURRENT_USER
  else if (uppercase(key) = 'HKEY_LOCAL_MACHINE') or (uppercase(key) = 'HKLM') then
    outkey := HKEY_LOCAL_MACHINE
  else if (uppercase(key) = 'HKEY_USERS') or (uppercase(key) = 'HKU') then
    outkey := HKEY_USERS
  else if (uppercase(key) = 'HKEY_CURRENT_CONFIG') or (uppercase(key) = 'HKCC') then
    outkey := HKEY_CURRENT_CONFIG
  else if uppercase(key) = 'HKEY_DYN_DATA' then
    outkey := HKEY_DYN_DATA
  else
    Result := False;
end;


constructor TuibRegistry.Create;
begin
  inherited Create;
  keyopened := False;
  baseindentlevel := LogDatei.LogLevel;
  force64 := False;
  KeyOpenMode := KEY_ALL_ACCESS;
  LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
end;

constructor TuibRegistry.Create(myforce64: boolean);
begin
  inherited Create;
  keyopened := False;
  baseindentlevel := LogDatei.LogLevel;
  force64 := myforce64;
  if force64 then
  begin
    KeyOpenMode := KEY_ALL_ACCESS or KEY_WOW64_64KEY;
    LogDatei.log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    KeyOpenMode := KEY_ALL_ACCESS or KEY_WOW64_32KEY;
    LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
  end;
end;


constructor TuibRegistry.Create(myforce64: boolean; ReadOnly: boolean);
begin
  inherited Create;
  keyopened := False;
  baseindentlevel := LogDatei.LogLevel;
  if ReadOnly then
  begin
    KeyOpenMode := KEY_READ;
    LogDatei.log('Registry started readonly', LLdebug);
  end
  else
  begin
    KeyOpenMode := KEY_ALL_ACCESS;
    LogDatei.log('Registry started KEY_ALL_ACCESS', LLdebug2);
  end;
  force64 := myforce64;
  if force64 then
  begin
    KeyOpenMode := KeyOpenMode or KEY_WOW64_64KEY;
    LogDatei.log('Registry started without redirection (64 Bit)', LLdebug);
  end
  else
  begin
    KeyOpenMode := KeyOpenMode or KEY_WOW64_32KEY;
    LogDatei.log('Registry started with redirection (32 Bit)', LLdebug);
  end;
end;


destructor TuibRegistry.Destroy;
begin
  if keyopened then
    CloseKey;
  inherited Destroy;
end;


function TuibRegistry.OpenExistingKey(const Key0, Key: string): boolean;
var
  rkey: HKEY;
  //dwdisposition: dword;
  regresult: integer = 0;
  enckey: string;

begin
  Result := False;
  enckey := UTF8ToWinCP(key);
  if not GetHKEY(Key0, rkey) then
  begin
    LogS := 'Error: ' + Key0 + ' not accepted as registry root key';
    LogDatei.log(LogS, LLNotice);
  end
  else
  begin
    if keyopened then
      CloseKey;
    regresult :=
      RegOpenKeyEx(rkey, PChar(enckey), 0, KeyOpenMode, mykey);

    if regresult <> ERROR_SUCCESS then
    begin
      LogS := 'Info: Registry key ' + '[' + key0 + '\' + key + '] ' +
        '  could not be opened by RegOpenKeyEx, ' + ' Errorno ' +
        IntToStr(regresult) + ' "' + RemoveLineBreaks(SysErrorMessage(regresult)) + '"';
      LogDatei.log(LogS, LLInfo);
      LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;

    end
    else
    begin
      keyopened := True;

      LogS := 'opened';

      LogS := 'Registry key ' + '[' + key0 + '\' + key + ']  ' + LogS;
      LogDatei.log(LogS, LLInfo);

      Result := True;
    end;

  end;

  //LogDatei.LogSIndentLevel := baseindentlevel + countChar ('\', key);
  //LogDatei.log ( 'setze LogSIndentLevel auf ' + inttostr(LogDatei.LogSIndentLevel), LLInfo);
end;


function TuibRegistry.OpenKey(const Key0, Key: string): boolean;
var
  rkey: HKEY;
  dwdisposition: dword = 0;
  regresult: integer = 0;
  enckey: string;
begin
  Result := False;
  enckey := UTF8ToWinCP(key);
  if not GetHKEY(Key0, rkey) then
  begin
    LogS := 'Error: ' + Key0 + ' not accepted as registry root key';
    LogDatei.log(LogS, LLNotice);
  end
  else
  begin
    if keyopened then
      CloseKey;
    regresult :=
      RegCreateKeyEx(rkey, PChar(enckey), 0, nil, reg_option_non_volatile,
      KeyOpenMode, nil, mykey, @dwDisposition);

    if regresult <> ERROR_SUCCESS then
    begin
      LogS := 'Registry key ' + '[' + key0 + '\' + key + '] ' +
        'could not be opened by RegCreateKeyEx, System Errorno: ' +
        IntToStr(regresult) + ' "' + RemoveLineBreaks(SysErrorMessage(regresult)) + '"';
      LogDatei.log(LogS, LLError);
      if (regresult = 87) and ((key0 = 'HKEY_USERS') or (key0 = 'HKEY_LOCAL_MACHINE')) then
      begin
        // See https://docs.microsoft.com/en-us/windows/win32/api/winreg/nf-winreg-regcreatekeya
        if Pos('\', key) > 0 then
          LogDatei.log('Hint: It is not allowed to create new subkeys directly under '
          + key0 + '. Please check that "' + key0 + '\' + Copy(key, 1, Pos('\', key)-1) + '" exists.', LLNotice)
        else
          LogDatei.log('Hint: It is not allowed to create new subkeys directly under '
          + key0 + ' !', LLWarning);
      end;
    end
    else
    begin
      keyopened := True;

      if dwDisposition = reg_created_new_key then
        LogS := 'created'
      else
        LogS := 'opened';

      LogS := 'Registry key ' + '[' + key0 + '\' + key + '] ' + LogS;
      LogDatei.log(LogS, LLInfo);

      Result := True;
    end;
  end;

  LogDatei.LogSIndentLevel := baseindentlevel + countChar('\', key);

end;

function TuibRegistry.GetAllSubkeys(const key0: string): TStringList;
var
  s: string = '';
  b: PChar;
  len: cardinal = 0;
  i: integer = 0;
  errorcode: longint = 0;
  subkeynumber: cardinal = 0;
  maxsubkeylength: cardinal = 0;
  //startindentlevel : Integer;
  //keyToOpen: string;

begin
  Result := TStringList.Create;
  keyopened := openExistingKey(key0, '');

  if keyopened then
  begin
    errorcode := RegQueryInfoKey(mykey, nil, nil, nil, @subkeynumber,
      @maxsubkeylength, nil, nil, nil, nil, nil, nil);
    LogDatei.log(' errorcode ' + IntToStr(errorcode) + ' "' +
      RemoveLineBreaks(SysErrorMessage(ErrorCode)) + '"', LLInfo);

    b := StrAlloc(maxsubkeylength + 1);
    for i := subkeynumber - 1 downto 0 do
    begin
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;
      len := maxsubkeylength + 1;
      errorcode := RegEnumKeyEx(mykey, i, b, len, nil, nil, nil, nil);
      LogDatei.log(' errorcode ' + IntToStr(errorcode) + ' "' +
        RemoveLineBreaks(SysErrorMessage(ErrorCode)) + '"', LLInfo);
      LogDatei.log('there ' + b, LLInfo);
      setString(s, b, len);
      LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
    end;
    Result.add(s);
    freemem(b);

  end;
end;

function TuibRegistry.GetAllVars: TStringList;
var
  s: string = '';
  valname: array[0..255] of char;
  //valname: unicodestring;
  //len: cardinal;
  i: integer = 0;
  //errorcode: longint;
  size, dataType: DWord;
  nameSize: cardinal = 0;
  Data: pbyte;
  //startindentlevel: integer;
  //keyToOpen: string;

begin
  Result := TStringList.Create;

  if keyopened then
  begin
    LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

    size := 0;
    nameSize := 255;
    i := 0;
    Data := nil;
    // first call to get the size
    while (RegEnumValue(mykey, i, PChar(ValName), nameSize, nil,
        @datatype, Data, @size) = Error_success) do
    begin
      getMem(Data, Size);
      nameSize := 255;
      // second call to get the data
      RegEnumValue(mykey, i, PChar(ValName), nameSize, nil, @datatype, Data, @size);

      setString(s, PChar(ValName), nameSize);
      LogDatei.log('entry:"' + s + '"', LLInfo);
      Result.add(WinCPToUTF8(s));
      FreeMem(Data);

      Data := nil;
      Size := 0;
      NameSize := 255;

      Inc(i);
    end;

  end;

  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel - 1;
end;




function TuibRegistry.DeleteSubKey(const Key0, Key, subkey: string): boolean;
var
  s: string = '';
  pBuf: array [0..MAX_PATH] of char;
  dwLen: DWord = 0;
  //len: cardinal;
  //i, j: integer;
  errorcode: longint = 0;
  subkeynumber: cardinal = 0;
  maxsubkeylength: cardinal = 0;
  startindentlevel: integer = 0;
  keyToOpen: string = '';
  //, keytoopenFordelete
  continue: boolean;
  //keys: TXStringList;
  enckey, encsubkey: string;

begin
  Result := False;
  enckey := UTF8ToWinCP(key);
  encsubkey := UTF8ToWinCP(subkey);
  startIndentLevel := LogDatei.LogSIndentLevel;
  LogDatei.LogSIndentLevel := LogDatei.LogSIndentLevel + 1;

  (* no encoding here becaus we are perhaps in a recusrsion
  if enckey = '' then
    keyToOpen := encsubkey
  else
    keyToOpen := enckey + '\' + encsubkey;
  *)
  if enckey = '' then
    keyToOpen := subkey
  else
    keyToOpen := key + '\' + subkey;


  // init loop
  subkeynumber := 0;
  if openExistingKey(key0, keyToOPen) then
    errorcode := RegQueryInfoKey(mykey, nil, nil, nil, @subkeynumber,
      @maxsubkeylength, nil, nil, nil, nil, nil, nil);

  continue := True;

  while continue and (subkeynumber > 0) do
  begin

    ZeroMemory(@pbuf, sizeOf(pBuf));
    dwLen := sizeOf(pBuf);

    errorcode := RegEnumKeyEx(mykey, subkeynumber - 1, pBuf, dwlen, nil, nil, nil, nil);
    if (*errorcode = Error_no_more_items*) not (errorcode = Error_success) then
      continue := False;
    //LogDatei.log (' errorcode ' + inttostr(errorcode) + ' "' + RemoveLineBreaks(SysErrorMessage (ErrorCode) + '"', LLInfo);
    //LogDatei.log ('there ' + b, LLInfo);

    if (continue) then
    begin
      setString(s, pBuf, dwlen);
      //recursion
      continue := DeleteSubkey(key0, keyToOpen, s);
    end;

    // loop condition
    subkeynumber := 0;
    if openExistingKey(key0, keyToOPen) then
      errorcode := RegQueryInfoKey(mykey, nil, nil, nil, @subkeynumber,
        @maxsubkeylength, nil, nil, nil, nil, nil, nil);
  end;

  LogDatei.log('we should try to delete ' + keytoopen + ' in ' + key0, LLdebug2);

  if OpenExistingKey(key0, '') then
  begin
    errorcode := RegDeleteKey(mykey, PChar(UTF8ToWinCP(keytoopen)));
    if errorcode = error_success then
    begin
      Result := True;
      LogDatei.log(keytoopen + ' deleted', LLInfo);
    end
    else
    if errorCode = 2 then
    begin
      LogS := 'Info: key "' + keytoopen + '" not found ';
      LogDatei.log(LogS, LLInfo);
    end
    else
    begin
      LogS := 'Error: subkey "' + subkey + '" could not be deleted.  Errorcode ' +
        IntToStr(errorcode) + '. Message "' +
        RemoveLineBreaks(SysErrorMessage(ErrorCode)) + '"';
      LogDatei.log(LogS, LLNotice);
    end;
    CloseKey;
  end;
  LogDatei.LogSIndentLevel := startIndentLevel;
end;



function TuibRegistry.DeleteSimpleSubKey(Key0, Key, subkey: string): boolean;
var
  //regresult: integer;
  //i: integer;
  errorcode: longint = 0;
  //startindentlevel : Integer;
  loglevel: integer = 0;
  enckey, encsubkey: string;

begin
  Result := True;
  enckey := UTF8ToWinCP(key);
  encsubkey := UTF8ToWinCP(subkey);
  //startindentlevel := LogDatei.LogSIndentLevel;

  loglevel := Logdatei.LogLevel;
  LogDatei.LogLevel := LLInfo; //don't log the opening of a key
  if not OpenExistingKey(Key0, key) then
  begin
    LogDatei.Loglevel := loglevel;
    Result := False;
  end
  else
  begin
    LogDatei.Loglevel := loglevel;

    errorcode := RegDeleteKey(mykey, PChar(encsubkey));

    if errorCode = Error_success then
    begin
      Result := True;
      LogS := 'deleted subkey "' + subkey + '"';
      LogDatei.log(LogS, LLInfo);
    end

    else if errorCode = 2 then
    begin
      LogS := 'Info: subkey "' + subkey + '" not found ';
      LogDatei.log(LogS, LLInfo);
    end
    else
    begin
      LogS := 'Error: subkey "' + subkey + '" could not be deleted.  Errorcode ' +
        IntToStr(errorcode) + '. Message "' +
        RemoveLineBreaks(SysErrorMessage(ErrorCode)) + '"';
      LogDatei.log(LogS, LLNotice);
    end;

    CloseKey;

  end;

  //LogDatei.LogSIndentLevel := startindentlevel;

end;




function TuibRegistry.DeleteKey(const Key0, Key: string): boolean;
var
  parentkey: string = '';
  subkeys: TXStringList;
  i: integer = 0;

begin
  subkeys := TXStringList.Create;
  stringsplit(key, '\', subkeys);

  if subkeys.Count = 0 then
  begin
    Result := False;
    LogDatei.log('No registry deleting on the upper level', LLInfo);
  end
  else
  if subkeys.Count = 1 then
  begin
    deleteSubKey(key0, '', subkeys[0]);
  end
  else
  begin
    parentkey := subkeys[0];
    for i := 1 to subkeys.Count - 2 do
    begin
      parentkey := parentkey + '\' + subkeys[i];
    end;

    deleteSubKey(key0, parentkey, subkeys[subkeys.Count - 1]);
  end;

end;


function TuibRegistry.CloseKey: boolean;
var
  regresult: integer = 0;
begin
  regresult := RegCloseKey(mykey);
  if not keyopened or (regresult = Error_Success) then
  begin
    Result := True;
    LogDatei.log('Key closed', LLInfo);
    keyopened := False;
  end
  else
  begin
    Result := False;
    LogDatei.log
    ('Warning: Key could not be closed. Error ' + IntToStr(Regresult) +
      ': "' + RemoveLineBreaks(SysErrorMessage(regresult)) + '"', LLWarning);
  end;
end;

function TuibRegistry.FlushKey: boolean;
var
  regresult: integer = 0;
begin
  regresult := RegFlushKey(mykey);
  if regresult = Error_Success then
  begin
    Result := True;
    LogDatei.log('Key flushed', LLInfo);
  end
  else
  begin
    Result := False;
    LogDatei.log
    ('Warning: Key could not be flushed. Error ' + IntToStr(regresult) +
      ': "' + RemoveLineBreaks(SysErrorMessage(regresult)) + '"', LLWarning);
  end;
end;


function dwTypeToTuibRegDataType(dwType: dword): TuibRegDataType;
var
  regType: TuibRegDataType;

begin
  regType := trdUnknown;
  case dwType of
    REG_BINARY: regType := trdBinary;
    REG_DWORD
      (* ist REG_DWORD_Little_Endian *): regType := trdInteger;
    REG_DWORD_BIG_ENDIAN: regType := trdUnknown;
    REG_EXPAND_SZ: regType := trdExpandString;
    REG_LINK: regType := trdUnknown;
    REG_MULTI_SZ: regType := trdMultiString;
    REG_NONE: regType := trdUnknown;
    REG_RESOURCE_LIST: regType := trdUnknown;
    REG_SZ: regType := trdString;
  end;

  Result := regType;

end;


function tUibRegDataTypeToString(regType: TuibRegDataType): string;

begin
  case regType of

    trdUnknown: Result := 'Unknown';
    trdString: Result := 'String';
    trdDefaultString: Result := 'String';
    trdExpandString: Result := 'ExpandString';
    trdInteger: Result := 'Integer';
    trdBinary: Result := 'Binary';
    trdMultiString: Result := 'MultiString';

    else
      Result := '';

  end;

end;


function TuibRegistry.ReadEntry
  (const Name: string; var regType: TuibRegDataType; var Value: string): boolean;

var
  regresult: integer = 0;
  dwDataSize: Dword = 0;
  lpData_OldValue: pointer;
  ecW: PWChar;
  dwType: dword = 0;
  encname, encvalue: string;
  encnameW: WideString;
  //encvalueW: WideString;

begin
  Value := '';
  Result := True;
  if Name = '' then
  begin
    // do not try to encode empty strings
    encname := '';
    encnameW := '';
  end
  else
  begin
    encname := UTF8ToWinCP(Name);
    encnameW := UTF8ToUTF16(Name);
  end;

  regresult := RegQueryValueExW(mykey, PWChar(encnameW), nil, @dwType, nil, @dwDataSize);
  if regresult = Error_Success then
  begin
    LogDatei.log('Found Name: ' + Name + ' (encoded: ' + encname +
      ') bytes:' + IntToStr(dwDataSize), LLDebug3);
    GetMem(lpData_OldValue, dwDataSize);

    regresult := RegQueryValueExW(mykey, PWChar(encnameW), nil, @dwType,
      lpData_OldValue, @dwDataSize);
    if regresult <> Error_Success then
    begin
      Result := False;
      LogDatei.log('Warning: Querying existing value from "' +
        Name + '". Problem: ' + IntToStr(regresult) + ' "' +
        RemoveLineBreaks(SysErrorMessage(regresult)) + '"', LLWarning);
    end
    else
      LogDatei.log('successful querying for Value from Name: ' +
        Name + ' (encoded: ' + encname + ') bytes:' + IntToStr(
        dwDataSize), LLDebug3);
  end
  else
  begin
    Result := False;
    LogDatei.log('Variable "' + Name + '"  not found. Code: ' + IntToStr(regresult)
      , LLInfo);
  end;

  if not Result then
    exit;

  regType := dwTypeToTuibRegDataType(dwType);

  if regType = trdUnknown then
  begin
    LogDatei.log('Warning: The type of the variable "' + Name +
      '" cannot be interpreted',
      LLWarning);
    regType := trdBinary;
  end;

  case regType of
    trdString, trdExpandString:
    begin
      regresult := RegQueryValueExW(mykey, PWCHAR(encnameW), nil,
        @dwType, nil, @dwDataSize);
      if regresult = Error_Success then
      begin
        LogDatei.log('Found Name: ' + Name + ' (encoded: ' +
          encname + ') bytes:' + IntToStr(dwDataSize), LLDebug3);
        if dwDataSize = 0 then
        begin
          // no bytes - no value
          Value := '';
          //encvalueW := '';
          encvalue := '';
          LogDatei.log('Found empty Value:  bytes:' + IntToStr(dwDataSize), LLDebug3);
        end
        else
        begin
          GetMem(lpData_OldValue, dwDataSize + 10);
          FillChar(lpData_OldValue^, dwDataSize + 8, 0);
          GetMem(ecW, dwDataSize + 10);
          FillChar(ecW^, dwDataSize + 8, 0);
          regresult := RegQueryValueExW(mykey, PWChar(encnameW), nil,
            @dwType, lpData_OldValue, @dwDataSize);
          if regresult <> Error_Success then
          begin
            Result := False;
            LogDatei.log('Warning: Querying existing value from "' +
              Name + '". Problem: ' + IntToStr(regresult), LLWarning);
          end
          else
          begin
            LogDatei.log('successful querying for Value from Name: ' +
              Name + ' (encoded: ' + encname + ') bytes:' + IntToStr(
              dwDataSize), LLDebug3);
            StrLCopy(ecW, lpData_OldValue, dwDataSize);
            Value := UTF16ToUTF8(ecW);
            LogDatei.log('Found Value0: ' + Value + ')  bytes:' +
              IntToStr(dwDataSize), LLDebug3);

          end;
        end;
      end
      else
      begin
        Result := False;
        LogDatei.log('Variable "' + Name + ' (encoded: ' + UTF16ToUTF8(encnameW) +
          '"  not found. Code: ' + IntToStr(regresult)
          , LLWarning);
      end;
    end;

    trdMultiString:
    begin
      // todo widestring implementation
      regresult := RegQueryValueEx(mykey, PChar(encname), nil,
        @dwType, nil, @dwDataSize);
      GetMem(lpData_OldValue, dwDataSize + 10);
      FillChar(lpData_OldValue^, dwDataSize + 8, 0);
      regresult := RegQueryValueEx(mykey, PChar(encname), nil,
        @dwType, lpData_OldValue, @dwDataSize);
      MultiSzToText(lpData_OldValue, dwDataSize, encvalue);
      if dwDataSize = 0 then
      begin
        // no bytes - no value
        Value := '';
        encvalue := '';
        //encvalueW := '';
        LogDatei.log('Found Empty Value:  bytes:' + IntToStr(dwDataSize), LLDebug3);
      end
      else
        Value := WinCPToUTF8(encvalue);
      LogDatei.log('Found Value: ' + Value + ' (encoded: ' + encvalue + ')', LLDebug3);
    end;


    trdBinary:
    begin
      // todo widestring implementation
      regresult := RegQueryValueEx(mykey, PChar(encname), nil,
        @dwType, nil, @dwDataSize);
      GetMem(lpData_OldValue, dwDataSize + 10);
      FillChar(lpData_OldValue^, dwDataSize + 8, 0);
      regresult := RegQueryValueEx(mykey, PChar(encname), nil,
        @dwType, lpData_OldValue, @dwDataSize);
      Value := BinaryToStr(lpData_OldValue, dwDataSize);

    end;

    trdInteger: Value := IntToStr(DWord(lpData_OldValue^));
  end;

end;



function TuibRegistry.ReadBinary(Name: string; var Buffer; BufSize: integer): integer;
  //http://www.delphipraxis.net/132107-registry-binary-lesen-und-schreiben.html
var
  regresult: integer = 0;
  //dwType: dword;
  DataType: integer = 0;
  encname: string;

begin
  Result := -1;
  DataType := REG_NONE;
  encname := UTF8ToWinCP(Name);

  try
    // return the size of the buffer required
    if BufSize = 0 then
    begin
      // get data type and buffer size
      regresult := RegQueryValueEx(mykey, PChar(encname), nil, @DataType, nil, @BufSize);
      if regresult = ERROR_SUCCESS then
      begin
        Result := BufSize;
      end
      else
      begin
        LogDatei.log('Variable "' + Name + '"  not found. Code: ' +
          IntToStr(regresult), LLWarning);
      end;
    end
    else
    begin
      // get data
      if RegQueryValueEx(mykey, PChar(encname), nil, @DataType,
        PByte(Buffer), @BufSize) = ERROR_SUCCESS then
        Result := BufSize
      else
      begin
        Result := -1;
        LogDatei.log('Warning: Querying existing value from "' +
          Name + '". Problem: ' + IntToStr(regresult), LLWarning);
      end;
    end;
  except
    on e: Exception do
    begin
      Result := -1;
      LogDatei.log('In ReadBinary Variable "' + Name + '" exceptin. Code: ' +
        e.message, LLError);
    end;
  end;
end;


procedure TuibRegistry.WriteEntry
  (const Name: string; regType: TuibRegDataType; const Value: string);
var
  regresult: integer = 0;

  dwDataSize: Dword = 0;
  lpData_OldValue: Pointer;

  dwType: dword = 0;
  iValue: DWord = 0;

  datafound: boolean;
  oldValue: string = '';
  newValue: string = '';
  compareValue: string = '';
  //dataS: PChar;

  oldregType: TuibRegDataType;

  s: PChar;
  lpdata: Pointer;
  cbdata: dword = 0;

  bytesarray: TByteArray;
  errorOccured: boolean;
  errortext: string = '';
  encname, encvalue: string;
  encnameW, encvalueW: WideString;
  ecW: PWChar;

begin
  oldValue := '';
  datafound := True;
  errortext := '';
  if Name = '' then
  begin
    // do not try to encode empty strings
    encname := '';
    encnameW := '';
  end
  else
  begin
    encname := UTF8ToWinCP(Name);
    encnameW := UTF8ToUTF16(Name);
  end;
  if Value = '' then
  begin
    // do not try to encode empty strings
    encvalue := '';
    encvalueW := '';
  end
  else
  begin
    encvalue := UTF8ToWinCP(Value);
    encvalueW := UTF8ToUTF16(Value);
  end;

  dwType := 0;
  regresult := RegQueryValueExW(mykey, PWChar(encnameW), nil, @dwType, nil, @dwDataSize);
  if regresult = Error_Success then
  begin
    LogDatei.log('Found Name: ' + Name + ' (encoded: ' + encname +
      ') bytes:' + IntToStr(dwDataSize), LLDebug3);
    GetMem(lpData_OldValue, dwDataSize + 10);
    FillChar(lpData_OldValue^, dwDataSize + 8, 0);
    regresult := RegQueryValueExW(mykey, PWChar(encnameW), nil, @dwType,
      lpData_OldValue, @dwDataSize);
    if regresult <> Error_Success then
    begin
      datafound := False;
      LogDatei.log('Warning: Querying existing value from "' +
        Name + '". Problem: ' + IntToStr(regresult) + ' "' +
        RemoveLineBreaks(SysErrorMessage(regresult)) + '"', LLWarning);
    end
    else
      LogDatei.log('successful querying for Value from Name: ' +
        Name + ' (encoded: ' + encname + ') bytes:' + IntToStr(
        dwDataSize), LLDebug3);
  end
  else
    datafound := False;

  if regType = trdDefaultString then
  begin
    // if no datatype is given use string (or if there is a '%' expandstring
    if pos('%', Value) <> 0 then
      regType := trdExpandString
    else
      regType := trdString;
  end;

  oldRegType := dwTypeToTuibRegDataType(dwType);

  if datafound and (oldRegType <> regType) then
    LogDatei.log('Warning: Existing value has type ' +
      tUibRegDataTypeToString(oldRegType) +
      ', we are about to write value of type ' + tUibRegDataTypeToString(regType),
      LLWarning);

  errorOccured := False;
  // get the old value in the correct format
  if datafound then
  begin
    case oldRegType of
      trdString, trdExpandString:
      begin
        (*
        SetLength(oldValue, dwDataSize - 1);
        if dwDataSize > 1 then
          StrLCopy(PChar(oldValue), lpData_OldValue, dwDataSize - 1);
        *)
        if dwDataSize = 0 then
        begin
          // no bytes - no value
          oldValue := '';
          encvalue := '';
          LogDatei.log('Found empty oldvalue:  bytes:' + IntToStr(dwDataSize), LLDebug3);
        end
        else
        begin
          GetMem(ecW, dwDataSize + 10);
          FillChar(ecW^, dwDataSize + 8, 0);
          StrLCopy(ecW, lpData_OldValue, dwDataSize);
          oldValue := UTF16ToUTF8(ecW);
          LogDatei.log('Found oldValue: ' + oldValue + ')  bytes:' +
            IntToStr(dwDataSize), LLDebug3);
        end;
      end;
      trdMultiString:
      begin
        // todo widestring implementation
        regresult := RegQueryValueEx(mykey, PChar(encname), nil,
          @dwType, lpData_OldValue, @dwDataSize);
        MultiSzToText(lpData_OldValue, dwDataSize, oldValue);
      end;
      trdBinary:
      begin
        // todo widestring implementation
        regresult := RegQueryValueEx(mykey, PChar(encname), nil,
          @dwType, lpData_OldValue, @dwDataSize);
        oldValue := BinaryToStr(lpData_OldValue, dwDataSize);
      end;
      trdInteger:
      begin
        oldValue := IntToStr(DWord(lpData_OldValue^));
      end;
    end;
  end;

  // set the new value
  case regType of
    trdString:
    begin
      // length of widestring gives the number of 2 byte blocks (even on 4 byte chars)
      regresult := regSetValueExW(mykey, PWCHAR(encnameW), 0, REG_SZ,
        PWCHAR(encvalueW), length(encvalueW) * sizeOf(Wchar) + 2);

      if datafound then
      begin
        compareValue := UTF16ToUTF8(encvalueW);
      end;
    end;
    trdExpandString:
    begin
      regresult := regSetValueExW(mykey, PWCHAR(encnameW), 0,
        REG_EXPAND_SZ, PWCHAR(encvalueW), length(encvalueW) * sizeOf(Wchar) + 2);
      if datafound then
      begin
        compareValue := UTF16ToUTF8(encvalueW);
      end;
    end;
    trdMultiString:
    begin
      newValue := encvalue;
      if not MakeMultiSZ(Value, bytesarray, BytesarrayLength, lpdata,
        cbdata, errortext) then
        erroroccured := True
      else
      begin
        // todo widechar implementation
        regresult := regSetValueEx(mykey, PChar(encname), 0,
          REG_MULTI_SZ, lpdata, cbdata);
        if datafound then
        begin
          compareValue := WinCPToUTF8(PChar(lpdata));
        end;
      end;
    end;

    trdBinary:
    begin
      // todo widechar implementation
      s := StrAlloc(length(encvalue) + 1);
      StrCopy(s, PChar(encvalue));
      StrToBinary(s, lpdata, cbdata);
      regresult := regSetValueEx(mykey, PChar(encname), 0, REG_BINARY,
        lpdata, cbdata);
      if datafound then
      begin
        compareValue := BinaryToStr(lpdata, cbdata);
      end;
      StrDispose(s);
    end;
    trdInteger:
    begin
      iValue := StrToINT64(encvalue);
      regresult := regSetValueExW(mykey, PWChar(encnameW), 0, REG_Dword, @iValue, 4);
      if datafound then
      begin
        compareValue := IntToStr(iValue);
      end;
    end;
  end;


  if (regresult = ERROR_SUCCESS) and not errorOccured then
  begin
    if datafound then
    begin
      if compareValue <> oldValue then
      begin
        LogS := 'Variable "' + Name + '" had value  "' +
          StringReplace(oldvalue, #13#10, MultiszVisualDelimiter) + '"';
        LogDatei.log(LogS, LLInfo);
        LogS := 'Info:    "' + Name + '" changed to "' +
          StringReplace(comparevalue, #10, MultiszVisualDelimiter) + '"';
        LogDatei.log(LogS, LLInfo);
        LogDatei.NumberOfHints := LogDatei.NumberOfHints + 1;
      end
      else
      begin
        LogS := 'Variable "' + Name + '"  is keeping its value "' + compareValue + '"';
        LogDatei.log(LogS, LLInfo);
      end;
    end
    else
    begin
      LogS := 'Variable "' + Name + '"  set to "' + Value + '"';
      LogDatei.log(LogS, LLInfo);
    end;
  end
  else
  begin
    LogS := 'Variable "' + Name + '" could not be set  ' + ErrorText;
    LogDatei.log(LogS, LLError);
  end;
end;

function TuibRegistry.ReadString(const Name: string): string;
var
  regType: TuibRegDataType;

begin
  if ReadEntry(Name, regType, Result) then
  begin
    if (regType <> trdString) and (regType <> trdExpandString) then
    begin
      if regType = trdMultiString then
      //result := StringReplace (result, #13 + #10, MultiszVisualDelimiter)
      else
        raise ERegistryException.Create('Registry type mismatch, "' +
          Name + '" has not string type');
    end;
  end
  else
    raise ERegistryException.Create('Registry variable "' + Name + '" cannot be read');
end;


function TuibRegistry.ReadInteger(const Name: string): integer;
var
  regType: TuibRegDataType;
  StrValue: string = '';
begin
  if not ReadEntry(Name, regType, StrValue) then
    raise ERegistryException.Create('Registry variable "' + Name + '" cannot be read');

  if (regType <> trdInteger) then
    raise ERegistryException.Create('Registry type mismatch, "' +
      Name + '" has not DWORD type');

  Result := StrToInt(StrValue);
end;


procedure TuibRegistry.SupplementItems(Separator: char; const Name, Supplement: string);
var
  Entrylist, SuppList: TStringList;
  Line: string = '';
  Entry: string = '';
  i: integer = 0;
  Info: string = '';
  FoundDataType: TuibRegDataType;
  //dword1: integer;
  regresult: integer = 0;
  pdata: PByte;
  regType: pDWORD;
  psize: pdword;
  encname, encsupp: string;

begin
  encname := UTF8ToWinCP(Name);
  encsupp := UTF8ToWinCP(Supplement);
  getmem(psize, 4);
  getmem(pdata, psize^);
  new(regType);

  RegQueryValueEx(mykey, PChar(encname), nil, regType, pdata, psize);
  case regType^ of
    1: FoundDataType := trdString;
    2: FoundDataType := trdExpandString
    else
      FoundDataType := trdString;
  end;
  freemem(pdata);
  pdata := nil;
  freemem(psize);
  psize := nil;
  freemem(regType);
  regType := nil;


  (*  FoundDataType := GetDataType (Name); *)
  (* tut nichts *)
  try
    Line := ReadString(encname);
  except
    on e: ERegistryException do
    begin
     (*
      LogS := 'Error: "' + e.Message;
      LogDatei.log (LogS, LLNotice);
      LogDatei.NumberOfErrors := LogDatei.NumberOfErrors + 1;
     *)
      LogS := 'Warning: "' + e.Message;
      LogDatei.log(LogS, LLNotice);
    end;
  end;

  if Separator = #0 then
    Line := Line + encsupp
  else
  begin
    Entrylist := TStringList.Create;
    Supplist := TStringList.Create;

    (* build (old) Entrylist *)
    while Line <> '' do
    begin
      GetWord(Line, Entry, Line, [Separator]);
      Skip(Separator, Line, Line, Info);
      EntryList.Add(Entry);
      (* if pos ('%', Entry) > 0
      then FoundDataType := trdExpandString; *)
    end;

    (* build List of Supp-Entries *)
    Line := encsupp;
    while Line <> '' do
    begin
      GetWord(Line, Entry, Line, [Separator]);
      Skip(Separator, Line, Line, Info);
      SuppList.Add(Entry);
      if pos('%', Entry) > 0 then
        FoundDataType := trdExpandString;
    end;


    (* add Supp-Entries to Entrylist, if it is not included there *)
    for i := 1 to Supplist.Count do
    begin
      Entry := Supplist.Strings[i - 1];
      if Entrylist.IndexOf(Entry) = -1 then
        EntryList.Add(Entry);
    end;

    (* make Line for Registry *)
    Line := '';
    if EntryList.Count > 0 then
    begin
      Line := EntryList.Strings[0];
      for i := 2 to EntryList.Count do
        Line := Line + Separator + EntryList.Strings[i - 1];
    end;

    FreeAndNil(EntryList);
    FreeAndNil(SuppList);
  end;

  regresult := ERROR_SUCCESS;

  if FoundDataType = trdString then
    regresult := regSetValueEx(mykey, PChar(encname), 0, REG_SZ,
      PChar(Line), Length(Line))
  else
    regresult := regSetValueEx(mykey, PChar(encname), 0, REG_EXPAND_SZ,
      PChar(Line), Length(Line));

  if regresult = ERROR_SUCCESS then
  begin
    LogS := 'Variable "' + Name + '"   supplemented with "' + Supplement + '"';
    LogDatei.log(LogS, LLInfo);
  end
  else
  begin
    LogS := 'Error: Registry entry"' + Name +
      '"   could not be read or could not be written';
    if regresult <> ERROR_SUCCESS then
      LogS := LogS + ' WINAPI-Fehler ' + IntToStr(regresult);
    LogDatei.log(LogS, LLNotice);
  end;
end;


function TuibRegistry.RestoreFromFile(FilePath: string): boolean;
var
  regresult: integer = 0;

  {Variablen fuer das Setzen von Prozessrechten}
  //hToken: THandle;
  //tp: TTokenPrivileges;
  //h: DWord;

begin
  Result := False;

  SetProcessPrivilege('SeRestorePrivilege');

  regresult := RegRestoreKey(mykey, PChar(FilePath), 0);

  if regresult <> ERROR_SUCCESS then
  begin
    LogS := 'Error: The key could not be restored from file "' +
      FilePath + '".' + ' Code ' + IntToStr(regresult) + ': "' +
      RemoveLineBreaks(SysErrorMessage(regresult)) + '"';
    LogDatei.log(LogS, LLNotice);
  end
  else
  begin
    LogS := 'Key restored from "' + FilePath + '"';
    LogDatei.log(LogS, LLInfo);
    Result := True;
  end;
end;


function TuibRegistry.DeleteEntry(const Name: string): boolean;

var
  errorcode: longint = 0;
  Value: string = '';
  regType: TuibRegDataType;
  DisplayName: string = '';
  encname: string;

begin
  encname := UTF8ToWinCP(Name);
  Result := False;
  if ReadEntry(Name, regType, Value) then
  begin
    if Name = '' then
      DisplayName := '(Standard)'
    else
      DisplayName := Name;
    LogS := 'Entry   "' + DisplayName + '" = "' + Value + '"  ';

    errorcode := RegDeleteValue(mykey, PChar(encname));
    if errorCode = Error_success then
    begin
      Result := True;
      LogS := LogS + 'deleted';
      LogDatei.log(LogS, LLInfo);
    end
    else
    begin
      LogS := 'Error: ' + LogS + 'could not be deleted. Errorcode + ' +
        IntToStr(errorcode) + '. Message "' +
        RemoveLineBreaks(SysErrorMessage(ErrorCode)) + '"';
      LogDatei.log(LogS, LLNotice);
    end;
  end;
end;


function TuibRegistry.SetSecurity(TemplateKeyName, KeyName: string): boolean;
  // experimentell, noch nicht korrekt
var
  LengthTemplateSecDes: Dword = 0;
  pTemplateSecurityDescriptor: pSecurityDescriptor;
  regresult: integer = 0;
  hTemplateKey: HKEY;
  encTemplateKeyname, encKeyname: string;

  {Variablen f�r das Setzen von Proze�rechten}
  //hToken: THandle;
  //tp: TTokenPrivileges;
  //h: DWord;

  procedure KeyInfo(hSubKey: HKEY; var Number, MaxLength: integer);
  var
    pClass: PChar;
    pcbClass, pcsubkey, pcbmaxsubkeylength, pcbmaxclasslength, pcValues,
    pcbMaxValueNameLength, pcbMaxValueLen, pcbSecuritydescriptor: pDword;
    pftlastwritetime: pFileTime;
    regresult: integer = 0;

  begin
    Result := False;

    pClass := StrAlloc(30);
    GetMem(pcbClass, 4);
    GetMem(pcsubkey, 4);
    GetMem(pcbmaxsubkeylength, 4);
    GetMem(pcbmaxclasslength, 4);
    GetMem(pcValues, 4);
    GetMem(pcbMaxValueNameLength, 4);
    GetMem(pcbMaxValueLen, 4);
    GetMem(pcbSecuritydescriptor, 4);
    GetMem(pftlastwritetime, SizeOf(TFiletime));
    regresult :=
      RegQueryInfoKey(hSubKey, pClass, pcbClass, nil, pcSubKey,
      pcbmaxsubkeylength, pcbmaxclasslength, pcValues, pcbMaxValueNameLength,
      pcbMaxValueLen, pcbSecuritydescriptor, pftlastwritetime);

    Number := integer(pcSubKey^);
    MaxLength := integer(pcbMaxSubKeyLength^);

    FreeMem(pcbClass);
    FreeMem(pcsubkey);
    FreeMem(pcbmaxsubkeylength);
    FreeMem(pcbmaxclasslength);
    FreeMem(pcValues);
    FreeMem(pcbMaxValueNameLength);
    FreeMem(pcbMaxValueLen);
    FreeMem(pcbSecuritydescriptor);
    FreeMem(pftlastwritetime);
  end;


  procedure mySetSecurity(hSubKey: HKEY; SubKeyname: string;
  const pTemplateSecurity: pSecurityDescriptor);
  var
    hNewSubKey: HKEY;
    regresult: longint = 0;
    SubKeyNumber: integer = 0;
    MaxSubKeyLength: integer = 0;
    i: integer = 0;
    pSubKeyName: PChar;
    cbSubKeyName: DWORD = 0;
    NewPathName: string = '';
    pftlastwritetime: pFileTime;
    encname: string;

  begin
    encname := UTF8ToWinCP(SubKeyname);
    regresult := RegSetKeySecurity(hSubKey, dacl_security_information,
      pTemplateSecurity);

    KeyInfo(hSubKey, SubKeyNumber, MaxSubKeyLength);

    GetMem(pSubKeyName, ((MaxSubKeyLength + 1) * 2));
    GetMem(pftLastWriteTime, SizeOf(TFiletime));


    for i := 0 to (SubKeyNumber - 1) do
    begin
      regresult := RegEnumKeyEx(hSubKey, i, pSubKeyName, cbSubKeyName,
        nil, nil, nil, pftLastWriteTime);
      NewPathName := encname + '\' + pSubKeyName;
      regresult := RegOpenKeyEx(HKEY_USERS, PChar(NewPathName), 0,
        KEY_ALL_ACCESS, hNewSubKey);
      mySetSecurity(hNewSubKey, NewPathName, pTemplateSecurity);
      regresult := RegCloseKey(hNewSubKey);

    end;

    FreeMem(pSubKeyName);
    FreeMem(pftLastWriteTime);
  end;

begin
  encTemplateKeyname := UTF8ToWinCP(TemplateKeyName);
  encKeyname := UTF8ToWinCP(KeyName);
  SetProcessPrivilege('SeSecurityPrivilege');

  {Abfrage der Sicherheitsinformation des Templates}
  regresult := RegOpenKeyEx(HKEY_USERS, PChar(encTemplateKeyname),
    0, KEY_ALL_ACCESS, hTemplateKey);

  GetMem(pTemplateSecurityDescriptor, 0);
  regresult := RegGetKeySecurity(hTemplateKey, dacl_security_information,
    pTemplateSecurityDescriptor, LengthTemplateSecDes);
  FreeMem(pTemplateSecurityDescriptor);

  GetMem(pTemplateSecurityDescriptor, LengthTemplateSecDes);
  regresult := RegGetKeySecurity(hTemplateKey, dacl_security_information,
    pTemplateSecurityDescriptor, LengthTemplateSecDes);
  regresult := RegCloseKey(hTemplateKey);

  {Aufrufen der rekursiven Prozedur SetSecurity}
  mySetSecurity(mykey, KeyName, pTemplateSecurityDescriptor);

  FreeMem(pTemplateSecurityDescriptor);
end;

{$ENDIF WINDOWS}



end.
