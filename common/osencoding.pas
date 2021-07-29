unit osencoding;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel



{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  strUtils,
  lconvencoding,
  charencstreams,
  LAZUTF8;

procedure initEncoding;
function isSupportedEncoding(testEncoding: string): boolean;
procedure logSupportedEncodings;

function isEncodingUnicode(encodingString: string): boolean;
function UniStreamTypes2uniEncoding(inEncoding: TUniStreamTypes;
  hasBOM: boolean): string;
function uniEncoding2UniStreamTypes(fileName: string; encodingString: string;
  var hasBOM: boolean): TUniStreamTypes;

function hasFileBom(infilename: string): boolean;
function getFileBom(inFileName: string; var gottenEncoding: string): boolean;

function loadUnicodeTextFile(filename: string; var hasBOM: boolean;
  var foundEncoding: string): TStringList;
procedure saveUnicodeTextFile(inlist: TStrings; outFileName: string; encoding: string);

function stringListLoadUnicodeFromList(inlist: TStringList): TStringList;

function searchEncoding(const searchText: string): string; overload;
function searchEncoding(const searchText: string; var isPlainAscii: boolean): string;
  overload;

function reencode(const sourceText: string; const sourceEncoding: string): string;
  overload;
function reencode(const sourceText: string; const sourceEncoding: string;
  var usedSourceEncoding: string): string; overload;
function reencode(const sourceText: string; const sourceEncoding: string;
  var usedSourceEncoding: string; destEncoding: string): string; overload;

function loadTextFileWithEncoding(filename, encoding: string): TStringList;
procedure saveTextFileWithEncoding(inlist: TStrings; outFileName: string;
  encoding: string);

function osNormalizeEncoding(const Encoding: string): string;

function isPlainAsciiString(const searchText: string): boolean;

var
  supportedEncodings: TStringList;
  mysystemEncoding: string;
  DefaultEncoding: string = 'utf8';

implementation

uses
  oslog;

var
  i, k: integer;
  additionalEncoding: string;


procedure initEncoding;
begin
  {$IFDEF LINUX}
  mysystemEncoding := 'utf8';
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  mysystemEncoding := GetDefaultTextEncoding;
  {$ENDIF DARWIN}
  {$IFDEF WINDOWS}
  //mysystemEncoding := 'utf8';
  mysystemEncoding := GetDefaultTextEncoding;
  {$ENDIF WINDOWS}
  // get the supported encodings
  supportedEncodings := TStringList.Create;
  GetSupportedEncodings(supportedEncodings);
  supportedEncodings.Add('UTF-16');
  supportedEncodings.Add('UTF-16BE');
  supportedEncodings.Add('UTF-16LE');
  supportedEncodings.Add('UNICODE');
  //supportedEncodings.Add('UTF-16-BOM');
  supportedEncodings.Add('UTF-16BEBOM');
  supportedEncodings.Add('UTF-16LEBOM');
  //supportedEncodings.Add('UTF-32-BOM');
  supportedEncodings.Add('UTF-32BEBOM');
  supportedEncodings.Add('UTF-32LEBOM');

  // add the aliases (utf8 is alias for UTF-8)
  k := supportedEncodings.Count;
  for i := 0 to k - 1 do
  begin
    additionalEncoding := LowerCase(supportedEncodings.Strings[i]);
    (*
    if pos('-', additionalEncoding) > 0 then
      additionalEncoding := copy(additionalEncoding, 0,
        pos('-', additionalEncoding) - 1) + copy(additionalEncoding,
        pos('-', additionalEncoding) + 1, length(additionalEncoding));
        *)
    additionalEncoding := NormalizeEncoding(additionalEncoding);
    supportedEncodings.Add(additionalEncoding);
  end;
  //for i:= 0 to supportedEncodings.Count-1 do writeln(supportedEncodings.Strings[i]);
end;

function isSupportedEncoding(testEncoding: string): boolean;
begin
  Result := False;
  testEncoding := NormalizeEncoding(testEncoding);
  if supportedEncodings.IndexOf(testEncoding) > -1 then
    Result := True;
  // logdatei.log_prog('Found or given Encoding: ' + testEncoding +
  //              ' is not supported.', LLWarning);
end;

procedure logSupportedEncodings;
begin
  if LogDatei <> nil then
    for i := 0 to supportedEncodings.Count - 1 do
      logdatei.log_prog(supportedEncodings.Strings[i], LLDebug2);
end;



function isEncodingUnicode(encodingString: string): boolean;
var
  lencstr, bomstr: string;
begin
  lencstr := NormalizeEncoding(encodingString);
  Result := False;
  bomstr := copy(lencstr, length(lencstr) - 2, length(lencstr));
  if bomstr = 'bom' then
    Result := True
  else if (lencstr = Lowercase('unicode')) or (lencstr = Lowercase('utf8')) or
    (lencstr = Lowercase('utf16')) or (lencstr = Lowercase('utf32')) or
    (lencstr = Lowercase('utf16le')) or (lencstr = Lowercase('utf16be')) or
    (lencstr = Lowercase('ucs2le')) or (lencstr = Lowercase('ucs2be')) or
    (lencstr = Lowercase('utf32le')) or (lencstr = Lowercase('utf32be')) then
    Result := True;
end;

function UniStreamTypes2uniEncoding(inEncoding: TUniStreamTypes;
  hasBOM: boolean): string;
begin
  if (inEncoding = ufUtf8) then
    Result := 'utf8';

  if (inEncoding = ufUtf16le) then
    Result := 'utf16le';

  if (inEncoding = ufUtf16be) then
    Result := 'utf16be';

  if (inEncoding = ufUtf32le) then
    Result := 'utf32le';

  if (inEncoding = ufUtf32be) then
    Result := 'utf132be';

  if (inEncoding = ufANSI) then
    Result := 'ansi';

  if (hasBOM = True) and (inEncoding <> ufANSI) then
    Result := Result + 'bom';

end;

function uniEncoding2UniStreamTypes(fileName: string; encodingString: string;
  var hasBOM: boolean): TUniStreamTypes;
var
  lencstr: string;
  fCES: TCharEncStream;
begin
  Result := ufUtf8;
  hasBOM := False;
  //fCES := TCharEncStream.Create;
  //fCES.Reset;
  lencstr := NormalizeEncoding(encodingString);
  if copy(lencstr, length(lencstr) - 2, length(lencstr)) = 'bom' then
  begin
    hasBOM := True;
    lencstr := copy(lencstr, 0, length(lencstr) - 3);
  end;

  if lencstr = Lowercase('unicode') then
    if fileExists(fileName) then
    begin
      fCES := TCharEncStream.Create;
      fCES.Reset;
      fCES.LoadFromFile(ExpandFileName(fileName));
      hasBOM := fCES.HasBOM;
      Result := fCES.UniStreamType;
    end
    else
      Result := ufUtf8;

  if (lencstr = Lowercase('utf8')) then
    Result := ufUtf8;

  if (lencstr = Lowercase('utf16le')) or (lencstr = Lowercase('ucs2le')) then
    Result := ufUtf16le;

  if (lencstr = Lowercase('utf16')) or (lencstr = Lowercase('utf16be')) or
    (lencstr = Lowercase('ucs2be')) then
    Result := ufUtf16be;

  if (lencstr = Lowercase('utf32')) or (lencstr = Lowercase('utf32be')) then
    Result := ufUtf32be;

  if (lencstr = Lowercase('utf32le')) then
    Result := ufUtf32le;

end;

function hasFileBom(inFileName: string): boolean;
var
  fCES: TCharEncStream;
  Utype: TUniStreamTypes;
begin
  fCES := TCharEncStream.Create;
  fCES.Reset;
  inFileName := ExpandFileName(inFileName);
  fCES.LoadFromFile(inFileName);
  Utype := fCES.UniStreamType;
  Result := fCES.HasBOM;
  fCES.Free;
end;

function getFileBom(inFileName: string; var gottenEncoding: string): boolean;
var
  myFile: TFileStream;
  Buffer: array [0..3] of byte;   // or AnsiChar;
  //FileFirstBytes : String ='';
  //myFile : File ;  i, count : SmallInt ;
  Fname: string;
begin
  (*
  Assignfile(myFile, inFileName);
  FileMode := fmOpenRead;
  Reset(myFile, 1);
  BlockRead(myFile, Buffer, 4, count);
  for i := 0 to count do
    FileFirstBytes:= FileFirstBytes + IntToStr(Buffer[i]);
  CloseFile(myFile);
  *)
  Fname := ExpandFileName(inFileName);
  try
    myFile := TFileStream.Create(Fname, fmOpenRead or fmShareDenyNone);
  except
    on E: Exception do
    begin
      Logdatei.log('getFileBom "' + Fname + '"', LLwarning);
      Logdatei.log(e.ClassName + ': Failed getFileBom, system message: "' +
        E.Message + '" - will retry',
        LLwarning);
      Sleep(200);
      try
        myFile :=
          TFileStream.Create(Fname, fmOpenRead or fmShareDenyNone);
      except
        on E: Exception do
        begin
          Logdatei.log('getFileBom Retry1"' + Fname + '"', LLwarning);
          Logdatei.log(e.ClassName +
            ': Failed getFileBom, system message: "' + E.Message +
            '" - will retry',
            LLwarning);
          Sleep(200);
          try
            myFile :=
              TFileStream.Create(Fname, fmOpenRead or fmShareDenyNone);
          except
            on E: Exception do
            begin
              Logdatei.log('getFileBom Retry2"' + Fname +
                '"', LLCritical);
              Logdatei.log(e.ClassName +
                ': Failed getFileBom, system message: "' +
                E.Message + '" - giving up',
                LLCritical);
              RaiseLastOSError;
            end
          end;
        end
      end;
    end
  end;
  //while F.Position < F.Size do
  myFile.Read(Buffer, 4);
  //FileFirstBytes := Buffer;
  myFile.Free;

  if (Buffer[0] = 239) and (Buffer[1] = 187) and (Buffer[2] = 191) then
    gottenEncoding := 'utf8bom';            //'#239#187#191' //#$EF#$BB#$BF
  if (Buffer[0] = 254) and (Buffer[1] = 255) then
    gottenEncoding := 'utf16bebom';          //'#254#255'     //#$FE#$FF
  if (Buffer[0] = 255) and (Buffer[1] = 254) then
    gottenEncoding := 'utf16lebom';          //'#255#254'     //#$FF#$FE
  if (Buffer[0] = 0) and (Buffer[1] = 0) and (Buffer[2] = 254) and (Buffer[3] = 255) then
    gottenEncoding := 'utf32bebom';          //'#0#0#254#255' //#0#0#$FE#$FF
  if (Buffer[0] = 255) and (Buffer[1] = 254) and (Buffer[2] = 0) and (Buffer[3] = 0) then
    gottenEncoding := 'utf32lebom';          //'255#254#0#0'  //#$FE#$FF#0#0

  if (copy(gottenEncoding, length(gottenEncoding) - 2, length(gottenEncoding)) =
    'bom') then
    Result := True
  else
    Result := False;

end;

function loadUnicodeTextFile(fileName: string; var hasBOM: boolean;
  var foundEncoding: string): TStringList;
var
  fCES: TCharEncStream;
  str: string;
begin
  Result := TStringList.Create;
  fCES := TCharEncStream.Create;
  fCES.Reset;
  fileName := ExpandFileName(fileName);
  try
    fCES.LoadFromFile(fileName);
  except
    on E: Exception do
    begin
      Logdatei.log(e.ClassName + ': Failed loadUnicodeTextFile: "' +
        fileName + ' (will retry)", system message: "' + E.Message + '"',
        LLDebug);
      Sleep(200);
      try
        fCES.LoadFromFile(fileName);
      except
        on E: Exception do
        begin
          Logdatei.log(e.ClassName + ': Failed loadUnicodeTextFile: "' +
            fileName + '" (will retry 1), system message: "' + E.Message + '"',
            LLDebug);
          Sleep(200);
          try
            fCES.LoadFromFile(fileName);
          except
            on E: Exception do
            begin
              Logdatei.log(e.ClassName +
                ': Failed loadUnicodeTextFile: "' + fileName +
                '" (will not retry), system message: "' + E.Message + '"',
                LLError);
              RaiseLastOSError;
            end
          end;
        end
      end;
    end
  end;
  str := fCES.UTF8Text;
  hasBOM := fCES.HasBOM;
  foundEncoding := UniStreamTypes2uniEncoding(fCES.UniStreamType, hasBOM);
  Result.Text := str;
  fCES.Free;
end;

procedure saveUnicodeTextFile(inlist: TStrings; outFileName: string; encoding: string);
var
  fCES: TCharEncStream;
  hasBOM: boolean;
begin
  fCES := TCharEncStream.Create;
  fCES.Reset;
  fCES.UniStreamType := uniEncoding2UniStreamTypes(outFileName, encoding, hasBOM);
  fCES.HasBOM := hasBOM;
  fCES.UTF8Text := inlist.Text;
  fCES.SaveToFile(outFileName);
  //fCES.ANSIEnc := GetSystemEncoding;
  fCES.Free;
end;

(*
function stringListLoadUtf16leFromFile(filename: string): TStringList;
var
  fCES: TCharEncStream;
begin
  Result := TStringList.Create;
  fCES := TCharEncStream.Create;
  fCES.Reset;
  fCES.UniStreamType := ufUtf16le;
  fileName := ExpandFileName(fileName);
  fCES.LoadFromFile(fileName);
  fCES.ANSIEnc := GetSystemEncoding;
  Result.Text := fCES.UTF8Text;
  fCES.Free;
end;
*)

function stringListLoadUnicodeFromList(inlist: TStringList): TStringList;
var
  fCES: TCharEncStream;
begin
  Result := TStringList.Create;
  fCES := TCharEncStream.Create;
  fCES.Reset;
  inlist.SaveToStream(fCES);
  fCES.ANSIEnc := GetSystemEncoding;
  Result.Text := fCES.UTF8Text;
  fCES.Free;
end;


function isStringInList(const str: string; const list: TStringList): boolean;
  // list.IndexOf(str) > -1 does not work as expected: ucs2be is found if str is 'u c s 2 b e'
var
  i: integer;
begin
  Result := False;
  i := 0;
  repeat
    if list.Strings[i] = str then
    begin
      Result := True;
    end;
    Inc(i);
  until (Result = True) or (i >= list.Count);
end;

function isPlainAsciiString(const searchText: string): boolean;
var
  endreached: boolean;
  i: cardinal;
begin
  Result := True;
  endreached := False;
  i := 1;
  repeat
    if not CharInSet(searchtext.Chars[i], [#0..#9, #11, #12, #14..#31, #127]) then
      Result := False;
    Inc(i);
    if i >= length(searchText) - 1 then endreached := True;
  until endreached or not Result;
end;

function searchEncoding(const searchText: string): string;
var
  isPlainAscii: boolean;
begin
  Result := searchEncoding(searchText, isPlainAscii);
end;

function searchEncoding(const searchText: string; var isPlainAscii: boolean): string;
  // tries to find entry: encoding=<encoding to use>
var
  mylist, myencodings: TStringList;
  i: integer;
  mytext: string;
  found: boolean;
  foundencodingstring, newencodingstring: string;
begin
  isPlainAscii := isPlainAsciiString(searchText);
  logdatei.log('searchEncoding: isPlainAscii = ' +
    BoolToStr(isPlainAscii, True), LLDebug2);
  Result := '';
  found := False;
  mylist := TStringList.Create;
  myencodings := TStringList.Create;
  mylist.Text := searchText;
  GetSupportedEncodings(myencodings);
  if mylist.IndexOfName('encoding') <> -1 then
  begin
    // we found an entry: encoding=<encoding to use>
    foundencodingstring := trim(mylist.Values['encoding']);
    foundencodingstring := NormalizeEncoding(foundencodingstring);
    logdatei.log('searchEncoding: foundencodingstring: ' +
      foundencodingstring, LLDebug2);
    if isStringInList(foundencodingstring, supportedEncodings) then
      Result := foundencodingstring
    else
    begin
      logdatei.log('searchEncoding: Foundencodingstring ' +
        foundencodingstring + ' is not in supportedEncodings list', LLWarning);
      i := 0;
      repeat
        // convert via utf8
        newencodingstring := ConvertEncoding(foundencodingstring,
          myencodings.Strings[i], 'utf8');
        newencodingstring := ConvertEncoding(newencodingstring, 'utf8',
          DefaultEncoding);
        logdatei.log('myencodings.Strings[i]: ' + myencodings.Strings[i] +
          'newencodingstring: ' + newencodingstring, LLDebug3);
        if isStringInList(newencodingstring, supportedEncodings) then
        begin
          // newencodingstring is a supported encoding
          Result := newencodingstring;
          found := True;
        end;
        Inc(i);
      until (found = True) or (i >= myencodings.Count);
    end;
    //logdatei.DependentAdd('searchText: '+searchText+', foundencodingstring: '+foundencodingstring+', mysystemEncoding: '+mysystemEncoding,LLEssential);
    //result := foundencodingstring;
  end
  else
  begin
    // it seems that we normally do not reach this branch

    // we found no encoding entry
    // we have now 2 possibilities:
    // a) there is no encoding entry
    // b) there is one but we do not find it because of the wrong encoding
    i := 0;
    repeat
      begin
        mytext := ConvertEncoding(searchText, myencodings.Strings[i], DefaultEncoding);
        logdatei.log('mytext: ' + mytext + ', myencodings.Strings[i]: ' +
          myencodings.Strings[i] + ', DefaultEncoding: ' + DefaultEncoding, LLDebug3);
        mylist.Text := mytext;
        if mylist.IndexOfName('encoding') <> -1 then
        begin
          // we found an entry: encoding=<encoding to use>
          Result := mylist.Values['encoding'];
          found := True;
        end;
        Inc(i);
      end;
    until (found = True) or (i >= myencodings.Count);
  end;
  myencodings.Free;
  mylist.Free;
end;

function reencode(const sourceText: string; const sourceEncoding: string): string;
var
  str: string;
begin
  Result := reencode(sourceText, sourceEncoding, str);
end;

function reencode(const sourceText: string; const sourceEncoding: string;
  var usedSourceEncoding: string): string;
begin
  Result := reencode(sourceText, sourceEncoding, usedSourceEncoding, 'utf8');
end;

function reencode(const sourceText: string; const sourceEncoding: string;
  var usedSourceEncoding: string; destEncoding: string): string;
var
  str: string;
  mylist: TStringList;
  i: integer;
begin
  //for i:= 0 to supportedEncodings.Count-1 do writeln(supportedEncodings.Strings[i]);
  Result := sourceText;
  usedSourceEncoding := sourceEncoding;
  if LowerCase(sourceEncoding) = 'system' then
    usedSourceEncoding := mysystemEncoding;
  if LowerCase(destEncoding) = 'system' then
    destEncoding := mysystemEncoding;
  // which means normally: do nothing
  if LowerCase(sourceEncoding) = 'auto' then
    usedSourceEncoding := guessEncoding(sourceText);
  // erasing the BOM part if exists
  if (copy(usedSourceEncoding, length(usedSourceEncoding) - 2,
    length(usedSourceEncoding)) = 'bom') then
    if (usedSourceEncoding[length(usedSourceEncoding) - 3] = '') then
      usedSourceEncoding := copy(usedSourceEncoding, 0, length(usedSourceEncoding) - 4)
    else
      usedSourceEncoding := copy(usedSourceEncoding, 0, length(usedSourceEncoding) - 3);
  if (copy(destEncoding, length(destEncoding) - 2, length(destEncoding)) = 'bom') then
    if (destEncoding[length(destEncoding) - 3] = '') then
      destEncoding := copy(destEncoding, 0, length(destEncoding) - 4)
    else
      destEncoding := copy(destEncoding, 0, length(destEncoding) - 3);
  // if not supported encoding
  if not isSupportedEncoding(usedSourceEncoding) then
    if Assigned(logdatei) then
      logdatei.log_prog('Found or given Encoding: ' + usedSourceEncoding +
        ' is not supported.', LLWarning);
  // normalizing encodings
  usedSourceEncoding := NormalizeEncoding(usedSourceEncoding);
  destEncoding := NormalizeEncoding(destEncoding);
  // if used and destination encodings are different
  if LowerCase(usedSourceEncoding) <> LowerCase(destEncoding) then
  begin
    if Assigned(logdatei) then
      logdatei.log_prog('Encodings are different so we have to reencode from ' +
        usedSourceEncoding + ' to ' + destEncoding, LLDebug2);
    // if reencoding is from or to utf8
    if (usedSourceEncoding = 'utf8') or (destEncoding = 'utf8') then
    begin
      if Assigned(logdatei) then
        logdatei.log_prog('We encode directly from or to utf8.', LLDebug2);

      if (usedSourceEncoding = 'utf16') then
        usedSourceEncoding := 'ucs2be';

      if (destEncoding = 'utf16') then
        destEncoding := 'ucs2be';

      if (usedSourceEncoding = 'utf16le') then
        usedSourceEncoding := 'ucs2le';

      if (destEncoding = 'utf16le') then
        destEncoding := 'ucs2le';

      if (usedSourceEncoding = 'utf16be') then
        usedSourceEncoding := 'ucs2be';

      if (destEncoding = 'utf16be') then
        destEncoding := 'ucs2be';

      if (usedSourceEncoding = 'unicode') then
        usedSourceEncoding := 'utf8';

      if (destEncoding = 'unicode') then
        destEncoding := 'utf8';

      // we use ConvertEncoding
      Result := ConvertEncoding(sourceText, usedSourceEncoding, destEncoding);
    end
    else
    begin
      if Assigned(logdatei) then
        logdatei.log_prog('We encode via utf8.', LLDebug2);
      if Assigned(logdatei) then
        logdatei.log_prog('Encodings are different so we have to reencode from ' +
          usedSourceEncoding + ' to ' + destEncoding, LLDebug2);
      if (usedSourceEncoding = 'ucs2be') or (usedSourceEncoding = 'ucs2le') then
      begin
        if Assigned(logdatei) then
          logdatei.log_prog('We encode line by line.', LLDebug2);
        mylist := TStringList.Create;
        mylist.Text := sourceText;
        for i := 0 to mylist.Count - 1 do
        begin
          str := mylist.Strings[i];
          mylist.Strings[i] := ConvertEncoding(str, usedSourceEncoding, 'utf8');
          if Assigned(logdatei) then
            logdatei.log_prog(usedSourceEncoding + ' to utf8: ' +
              str + ' to ' + mylist.Strings[i], LLDebug3);
        end;
        Result := mylist.Text;
        mylist.Free;
      end
      else
      begin
        Result := ConvertEncoding(sourceText, usedSourceEncoding, 'utf8');
        if Assigned(logdatei) then
          logdatei.log_prog(usedSourceEncoding + ' to utf8: ' +
            sourceText + ' to ' + Result, LLDebug3);
      end;
    end;
    if Assigned(logdatei) then
      logdatei.log_prog('Reencoding from ' + usedSourceEncoding +
        ' to ' + destEncoding, LLDebug2);
  end
  else if Assigned(logdatei) then
    logdatei.log_prog('Nothing to do: Reencoding from ' + usedSourceEncoding +
      ' to ' + destEncoding, LLDebug2);
end;

function loadTextFileWithEncoding(filename, encoding: string): TStringList;
(* var
    txtfile: Text;
    rawline: WideString;
    encline: string;
  *)
var
  bool: boolean;
  str: string;
begin
  Result := TStringList.Create;

  if encoding = '' then
  begin
    LogDatei.log('Warning : encodingString is empty - Fallback to System encoding',
      LLWarning);
    encoding := 'system';
  end;

  if isEncodingUnicode(encoding) then
    Result.AddStrings(loadUnicodeTextFile(filename, bool, str))
  else
  begin
    try
      Result.loadFromFile(filename);
    except
      on E: Exception do
      begin
        Logdatei.log(e.ClassName + ': Failed loadTextFileWithEncoding (ANSI): "' +
          fileName + ' (will retry)", system message: "' + E.Message + '"',
          LLDebug);
        Sleep(200);
        try
          Result.loadFromFile(filename);
        except
          on E: Exception do
          begin
            Logdatei.log(e.ClassName +
              ': Failed loadTextFileWithEncoding (ANSI): "' + fileName +
              '" (will retry 1), system message: "' + E.Message + '"',
              LLDebug);
            Sleep(200);
            try
              Result.loadFromFile(filename);
            except
              on E: Exception do
              begin
                Logdatei.log(e.ClassName +
                  ': Failed loadTextFileWithEncoding (ANSI): "' +
                  fileName + '" (will not retry), system message: "' + E.Message + '"',
                  LLError);
                RaiseLastOSError;
              end
            end;
          end
        end;
      end
    end;
    Result.Text := reencode(Result.Text, encoding);
  end;
  //str := result.Text;
end;

procedure saveTextFileWithEncoding(inlist: TStrings; outFileName: string;
  encoding: string);
var
  myfile: Text;
  usedenc: string;
begin
  if encoding = '' then
  begin
    LogDatei.log('Warning : encodingString is empty - Fallback to System encoding',
      LLWarning);
    encoding := 'system';
  end;

  if isEncodingUnicode(encoding) then
    saveUnicodeTextFile(inlist, outFileName, encoding)
  else
  begin
    AssignFile(myfile, outFileName);
    Rewrite(myfile);
    LogDatei.log('Will save (' + encoding + ') encoding to file: ' +
      outFileName + ' :', LLDebug2);
    LogDatei.log('-----------------', LLDebug3);
    Write(myfile, reencode(inlist.Text, 'utf8', usedenc, encoding));

    LogDatei.log('-----------------', LLDebug3);
    CloseFile(myfile);
    if LogDatei.UsedLogLevel >= LLDebug3 then
    begin
      LogDatei.log('Read file ' + outFileName + ' with encoding: ' +
        encoding, LLDebug2);
      LogDatei.log('-----------------', LLDebug3);
      logdatei.includelogtail(outFileName, inlist.Count, encoding);
      LogDatei.log('-----------------', LLDebug3);
    end;
  end;
end;

function osNormalizeEncoding(const Encoding: string): string;
begin
  Result := NormalizeEncoding(Encoding);
end;

initialization
  initEncoding;
end.
