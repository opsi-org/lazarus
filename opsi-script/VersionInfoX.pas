unit VersionInfoX;

//from
//http://devblog.brahmancreations.com/content/free-pascal-win32-versioninfopas

(* I wanted to add some Windows version information into my Lazarus/FreePascal programs
and tried these two options, one from Anders Melander ,
http://melander.dk/articles/versioninfo/ and the other a part of RXLib,
a general Component Library developed for Delphi and ported to FreePascal,
both of which are wrappers around the Win32 GetFileVersionInfo (and related) API functions.

With some support forum 1, forum 2 from the Lazarus Forums and FreePascal newsgroups
I was finally able to get it Anders Melander's version to compile,
mostly String/AnsiString incompatibilities, Large_Integer issues, and changing some
library names to their Lazarus/FPC equivalents.
The RX version had to many dependencies on some of its own units for me to convert it quickly.
*)

// -----------------------------------------------------------------------------
// Project:	VersionInfo
// Module:	VersionInfo
// Description: GetFileVersionInfo Win32 API wrapper.
// Version:	1.1
// Release:	1
// Date:	2-MAR-2008
// Target:	Delphi 2007, Win32.
// Author(s):	Anders Melander, anders@melander.dk
// Copyright:	(c) 2007-2008 Anders Melander.
//		All rights reserved.
// -----------------------------------------------------------------------------
// This work is licensed under the
//   "Creative Commons Attribution-Share Alike 3.0 Unported" license.
// http://creativecommons.org/licenses/by-sa/3.0/
// -----------------------------------------------------------------------------

//http://melander.dk/download/
// in opsi-winst context renamed to VersionInfoX since there is an old versioninfo unit

// uncommented Exception.CreateFmt(SListIndexError, [Index]), replaced by returning nil
// supplemented function DoGetString(const Key: string): string; 


{$IFDEF FPC}
	{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}

{$ENDIF}

interface

uses
{$IFDEF FPC}
LCLIntf,
lconvencoding,
{$ELSE}
{$ENDIF}
  Classes,
  Windows;

type
  TTranslationRec = packed record
    case Integer of
    0: (
      LanguageID: WORD;
      CharsetID: WORD);
    1: (
      TranslationID: DWORD);
  end;
  PTranslationRec = ^TTranslationRec;
  TTranslationTable = array[0..0] of TTranslationRec;
  PTranslationTable = ^TTranslationTable;


  TVersionInfo = class
  strict private
    FVersionBuffer: pointer;
    FValid: boolean;
    FFileInfo: PVSFixedFileInfo;
    FTranslationTable: PTranslationTable;
    FTranslationCount: integer;
  private
    function DoGetString(const Key: string): string;
    function GetCharset(Index: integer): WORD;
    function GetLanguage(Index: integer): WORD;
    function GetLanguageName(Index: integer): AnsiString;
    function GetFileVersion: int64;
    function GetProductVersion: int64;
    function GetFileFlags: DWORD;
    function GetFileDate: int64;
    function GetFileSubType: DWORD;
    function GetFileType: DWORD;
    function GetOS: DWORD;
    function GetTranslationRec(Index: integer): PTranslationRec;
    property TranslationTable[Index: integer]: PTranslationRec read GetTranslationRec;
  protected
    property VersionBuffer: pointer read FVersionBuffer;
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;
    class function VersionToString(Version: int64): string;
    class function StringToVersion(const Value: string): int64;
    function GetString(const Key: string; LanguageID: integer; CharsetID: integer): string; overload;
    function GetString(const Key, TranslationID: string): string; overload;
	function GetString(const Key: string; Index: integer = 0): string; overload;
	function GetFileVersionWithDots : string;
    property Valid: boolean read FValid;
    property Strings[const Key: string]: string read DoGetString; default;
    property FileVersion: int64 read GetFileVersion;
    property ProductVersion: int64 read GetProductVersion;
    property FileFlags: DWORD read GetFileFlags;
    property OS: DWORD read GetOS;
    property FileType: DWORD read GetFileType;
    property FileSubType: DWORD read GetFileSubType;
    property FileDate: int64 read GetFileDate;
    property LanguageID[Index: integer]: WORD read GetLanguage;
    property CharsetID[Index: integer]: WORD read GetCharset;
    //property LanguageID[Index: integer]: WORD read GetLanguage;
    //property CharsetID[Index: integer]: WORD read GetCharset;
    property LanguageNames[Index: integer]: string read GetLanguageName;
    property TranslationCount: integer read FTranslationCount;
  end;

  const
  PredefinedStrings: array[0..11] of string =
    ('Comments', 'CompanyName', 'FileDescription', 'FileVersion',
    'InternalName', 'LegalCopyright', 'LegalTrademarks', 'OriginalFilename',
    'PrivateBuild', 'ProductName', 'ProductVersion', 'SpecialBuild');


implementation

uses
{$IFDEF FPC}
{$ELSE}
RTLConsts,
{$ENDIF}
SysUtils;

{ TVersionInfoX }

constructor TVersionInfo.Create(const Filename: string);
var
  OrgFileName: AnsiString;
  InfoSize, Dummy: DWORD;
  Size: DWORD;
begin
  inherited Create;

  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the const string into a local variable to create a writeable copy.
  OrgFileName := Filename;
  UniqueString(OrgFileName);
  InfoSize := GetFileVersionInfoSize(PChar(OrgFileName), Dummy);
  if InfoSize <> 0 then
  begin
    GetMem(FVersionBuffer, InfoSize);
    try
      if GetFileVersionInfo(PChar(OrgFileName), Dummy, InfoSize, FVersionBuffer) then
      begin
        FValid := True;
        if (not VerQueryValue(FVersionBuffer, '\', Pointer(FFileInfo), Size)) then
          FFileInfo := nil;

        if (VerQueryValue(VersionBuffer, '\VarFileInfo\Translation', pointer(FTranslationTable), Size)) then
          FTranslationCount := Size div SizeOf(TTranslationRec)
        else
          FTranslationCount := 0;
      end;
    finally
      if (not FValid) then
      begin
        FreeMem(FVersionBuffer);
        FVersionBuffer := nil;
      end;
    end;
  end;
end;

destructor TVersionInfo.Destroy;
begin
  if (FVersionBuffer <> nil) then
    FreeMem(FVersionBuffer);
  FValid := False;
  inherited Destroy;
end;

function TVersionInfo.GetCharset(Index: integer): WORD;
begin
{$IFDEF FPC}
  Result := TranslationTable[Index]^.CharsetID;
{$ELSE}
  Result := TranslationTable[Index].CharsetID;
{$ENDIF}
end;

function TVersionInfo.GetFileDate: int64;
var
  LargeInteger: Large_Integer;
begin
  if (Valid) and (FFileInfo <> nil) then
  begin
{$IFDEF FPC}
    LargeInteger.LowPart := FFileInfo^.dwFileDateLS;
    LargeInteger.HighPart := FFileInfo^.dwFileDateMS;
{$ELSE}
    LargeInteger.LowPart := FFileInfo.dwFileDateLS;
    LargeInteger.HighPart := FFileInfo.dwFileDateMS;
{$ENDIF}
    Result := LargeInteger.QuadPart;
  end else
    Result := 0;
end;

function TVersionInfo.GetFileFlags: DWORD;
begin
  if (Valid) and (FFileInfo <> nil) then
{$IFDEF FPC}
    Result := FFileInfo^.dwFileFlags and FFileInfo^.dwFileFlagsMask
{$ELSE}
    Result := FFileInfo.dwFileFlags and FFileInfo.dwFileFlagsMask
{$ENDIF}     

  else
    Result := 0;
end;

function TVersionInfo.GetFileSubType: DWORD;
begin
  if (Valid) and (FFileInfo <> nil) then
{$IFDEF FPC}
    Result := FFileInfo^.dwFileSubtype
{$ELSE}
    Result := FFileInfo.dwFileSubtype
{$ENDIF}      
  else
    Result := 0;
end;

function TVersionInfo.GetFileType: DWORD;
begin
  if (Valid) and (FFileInfo <> nil) then
{$IFDEF FPC}
    Result := FFileInfo^.dwFileType
{$ELSE}
    Result := FFileInfo.dwFileType
{$ENDIF}      
  else
    Result := 0;
end;

function TVersionInfo.GetFileVersion: int64;
var
  LargeInteger: Large_Integer;
begin
  if (Valid) and (FFileInfo <> nil) then
  begin
{$IFDEF FPC}
    LargeInteger.LowPart := FFileInfo^.dwFileVersionLS;
    LargeInteger.HighPart := FFileInfo^.dwFileVersionMS;
{$ELSE}
		LargeInteger.LowPart := FFileInfo.dwFileVersionLS;
		LargeInteger.HighPart := FFileInfo.dwFileVersionMS;
{$ENDIF}      
    Result := LargeInteger.QuadPart;
  end else
    Result := 0;
end;

function TVersionInfo.GetLanguage(Index: integer): WORD;
begin
{$IFDEF FPC}
  Result := TranslationTable[Index]^.LanguageID;
{$ELSE}
  Result := TranslationTable[Index].LanguageID;
{$ENDIF}      
end;

function TVersionInfo.GetLanguageName(Index: integer): AnsiString;
var
  Size: DWORD;
begin
  SetLength(Result, 255);
{$IFDEF FPC}
  Size := VerLanguageName(TranslationTable[Index]^.TranslationID, PChar(Result), Length(Result));
{$ELSE}
  Size := VerLanguageName(TranslationTable[Index].TranslationID, PChar(Result), Length(Result));
{$ENDIF}      
  SetLength(Result, Size);
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TVersionInfo.GetTranslationRec(Index: integer): PTranslationRec;
begin
  if (not Valid) or (Index < 0) or (Index >= FTranslationCount) then
    result := nil
    //raise Exception.CreateFmt(SListIndexError, [Index]);
  else
  Result := @(FTranslationTable[Index]);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

function TVersionInfo.GetOS: DWORD;
begin
  if (Valid) and (FFileInfo <> nil) then
{$IFDEF FPC}
    Result := FFileInfo^.dwFileOS
{$ELSE}
    Result := FFileInfo.dwFileOS
{$ENDIF}      
  else
    Result := 0;
end;

function TVersionInfo.GetProductVersion: int64;
var
  LargeInteger: Large_Integer;
begin
  if (Valid) and (FFileInfo <> nil) then
  begin
{$IFDEF FPC}
    LargeInteger.LowPart := FFileInfo^.dwProductVersionLS;
    LargeInteger.HighPart := FFileInfo^.dwProductVersionMS;
{$ELSE}
    LargeInteger.LowPart := FFileInfo.dwProductVersionLS;
    LargeInteger.HighPart := FFileInfo.dwProductVersionMS;
{$ENDIF}      
    Result := LargeInteger.QuadPart;
  end else
    Result := 0;
end;

function TVersionInfo.GetString(const Key: string; Index: integer): string;
var
  TranslationRec: PTranslationRec;
begin
  TranslationRec := TranslationTable[Index];
  if TranslationRec = nil
  then
    result := ''
  else
{$IFDEF FPC}
    Result := GetString(Key, TranslationRec^.LanguageID, TranslationRec^.CharsetID);
{$ELSE}
    Result := GetString(Key, TranslationRec.LanguageID, TranslationRec.CharsetID);
{$ENDIF}      
end;

function TVersionInfo.DoGetString(const Key: string): string;
begin
  Result := GetString(Key, 0);
end;

function TVersionInfo.GetString(const Key: string; LanguageID, CharsetID: integer): string;
const
  MAXCODEPAGE = 18;
var
  TranslationID: string;
  codepagelist : array [1..MAXCODEPAGE] of integer;
  cpage : integer = 1;
begin
  TranslationID := Format('%.4x%.4x', [LanguageID, CharsetID]);
  Result := GetString(Key, TranslationID);

  if Result = '' then
  begin
    //guess the code page
    TranslationID := Format('%.4x%.4x', [LanguageID, StrToInt(copy(GetDefaultTextEncoding,3,4))]);
    Result := GetString(Key, TranslationID);
  end;

  if Result = '' then
  begin
    // check for all well known code pages
    codepagelist[1] := 874;
    codepagelist[2] := 932;
    codepagelist[3] := 936;
    codepagelist[4] := 949;
    codepagelist[5] := 950;
    codepagelist[6] := 1200;
    codepagelist[7] := 1201;
    codepagelist[8] := 1250;
    codepagelist[9] := 1251;
    codepagelist[10] := 1252;
    codepagelist[11] := 1253;
    codepagelist[12] := 1254;
    codepagelist[13] := 1255;
    codepagelist[14] := 1256;
    codepagelist[15] := 1257;
    codepagelist[16] := 1258;
    codepagelist[17] := 65000;
    codepagelist[18] := 65001;
  end;
  while (Result = '') and (cpage <= MAXCODEPAGE) do
  begin
    // check for all well known code pages
    TranslationID := Format('%.4x%.4x', [LanguageID, codepagelist[cpage]]);
    Result := GetString(Key, TranslationID);
    inc(cpage);
  end;
end;

function TVersionInfo.GetString(const Key: string; const TranslationID: string): string;
var
  Value: PChar;
  s: string;
  Size: DWORD;
begin
  if (Valid) then
  begin
    s := Format('\StringFileInfo\%s\%s', [TranslationID, Key]);
    if (VerQueryValue(VersionBuffer, PChar(s), pointer(Value), Size)) then
      Result := PChar(Value)
    else
      Result := '';
  end else
    Result := '';
end;

class function TVersionInfo.StringToVersion(const Value: string): int64;
var
  Version: record
    case Integer of
    0: (
      Words: array[0..3] of WORD);
    1: (
      QuadPart: int64);
  end;
  s, n: string;
  w: integer;
  i: integer;
begin
  s := Value;
  w := 0;
  while (s <> '') and (w < 4) do
  begin
    i := pos('.', s);
    if (i <= 0) then
      i := Length(s)+1;
    n := Copy(s, 1, i-1);
    s := Copy(s, i+1, MaxInt);
    Version.Words[w] := StrToInt(n);
    inc(w);
  end;
  Result := Version.QuadPart;
end;

class function TVersionInfo.VersionToString(Version: int64): string;
var
  v: Large_Integer;
begin
  v.QuadPart := Version;
  Result := Format('%d.%d.%d.%d',
    [v.HighPart shr 16, v.HighPart and $FFFF, v.LowPart shr 16, v.LowPart and $FFFF]);
end;

function TVersionInfo.GetFileVersionWithDots : string;
begin
	result := VersionToString(GetFileVersion);
end;


begin

{

	EditFileVersion.Text := TVersionInfo.VersionToString(Application.Version.FileVersion);
  EditProductVersion.Text := TVersionInfo.VersionToString(Application.Version.ProductVersion);

  for i := Low(PredefinedStrings) to High(PredefinedStrings) do
    AddString(PredefinedStrings[i]);

  for i := 0 to Application.Version.TranslationCount-1 do
    ListViewLanguages.Items.Add.Caption := Application.Version.LanguageNames[i];
}
end.

