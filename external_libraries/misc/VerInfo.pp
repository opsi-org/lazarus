// from https://forum.lazarus.freepascal.org/index.php/topic,41852.0.html
// Author ASerge:  https://forum.lazarus.freepascal.org/index.php?action=profile;u=59545
// Downloaded from https://forum.lazarus.freepascal.org/index.php?action=dlattach;topic=41852.0;attach=27447

unit VerInfo;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

interface

uses Windows, SysUtils;

type
  TLongVersion = Int64Rec;

  TVersionInfo = class(TObject)
  private
    FBuffer: Pointer;
    FFileName: string;
    function GetFileLongVersion: TLongVersion;
    function GetFixedFileInfo: PVSFixedFileInfo;
    function GetProductLongVersion: TLongVersion;
    function GetVerValue(const Name: string): string;
    procedure ReadVersionInfo;
    procedure SetFileName(const AValue: string);
  public
    constructor Create(const AFileName: string = ''); // '' - self exe name
    destructor Destroy; override;
    property FileName: string read FFileName write SetFileName;
    property FixedFileInfo: PVSFixedFileInfo read GetFixedFileInfo;
    property FileLongVersion: TLongVersion read GetFileLongVersion;
    property ProductLongVersion: TLongVersion read GetProductLongVersion;
    property Values[const Name: string]: string read GetVerValue; default;
  end;

const
  // Known names for exe resource
  CviComments = 'Comments';
  CviCompanyName = 'CompanyName';
  CviFileDescription = 'FileDescription';
  CviFileVersion = 'FileVersion';
  CviInternalName = 'InternalName';
  CviLegalCopyright = 'LegalCopyright';
  CviLegalTrademarks = 'LegalTrademarks';
  CviOriginalFilename = 'OriginalFilename';
  CviProductVersion = 'ProductVersion';
  CviProductName = 'ProductName';
  CviSpecialBuild = 'SpecialBuild';
  CviPrivateBuild = 'PrivateBuild';

implementation

function VerGetValue(Buffer: Pointer; const BlockName: string): Pointer;
var
  Dummy: UINT;
  BlockNameW: UnicodeString;
begin
  BlockNameW := UTF8Decode(BlockName);
  if not VerQueryValueW(Buffer, PWideChar(BlockNameW), @Result, @Dummy) then
    Result := nil;
end;


constructor TVersionInfo.Create(const AFileName: string);
begin
  inherited Create;
  SetFileName(AFileName);
end;

destructor TVersionInfo.Destroy;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  inherited;
end;

function TVersionInfo.GetFileLongVersion: TLongVersion;
var
  P: PVSFixedFileInfo;
begin
  P := GetFixedFileInfo;
  if P = nil then
  begin
    Result.Hi := 0;
    Result.Lo := 0;
  end
  else
  begin
    Result.Hi := P^.dwFileVersionMS;
    Result.Lo := P^.dwFileVersionLS;
  end;
end;

function TVersionInfo.GetFixedFileInfo: PVSFixedFileInfo;
begin
  Result := VerGetValue(FBuffer, '\');
end;

function TVersionInfo.GetProductLongVersion: TLongVersion;
var
  P: PVSFixedFileInfo;
begin
  P := GetFixedFileInfo;
  if P = nil then
  begin
    Result.Hi := 0;
    Result.Lo := 0;
  end
  else
  begin
    Result.Hi := P^.dwProductVersionMS;
    Result.Lo := P^.dwProductVersionLS;
  end;
end;

function TVersionInfo.GetVerValue(const Name: string): string;
type
  TTranslateInfo = packed record
    LangId, CodePage: word;
  end;
const
  Default: TTranslateInfo = (LangId: $0409; CodePage: 1252);
var
  PLang: ^TTranslateInfo;
  PValue: PChar;
  BlockName: string;
begin
  Result := '';
  PLang := VerGetValue(FBuffer, '\VarFileInfo\Translation');
  if PLang = nil then
    PLang := @Default;
  FmtStr(BlockName, '\StringFileInfo\%.4x%.4x\%s',
    [PLang^.LangId, PLang^.CodePage, Name]);
  PValue := VerGetValue(FBuffer, BlockName);
  if PValue <> nil then
    Result := UTF8Encode(WideString(PWideChar(PValue)));
end;

procedure TVersionInfo.ReadVersionInfo;
var
  Size: DWORD;
  Dummy: DWORD;
  FileNameW: UnicodeString;
begin
  FileNameW := UTF8Decode(FFileName);
  Size := GetFileVersionInfoSizeW(Pointer(FileNameW), @Dummy);
  if Size = 0 then
    RaiseLastOSError;
  GetMem(FBuffer, Size);
  if not GetFileVersionInfoW(Pointer(FileNameW), 0, Size, FBuffer) then
    RaiseLastOSError;
end;

procedure TVersionInfo.SetFileName(const AValue: string);
begin
  if AValue <> FFileName then
  begin
    FFileName := AValue;
    if FBuffer <> nil then
    begin
      FreeMem(FBuffer);
      FBuffer := nil;
    end;
  end;
  if FFileName = '' then
    FFileName := ParamStr(0);
  ReadVersionInfo;
end;

end.
