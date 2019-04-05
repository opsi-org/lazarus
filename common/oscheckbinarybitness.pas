unit oscheckbinarybitness;

{$mode objfpc}
interface
uses
  Classes,
  {$IFDEF LINUX}
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  JwaWindows,
  {$ENDIF WINDOWS}
  SysUtils;



{$IFDEF WINDOWS}
function IsCorrectBitness(const APath: WideString): Boolean;
function GetPEType(const APath: WideString): Byte;
function getBinaryArchitecture(const APath: WideString): string;
{$ENDIF WINDOWS}

implementation

{$IFDEF WINDOWS}
// from http://wiki.freepascal.org/Multiplatform_Programming_Guide#Detecting_bitness_of_external_library_before_loading_it
// (contributed in forum by GetMem):
// http://forum.lazarus.freepascal.org/index.php/topic,36834.msg245859.html#msg245859

function GetPEType(const APath: WideString): Byte;
const
  PE_UNKNOWN = 0; //if the file is not a valid dll, 0 is returned
 // PE_16BIT   = 1; // not supported by this function
  PE_32BIT   = 2;
  PE_64BIT   = 3;
var
  hFile, hFileMap: THandle;
  PMapView: Pointer;
  PIDH: PImageDosHeader;
  PINTH: PImageNtHeaders;
  Base: Pointer;
begin
  Result := PE_UNKNOWN;

  hFile := CreateFileW(PWideChar(APath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE then
  begin
    CloseHandle(hFile);
    Exit;
  end;

  hFileMap  := CreateFileMapping(hFile, nil, PAGE_READONLY, 0, 0, nil);
  if hFileMap = 0 then
  begin
    CloseHandle(hFile);
    CloseHandle(hFileMap);
    Exit;
  end;

  PMapView := MapViewOfFile(hFileMap, FILE_MAP_READ, 0, 0, 0);
  if PMapView = nil then
  begin
    CloseHandle(hFile);
    CloseHandle(hFileMap);
    Exit;
  end;

  PIDH := PImageDosHeader(PMapView);
  if PIDH^.e_magic <> IMAGE_DOS_SIGNATURE then
  begin
    CloseHandle(hFile);
    CloseHandle(hFileMap);
    UnmapViewOfFile(PMapView);
    Exit;
  end;

  Base := PIDH;
  PINTH := PIMAGENTHEADERS(Base + LongWord(PIDH^.e_lfanew));
  if PINTH^.Signature = IMAGE_NT_SIGNATURE then
  begin
    case PINTH^.OptionalHeader.Magic of
      $10b: Result := PE_32BIT;
      $20b: Result := PE_64BIT
    end;
  end;

  CloseHandle(hFile);
  CloseHandle(hFileMap);
  UnmapViewOfFile(PMapView);
end;

//Now, if you compile your application for 32-bit and 64-bit windows, you can check if dll's bitness is same as your application's:
function IsCorrectBitness(const APath: WideString): Boolean;
begin
  {$ifdef CPU32}
    Result := GetPEType(APath) = 2; //the application is compiled as 32-bit, we ask if GetPeType returns 2
  {$endif}
  {$ifdef CPU64}
    Result := GetPEType(APath) = 3; //the application is compiled as 64-bit, we ask if GetPeType returns 3
  {$endif}
end;

function getBinaryArchitecture(const APath: WideString): string;
var
  mypetype : integer;
begin
   mypetype := GetPEType(APath);
   case mypetype of
    2 : result := '32';
    3 : result := '64';
   else result := 'unknown';
   end;
end;
{$ENDIF WINDOWS}

end.

