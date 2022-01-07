unit osfuncwin3;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel


// inspiration from:
// http://fox.wikis.com/wc.dll?Wiki~GetWindowsVersion
// http://stackoverflow.com/questions/1268178/how-to-check-in-delphi-the-os-version-windows-7-or-server-2008-r2
// https://msdn.microsoft.com/en-us/library/windows/desktop/ms724358%28v=vs.85%29.aspx

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  JwaWinnt,
  jwawinbase,
  JwaWindows,
    Windows;
//,  DSiWin32;

var
  GetProductInfo: function(dwOSMajorVersion, dwOSMinorVersion,
  dwSpMajorVersion, dwSpMinorVersion: DWORD;
  var pdwReturnedProductType: DWORD): BOOL stdcall = nil;

var
  GetNativeSystemInfo: function(var SysInfo: TSystemInfo): BOOL stdcall = nil;

const
  PRODUCT_UNDEFINED = $000000000; // An unknown product
  PRODUCT_ULTIMATE = $000000001; // Ultimate Edition
  PRODUCT_HOME_BASIC = $000000002; // Home Basic Edition
  PRODUCT_HOME_PREMIUM = $000000003; // Home Premium Edition
  PRODUCT_ENTERPRISE = $000000004; // Enterprise Edition
  PRODUCT_HOME_BASIC_N = $000000005; // Home Basic Edition
  PRODUCT_BUSINESS = $000000006; // Business Edition
  PRODUCT_STANDARD_SERVER = $000000007;
  // Server Standard Edition (full installation)
  PRODUCT_DATACENTER_SERVER = $000000008;
  // Server Datacenter Edition (full installation)
  PRODUCT_SMALLBUSINESS_SERVER = $000000009; // Small Business Server
  PRODUCT_ENTERPRISE_SERVER = $00000000A;
  // Server Enterprise Edition (full installation)
  PRODUCT_STARTER = $00000000B; // Starter Edition
  PRODUCT_DATACENTER_SERVER_CORE = $00000000C;
  // Server Datacenter Edition (core installation)
  PRODUCT_STANDARD_SERVER_CORE = $00000000D;
  // Server Standard Edition (core installation)
  PRODUCT_ENTERPRISE_SERVER_CORE = $00000000E;
  // Server Enterprise Edition (core installation)
  PRODUCT_ENTERPRISE_SERVER_IA64 = $00000000F;
  // Server Enterprise Edition for Itanium-based Systems
  PRODUCT_BUSINESS_N = $000000010; // Business Edition
  PRODUCT_WEB_SERVER = $000000011;
  // Web Server Edition (full installation)
  PRODUCT_CLUSTER_SERVER = $000000012;
  // Cluster Server Edition
  PRODUCT_HOME_SERVER = $000000013; // Home Server Edition
  PRODUCT_STORAGE_EXPRESS_SERVER = $000000014;
  // Storage Server Express Edition
  PRODUCT_STORAGE_STANDARD_SERVER = $000000015;
  // Storage Server Standard Edition
  PRODUCT_STORAGE_WORKGROUP_SERVER = $000000016;
  // Storage Server Workgroup Edition
  PRODUCT_STORAGE_ENTERPRISE_SERVER = $000000017;
  // Storage Server Enterprise Edition
  PRODUCT_SERVER_FOR_SMALLBUSINESS = $000000018;
  // Server for Small Business Edition
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM = $000000019;
  // Small Business Server Premium Edition
  PRODUCT_HOME_PREMIUM_N = $00000001A; // Home Premium Edition
  PRODUCT_ENTERPRISE_N = $00000001B; // Enterprise Edition
  PRODUCT_ULTIMATE_N = $00000001C; // Ultimate Edition
  PRODUCT_WEB_SERVER_CORE = $00000001D;
  // Web Server Edition (core installation)
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT = $00000001E;
  // Windows Essential Business Server Management Server
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY = $00000001F;
  // Windows Essential Business Server Security Server
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING = $000000020;
  // Windows Essential Business Server Messaging Server
  PRODUCT_SERVER_FOUNDATION = $000000021; // Server Foundation
  PRODUCT_HOME_PREMIUM_SERVER = $000000022;
  // PRODUCT_HOME_PREMIUM_SERVER
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V = $000000023;
  // PRODUCT_SERVER_FOR_SMALLBUSINESS_V
  PRODUCT_STANDARD_SERVER_V = $000000024;
  // Server Standard Edition without Hyper-V (full installation)
  PRODUCT_DATACENTER_SERVER_V = $000000025;
  // Server Datacenter Edition without Hyper-V (full installation)
  PRODUCT_ENTERPRISE_SERVER_V = $000000026;
  // Server Enterprise Edition without Hyper-V (full installation)
  PRODUCT_DATACENTER_SERVER_CORE_V = $000000027;
  // Server Datacenter Edition without Hyper-V (core installation)
  PRODUCT_STANDARD_SERVER_CORE_V = $000000028;
  // Server Standard Edition without Hyper-V (core installation)
  PRODUCT_ENTERPRISE_SERVER_CORE_V = $000000029;
  // Server Enterprise Edition without Hyper-V (core installation)
  PRODUCT_PROFESSIONAL = $000000030; // Windows Pro
  PRODUCT_PROFESSIONAL_N = $000000031; // Windows Pro N
  PRODUCT_SB_SOLUTION_SERVER = $000000032;
  // PRODUCT_SB_SOLUTION_SERVER
  PRODUCT_SERVER_FOR_SB_SOLUTIONS = $000000033;
  // PRODUCT_SERVER_FOR_SB_SOLUTIONS
  PRODUCT_STANDARD_SERVER_SOLUTIONS = $000000034;
  // PRODUCT_STANDARD_SERVER_SOLUTIONS
  PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE = $000000035;
  // PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE
  PRODUCT_SB_SOLUTION_SERVER_EM = $000000036;
  // PRODUCT_SB_SOLUTION_SERVER_EM
  PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM = $000000037;
  // PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM
  PRODUCT_SOLUTION_EMBEDDEDSERVER = $000000038;
  // PRODUCT_SOLUTION_EMBEDDEDSERVER
  PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE = $000000039;
  // PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE = $00000003F;
  // PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT = $00000003B;
  // PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL = $00000003C;
  // PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC = $00000003D;
  // PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC = $00000003E;
  // PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC
  PRODUCT_CLUSTER_SERVER_V = $000000040;
  // PRODUCT_CLUSTER_SERVER_V
  PRODUCT_EMBEDDED = $000000041; // PRODUCT_EMBEDDED
  PRODUCT_STARTER_E = $000000042; // PRODUCT_STARTER_E
  PRODUCT_HOME_BASIC_E = $000000043; // PRODUCT_HOME_BASIC_E
  PRODUCT_HOME_PREMIUM_E = $000000044;
  // PRODUCT_HOME_PREMIUM_E
  PRODUCT_PROFESSIONAL_E = $000000045;
  // PRODUCT_PROFESSIONAL_E
  PRODUCT_ENTERPRISE_E = $000000046;
  // Windows 10 Enterprise E
  PRODUCT_ULTIMATE_E = $000000047; // PRODUCT_ULTIMATE_E
  PRODUCT_ENTERPRISE_EVALUATION = $000000048;
  // Windows 10 Enterprise Evaluation
  PRODUCT_ENTERPRISE_N_EVALUATION = $000000054;
  // Windows 10 Enterprise N Evaluation
  PRODUCT_CORE_N = $000000062; // Windows 10 Home N
  PRODUCT_CORE_COUNTRYSPECIFIC = $000000063; // Windows 10 Home China
  PRODUCT_CORE_SINGLELANGUAGE = $000000064;
  // Windows 10 Home Single Language
  PRODUCT_CORE = $000000065; // Windows 10 Home
  PRODUCT_EDUCATION = $000000079; // Windows 10 Education
  PRODUCT_EDUCATION_N = $00000007A; // Windows 10 Education
  PRODUCT_ENTERPRISE_S = $00000007D;
  // Windows 10 Enterprise 2015 LTSB
  PRODUCT_ENTERPRISE_S_N = $00000007E;
  // Windows 10 Enterprise 2015 LTSB N
  PRODUCT_ENTERPRISE_S_EVALUATION = $000000081;
  // Windows 10 Enterprise 2015 LTSB Evaluation
  PRODUCT_ENTERPRISE_S_N_EVALUATION = $000000082;
// Windows 10 Enterprise 2015 LTSB N Evaluation



function GetWinDirectory: string;
function GetWinSystemDirectory: string;
function GetSystemDefaultLocale(const typeOfValue: DWord): string;
function OSGetProductInfoNum: cardinal;
function getProductInfoStrByNum(num: cardinal): string;
function GetOSVersionEx(var lpVersionInformation: TOSVersionInfoEx): LPBOOL;
  stdcall; external kernel32 Name 'GetVersionExA';
function GetSystemOSVersionInfoEx(const typeOfValue: string): string;
function GetNTVersionMajor: Dword;
function GetNTVersionMinor: Dword;


implementation

function GetNTVersionMajor: Dword;
var
  vi: TOSVersionInfo;

begin
  Result := 0;
  vi.dwOSVersionInfoSize := SizeOf(vi);
  if not GetVersionEx(vi) then
    raise Exception.Create('Fehler ' + IntToStr(GetLastError) +
      ' ("' + syserrormessage(GetLastError) + '")')
  else
    Result := vi.dwMajorVersion;
end;


function GetNTVersionMinor: Dword;
var
  vi: TOSVersionInfo;

begin
  Result := 0;
  vi.dwOSVersionInfoSize := SizeOf(vi);
  if not GetVersionEx(vi) then
    raise Exception.Create('Fehler ' + IntToStr(GetLastError) +
      ' ("' + syserrormessage(GetLastError) + '")')
  else
    Result := vi.dwMinorVersion;
end;


function GetWinDirectory: string;
var
  buffer: PChar;
  size: word = 0;

begin
  Buffer := StrAlloc(144);
  Size := GetWindowsDirectory(Buffer, 144);
  Result := StrPas(Buffer);
  if Result[length(Result)] <> PathDelim then
    Result := Result + PathDelim;
end;


function GetWinSystemDirectory: string;
var
  path: array [0..500] of char;
  size: word = 0;
begin
  // size 144 did not work after changed to laz1.6/FPC3
  // changed to 500 and mixed with DSiGetSystemDirectory
  Result := '';
  FillByte (path,SizeOf(path),0);
  Size := GetSystemDirectory(@path, length(path));
  Result := StrPas(PChar(@path));
  if Result[length(Result)] <> PathDelim then
    Result := Result + PathDelim;
end;


(*
function GetWinSystemDirectory: string;
  var
    path: array [0..500] of char;
  begin
    if GetSystemDirectoryA(@path, 500) <> 0 then
      Result := StrPas(PChar(@path))
    else
      Result := '';
  end; { DSiGetSystemDirectory }
*)

function GetSystemDefaultLocale(const typeOfValue: DWord): string;
  // possible values: cf. "Locale Types" in windows.pas
var
  buffer: PChar;
  size: word = 0;
  usedsize: word = 0;

begin
  Result := '';
  size := 101;
  Buffer := StrAlloc(101);
  usedsize := GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, typeOfValue, buffer, size);
  if usedsize <> 0 then
    Result := StrPas(Buffer);
end;


  (* http://www.delphi-treff.de/tipps/system/wiki/Windows-Version%20ermitteln/
     http://msdn.microsoft.com/en-us/library/ms724833%28VS.85%29.aspx
     http://msdn.microsoft.com/en-us/library/dd419805.aspx  *)
function GetOSVersionInfoEx: TOSVersionInfoEx;
var
  OSVersionInfo: TOSVersionInfo absolute Result;
  Done: BOOL;
begin
  FillChar(Result, SizeOf(Result), #0);
  Done := False;
  try
    Result.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
    Done := GetVersionEx(@Result);
  except
  end;
  if not (Done) then
  begin
    try
      FillChar(Result, SizeOf(Result), #0);
      Result.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
      Done := GetVersionEx(OSVersionInfo);
    except
    end;
  end;
end;


function GetSystemOSVersionInfoEx(const typeOfValue: string): string;
const
  (* this constants are informations
  VER_NT_WORKSTATION = $00000001;
  VER_NT_DOMAIN_CONTROLLER = $00000002;
  VER_NT_SERVER = $00000003;

  VER_SUITE_SMALLBUSINESS = $00000001;
  VER_SUITE_ENTERPRISE = $00000002;
  VER_SUITE_BACKOFFICE = $00000004;
  VER_SUITE_COMMUNICATIONS = $00000008;
  VER_SUITE_TERMINAL = $00000010;
  VER_SUITE_SMALLBUSINESS_RESTRICTED = $00000020;
  VER_SUITE_EMBEDDEDNT = $00000040;
  VER_SUITE_DATACENTER = $00000080;
  VER_SUITE_SINGLEUSERTS = $00000100;
  VER_SUITE_PERSONAL = $00000200;
  VER_SUITE_BLADE = $00000400;

  SM_TABLETPC = 86;
  SM_MEDIACENTER = 87;
  SM_STARTER = 88;
  *)
  SM_SERVERR2 = 89;

var
  OSInfo: TOSVersionInfoEx;
begin
  Result := 'unknown';
  OSInfo := GetOSVersionInfoEx;
  if typeOfValue = 'major_version' then
    Result := IntToStr(OSInfo.dwMajorVersion);
  if typeOfValue = 'minor_version' then
    Result := IntToStr(OSInfo.dwMinorVersion);
  if typeOfValue = 'build_number' then
    Result := IntToStr(OSInfo.dwBuildNumber);
  if typeOfValue = 'platform_id' then
    Result := IntToStr(OSInfo.dwPlatformId);
  if typeOfValue = 'csd_version' then
    Result := OSInfo.szCSDVersion;
  if typeOfValue = 'service_pack_major' then
    Result := IntToStr(OSInfo.wServicePackMajor);
  if typeOfValue = 'service_pack_minor' then
    Result := IntToStr(OSInfo.wServicePackMinor);
  if typeOfValue = 'suite_mask' then
    Result := IntToStr(OSInfo.wSuiteMask);
  if typeOfValue = 'product_type_nr' then
    Result := IntToStr(OSInfo.wProductType);
  if typeOfValue = '2003r2' then
    if GetSystemMetrics(SM_SERVERR2) <> 0 then
      Result := 'true'
    else
      Result := 'false';
end;


function getProductInfoStrByNum(num: cardinal): string;
begin
  case num of
    $000000000: Result := 'An unknown product';
    $000000001: Result := 'Ultimate Edition"';
    $000000002: Result := 'Home Basic Edition';
    $000000003: Result := 'Home Premium Edition';
    $000000004: Result := 'Enterprise Edition';
    $000000005: Result := 'Home Basic Edition';
    $000000006: Result := 'Business Edition';
    $000000007: Result := 'Server Standard Edition (full installation)';
    $000000008: Result := 'Server Datacenter Edition (full installation)';
    $000000009: Result := 'Small Business Server';
    $00000000A: Result := 'Server Enterprise Edition (full installation)';
    $00000000B: Result := 'Starter Edition';
    $00000000C: Result := 'Server Datacenter Edition (core installation)';
    $00000000D: Result := 'Server Standard Edition (core installation)';
    $00000000E: Result := 'Server Enterprise Edition (core installation)';
    $00000000F: Result := 'Server Enterprise Edition for Itanium-based Systems';
    $000000010: Result := 'Business Edition';
    $000000011: Result := 'Web Server Edition (full installation)';
    $000000012: Result := 'Cluster Server Edition';
    $000000013: Result := 'Home Server Edition';
    $000000014: Result := 'Storage Server Express Edition';
    $000000015: Result := 'Storage Server Standard Edition';
    $000000016: Result := 'Storage Server Workgroup Edition';
    $000000017: Result := 'Storage Server Enterprise Edition';
    $000000018: Result := 'Server for Small Business Edition';
    $000000019: Result := 'Small Business Server Premium Edition';
    $00000001A: Result := 'PRODUCT_HOME_PREMIUM_N';
    $00000001B: Result := 'PRODUCT_ENTERPRISE_N';
    $00000001C: Result := 'PRODUCT_ULTIMATE_N';
    $00000001D: Result := 'PRODUCT_WEB_SERVER_CORE';
    $00000001E: Result := 'Windows Essential Business Server Management Server';
    $00000001F: Result := 'Windows Essential Business Server Security Server';
    $000000020: Result := 'Windows Essential Business Server Messaging Server';
    $000000021: Result := 'Server Foundation';
    $000000022: Result := 'PRODUCT_HOME_PREMIUM_SERVER';
    $000000023: Result := 'PRODUCT_SERVER_FOR_SMALLBUSINESS_V';
    $000000024: Result := 'Server Standard Edition without Hyper-V (full installation)'
      ;
    $000000025: Result :=
        'Server Datacenter Edition without Hyper-V (full installation)';
    $000000026: Result :=
        'Server Enterprise Edition without Hyper-V (full installation)';
    $000000027: Result :=
        'Server Datacenter Edition without Hyper-V (core installation)';
    $000000028: Result := 'Server Standard Edition without Hyper-V (core installation)'
      ;
    $000000029: Result :=
        'Server Enterprise Edition without Hyper-V (core installation)';
    $000000030: Result := 'PRODUCT_PROFESSIONAL';
    $000000031: Result := 'PRODUCT_PROFESSIONAL_N';
    $000000032: Result := 'PRODUCT_SB_SOLUTION_SERVER';
    $000000033: Result := 'PRODUCT_SERVER_FOR_SB_SOLUTIONS';
    $000000034: Result := 'PRODUCT_STANDARD_SERVER_SOLUTIONS';
    $000000035: Result := 'PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE';
    $000000036: Result := 'PRODUCT_SB_SOLUTION_SERVER_EM';
    $000000037: Result := 'PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM';
    $000000038: Result := 'PRODUCT_SOLUTION_EMBEDDEDSERVER';
    $000000039: Result := 'PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE';
    $00000003F: Result := 'PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE';
    $00000003B: Result := 'PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT';
    $00000003C: Result := 'PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL';
    $00000003D: Result := 'PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC';
    $00000003E: Result := 'PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC';
    $000000040: Result := 'PRODUCT_CLUSTER_SERVER_V';
    $000000041: Result := 'PRODUCT_EMBEDDED';
    $000000042: Result := 'PRODUCT_STARTER_E';
    $000000043: Result := 'PRODUCT_HOME_BASIC_E';
    $000000044: Result := 'PRODUCT_HOME_PREMIUM_E';
    $000000045: Result := 'PRODUCT_PROFESSIONAL_E';
    $000000046: Result := 'PRODUCT_ENTERPRISE_E';
    $000000047: Result := 'PRODUCT_ULTIMATE_E';
    $000000048: Result := 'PRODUCT_ENTERPRISE_EVALUATION';
    $000000054: Result := 'PRODUCT_ENTERPRISE_N_EVALUATION';
    $000000062: Result := 'PRODUCT_CORE_N';
    $000000063: Result := 'PRODUCT_CORE_COUNTRYSPECIFIC';
    $000000064: Result := 'PRODUCT_CORE_SINGLELANGUAGE';
    $000000065: Result := 'PRODUCT_CORE';
    $000000079: Result := 'PRODUCT_EDUCATION';
    $00000007A: Result := 'PRODUCT_EDUCATION_N';
    $00000007D: Result := 'Windows Enterprise 2015 LTSB';
    $00000007E: Result := 'Windows Enterprise 2015 LTSB N';
    $000000081: Result := 'Windows Enterprise 2015 LTSB Evaluation';
    $000000082: Result := 'Windows Enterprise 2015 LTSB N Evaluation';
    else
      Result := 'undefined';
  end;
end;

function OSGetProductInfoNum: cardinal;
var
  pdwReturnedProductType: DWORD;
begin
  Result := 0;
  pdwReturnedProductType := 0;
  if Assigned(GetProductInfo) then
  begin
    GetProductInfo(StrToInt(GetSystemOSVersionInfoEx('major_version')),
      StrToInt(GetSystemOSVersionInfoEx('minor_version')),
      0,
      0,
      pdwReturnedProductType);
  end;
  Result := pdwReturnedProductType;
end;



initialization
  @GetProductInfo := GetProcAddress(GetModuleHandle('KERNEL32.DLL'),
    'GetProductInfo');
  @GetNativeSystemInfo := GetProcAddress(GetModuleHandle('KERNEL32.DLL'),
    'GetNativeSystemInfo');

end.
