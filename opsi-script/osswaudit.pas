// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel



unit osswaudit;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



interface

uses
{$IFDEF WINDOWS}
  Windows,
  VersionInfoX,
  osregistry,
{$ENDIF}
  LCLIntf,
  SysUtils,
  Classes,
  osconf,
  osfunc,
  oslog,
  oslocale;

function GetSlowInfoCache(const productkey: string; hashkey: string;
  noredirect: string): string;
procedure getSwauditInfoList(const aktkey: string;
  const listtype: string; architecture: string;
  const mylist: TStrings);

implementation

uses
  osmain;

function FileTimeToDateTime(filetime: TFileTime): TDateTime;
  //http://www.delphipraxis.net/940-twin32finddata-tfiletime-nach-tdatetime.html
var
  systime: TSystemTime;
  datetime: TDateTime;
begin
  FileTimeToSystemTime(filetime, systime);
  datetime := EncodeDate(systime.wYear, systime.wMonth, systime.wDay);
  datetime := datetime + EncodeTime(systime.wHour, systime.wMinute,
    systime.wSecond, systime.wMilliseconds);
  Result := datetime;
end;


function GetSlowInfoCache(const productkey: string; hashkey: string;
  noredirect: string): string;
  //http://www.koders.com/cpp/fidEC4BA32C1610A97EE4BD88D7CC5A9CE08BF533F6.aspx
  //HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\App Management\ARPCache
  //http://www.delphipraxis.net/132107-registry-binary-lesen-und-schreiben.html
  // reads slowinfocache
  // productkey is the key below ARPCache
  //hash key may be: hasname, installsize, lastused, usagefrequency, binaryname

type
  //http://groups.google.com/group/alt.comp.freeware/browse_thread/thread/7f93f0a8cd8f532b/99a63e13a12e1f1a
  TSlowInfoCache = record
    cLen: DWORD;
    hasName: longbool;
    size: int64;
    LastUsed: TFileTime;
    Frequency: integer;
    path: array[0..261] of widechar;
  end;

var
  buf: array of byte;
  size: integer;
  i: integer;
  s: string;
  basekey: string;
  myslowinfocache: TSlowInfoCache;
  mytimestamp: TFileTime;
  mydate: tdatetime;
  mydatestr: string;
  Regist: Tuibregistry;

begin
  Result := '';
  if lowercase(noredirect) = 'on' then
    Regist := Tuibregistry.Create(True)
  else
    Regist := Tuibregistry.Create(False);
  basekey := 'SOFTWARE\Microsoft\Windows\CurrentVersion\App Management\ARPCache\' +
    productkey;
  if Regist.OpenExistingKey('HKEY_LOCAL_MACHINE', basekey) then
  begin
    try
      size := Regist.ReadBinary('SlowInfoCache', buf, 0);
      if size <> -1 then
      begin
        SetLength(buf, size);
        Regist.ReadBinary('SlowInfoCache', buf, size);
      end;
      move(buf[0], myslowinfocache, SizeOf(myslowinfocache));
      mytimestamp := myslowinfocache.lastused;
      try
        mydate := FileTimeToDateTime(mytimestamp);
        mydatestr := FormatDateTime('yyyy-mm-dd hh:mm:ss', mydate);
        if lowercase(mydatestr) = 'null' then
          mydatestr := '';
        if lowercase(mydatestr) = '1601-01-01 00:00:00' then
          mydatestr := '';
      except
        mydatestr := '';
      end;
      if lowercase(hashkey) = 'hasname' then
        Result := BoolToStr(myslowinfocache.hasName, True);
      if lowercase(hashkey) = 'installsize' then
        Result := IntToStr(myslowinfocache.size);
      if lowercase(hashkey) = 'lastused' then
        Result := mydatestr;
      if lowercase(hashkey) = 'usagefrequency' then
        Result := IntToStr(myslowinfocache.frequency);
      if myslowinfocache.hasName then
        if lowercase(hashkey) = 'binaryname' then
          Result := myslowinfocache.path;
    finally
      Regist.CloseKey;
      Regist.Free;
      Regist := nil;
    end;
  end;
end;

procedure getSwauditInfoList(const aktkey: string;
  const listtype: string; architecture: string;
  const mylist: TStrings);
var
  winSoftwareId: string;
  displayVersion: string;
  displayName: string;
  uninstallString: string;
  binaryName: string;
  lastUsed: string;
  Language: string;
  installSize: string;
  usageFrequency: string;
  boolNoRedirect: boolean;
  noredirect: string;
  Name: string;
  computernaming: string;
  myver: TVersionInfo;
  myfilename: string;
  mylang: integer;

begin
  //mylist := TStringlist.Create;
  boolNoRedirect := False;
  noredirect := 'off';
  if architecture = 'x64' then
  begin
    boolNoRedirect := True;
    noredirect := 'on';
  end;
  if aktkey <> '' then
  begin
    winSoftwareId := copy(aktkey, LastDelimiter('\', aktkey) + 1, length(aktkey));
    if winSoftwareId <> '' then
    begin
      displayVersion := GetRegistrystringvalue(aktkey, 'DisplayVersion', boolNoRedirect);
      displayName := GetRegistrystringvalue(aktkey, 'displayName', boolNoRedirect);
      uninstallString := GetRegistrystringvalue(aktkey, 'uninstallString', boolNoRedirect);
      if True then
      begin
        installSize := GetSlowInfoCache(winSoftwareId, 'installSize', noredirect);
        usageFrequency := GetSlowInfoCache(winSoftwareId, 'usageFrequency', noredirect);
        binaryName := GetSlowInfoCache(winSoftwareId, 'binaryName', noredirect);
        lastUsed := GetSlowInfoCache(winSoftwareId, 'lastUsed', noredirect);
        if FileExists(binaryName) then
        begin
          try
            myfilename := binaryName;
            myver := TVersionInfo.Create(myfilename);
            mylang := myver.LanguageID[0];
            language := getlangcodeByHexvalueStr('0x' + IntToHex(mylang, 4));
          except
            language := '';
          end;
          if myver <> nil then
            myver.Free;
        end;
        if installSize = '' then
          installSize := '-1';
        if usageFrequency = '' then
          usageFrequency := '-1';
      end
      else
      begin
        installSize := '-1';
        usageFrequency := '-1';
        binaryName := '';
        lastUsed := '';
        language := '';
      end;
      computernaming := osconf.computername;
      if computernaming = '' then
        computernaming := oslog.getComputerName;

      if displayName = '' then
        Name := winSoftwareId
      else
        Name := displayName;
      if listtype = 'softwareHashlist' then
      begin
        mylist.Add('[newmap]');
        mylist.Add('type=AuditSoftware');
        mylist.Add('name=' + Name);
        mylist.Add('version=' + displayVersion);
        mylist.Add('subVersion=');
        mylist.Add('language=');
        mylist.Add('architecture=' + architecture);
        mylist.Add('windowsSoftwareId=' + winSoftwareId);
        mylist.Add('windowsDisplayName=' + displayName);
        mylist.Add('windowsDisplayVersion=' + displayVersion);
        mylist.Add('installSize=' + installSize);
      end;

      if listtype = 'softwareOnClientHashlist' then
      begin
        mylist.Add('[newmap]');
        mylist.Add('type=AuditSoftwareOnClient');
        mylist.Add('name=' + Name);
        mylist.Add('version=' + displayVersion);
        mylist.Add('subVersion=');
        mylist.Add('language=');
        mylist.Add('architecture=' + architecture);
        mylist.Add('clientId=' + computernaming);
        mylist.Add('windowsSoftwareId=' + winSoftwareId);
        mylist.Add('windowsDisplayName=' + displayName);
        mylist.Add('windowsDisplayVersion=' + displayVersion);
        mylist.Add('uninstallString=' + uninstallString);
        mylist.Add('binaryName=' + binaryName);
        mylist.Add('usageFrequency=' + usageFrequency);
        if lastUsed <> '' then
          mylist.Add('lastUsed=' + lastUsed);
      end;
    end;
  end;
end;

(*
if not ("%aktkey%" = "")
  set $winSoftwareId$ = takeString(6,splitString("%aktkey%",'\'))
  if not ($winSoftwareId$ = "")
    set $displayVersion$ = GetRegistrystringvalue ("[%aktkey%] DisplayVersion")
    set $displayName$ = GetRegistrystringvalue ("[%aktkey%] displayName")
    set $uninstallString$ = GetRegistrystringvalue ("[%aktkey%] uninstallString")
    if $details$ = "on"
      set $SlowInfoMap$ = GetSlowInfoMap($winSoftwareId$,"on")
      if count($SlowInfoMap$) > "0"
        set $binaryName$ = getValue("binaryName",$SlowInfoMap$)
        if ($binaryName$ = "") or ($binaryName$ = "NULL")
          set $binaryName$ = ""
        endif
        set $installSize$ = getValue("installSize",$SlowInfoMap$)
        if ($installSize$ = "") or ($installSize$ = "NULL")
          set $installSize$ = "0"
        endif
        set $lastUsed$ = getValue("lastUsed",$SlowInfoMap$)
        if ($lastUsed$ = "") or ($lastUsed$ = "NULL")
          set $lastUsed$ = ""
        endif
        set $usageFrequency$ = getValue("usageFrequency",$SlowInfoMap$)
        if ($usageFrequency$ = "") or ($usageFrequency$ = "NULL")
          set $usageFrequency$ = "0"
        endif
        if not(($binaryName$ = "") or ($binaryName$ = "NULL"))
          if FileExists($binaryName$)
            set $VerInfoMap$ = getFileInfoMap($binaryName$)
            set $langCodeHex$ = DecStrToHexStr(getValue("LanguageID 0",$VerInfoMap$),"4")
            set $Language$ = LangCodeByHex($langCodeHex$)
          endif
        endif
      else
        set $installSize$ = "-1"
        set $usageFrequency$ = "-1"
        set $binaryName$ = ""
        set $lastUsed$ = ""
      endif
    endif
    setloglevel=4
    sub_create_hash4
  endif

if $displayName$ = ""
  set $name$ = $winSoftwareId$
else
  set $name$ = $displayName$
endif
set $version$ = $displayVersion$

;set $ident$ = $name$+";"+$version$+";"+$subversion$+";"+$language$+";"+$architecture$
set $softwareHashlist$ = addtolist($softwareHashlist$, "[newmap]")
set $softwareHashlist$ = addtolist($softwareHashlist$, "type=AuditSoftware")
set $softwareHashlist$ = addtolist($softwareHashlist$, "name="+$name$)
set $softwareHashlist$ = addtolist($softwareHashlist$, "version="+$version$)
set $softwareHashlist$ = addtolist($softwareHashlist$, "subVersion="+$subversion$)
set $softwareHashlist$ = addtolist($softwareHashlist$, "language="+$Language$)
set $softwareHashlist$ = addtolist($softwareHashlist$, "architecture="+$architecture$)
set $softwareHashlist$ = addtolist($softwareHashlist$, "windowsSoftwareId="+$winSoftwareId$)
set $softwareHashlist$ = addtolist($softwareHashlist$, "windowsDisplayName="+$displayName$)
set $softwareHashlist$ = addtolist($softwareHashlist$, "windowsDisplayVersion="+$displayVersion$)
if $details$ = "on"
  set $softwareHashlist$ = addtolist($softwareHashlist$, "installSize="+$installSize$)
endif

set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "[newmap]")
set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "type=AuditSoftwareOnClient")
set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "name="+$name$)
set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "version="+$version$)
set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "subVersion="+$subversion$)
set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "language="+$Language$)
set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "architecture="+$architecture$)
set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "clientId="+$clientId$)
set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "uninstallString="+$uninstallString$)
if $details$ = "on"
  set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "binaryName="+$binaryName$)
  set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "usageFrequency="+$usageFrequency$)
  if not($lastUsed$ = "")
    set $softwareOnClientHashlist$ = addtolist($softwareOnClientHashlist$, "lastUsed="+$lastUsed$)
  endif
endif
*)




end.
