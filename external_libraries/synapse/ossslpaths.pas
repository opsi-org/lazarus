unit osSSLPaths;
// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Jan Werner
// credits: http://www.opsi.org/credits/

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, osLog;

function GetSSLPath(LibName: string):string;
(*******************************************
  GetSSLPath tries to determine the correct path to the ssl library provided by opsi.
  Gives back the path to the ssl library if the library was found in the filesystem
  otherwise LibName is returned
********************************************)

implementation

{$IFDEF OPSISCRIPT}
uses
  osMain;// needed for logging with startupmessages
{$ENDIF OPSISCRIPT}

function GetSSLPath(LibName: string): string;
var
  i,j : integer;
  Path : TStringList = nil;
  FileList : TStringList = nil;
begin
  //if not Assigned(LogDatei) then LogDatei := TLogInfo.Create;
  {$IFDEF OPSISCRIPT}
  if not Assigned(StartupMessages) then StartupMessages := TStringList.Create;
  {$ENDIF OPSISCRIPT}
  //LogDatei.LogLevel := 7;
  Path := TStringList.Create;
  try
    Path.Clear;
    Path.Sorted := False;
    {$IFDEF WINDOWS}
      Path.Append(ProgramDirectory + LibName);
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
      Path.Append(ProgramDirectory + LibName);
      {$IFDEF OPSISCRIPT}
        Path.Append('/opt/opsi-script/' + LibName);
        Path.Append('/opt/opsi-script-nogui/' + LibName);
        Path.Append('/tmp/opsi-linux-client-agent/files/opsi-script/' + LibName);
        Path.Append('/tmp/opsi-linux-client-agent/files/opsi-script-nogui/' + LibName);
        Path.Append('/tmp/opsi-linux-client-agent/files/opsi/opsi-script/64/opsi-script-nogui/' + LibName);
        Path.Append('/tmp/opsi-linux-client-agent/files/opsi/opsi-script/64/opsi-script/' + LibName);
        Path.Append('/tmp/opsi-linux-client-agent/files/opsi/opsi-script/32/opsi-script-nogui/' + LibName);
        Path.Append('/tmp/opsi-linux-client-agent/files/opsi/opsi-script/32/opsi-script/' + LibName);
      {$ENDIF OPSISCRIPT}
      Path.Append('/usr/bin/' + LibName);
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
      {$IFDEF APP_BUNDLE}
        Path.Append(ProgramDirectory + '../Frameworks/' + LibName);
      {$ELSE}
        Path.Append(ProgramDirectory + LibName);
        Path.Append('/usr/local/lib/' + LibName);
        try
          if (LibName = 'libssl.dylib') then
          begin
            FileList := FindAllFiles('/usr/local/lib/','libssl.*.dylib',false);
          end
          else if (LibName = 'libcrypto.dylib') then
          begin
            FileList := FindAllFiles('/usr/local/lib/','libcrypto.*.dylib',false);
          end;
          if FileList.Count > 0 then
            Path.Append(ExtractFileName(FileList.Strings[FileList.count - 1]));
          Path.Append(ProgramDirectory + '../Frameworks/' + LibName);
        finally
          if Assigned(FileList) then
            FreeAndNil(FileList);
        end;
      {$ENDIF APP_BUNDLE}
    {$ENDIF DARWIn}
    Path.Append(LibName);
    i := 0;
    while (not FileExists(Path.Strings[i])) and (i < (Path.Count-1)) do
    begin
      inc(i);
    end;
    {$IFDEF OPSISCRIPT}
    if (i < (Path.Count-1)) then
      StartupMessages.Append('ssl library path: ' + Path.Strings[i])
    else
    begin
      StartupMessages.Append('WARNING: SSL LIBRARY (' + Path.Strings[i] + ') NOT FOUND IN ANY EXPECTED PATH:');
      for j := 0 to (Path.Count-2) do
        startupMessages.Append('   - ' + Path.Strings[j]);
      StartupMessages.Append('     Neverthless (' + Path.Strings[i] + ') might be found by the OS.');
    end;
    {$ENDIF OPSISCRIPT}
    Result := Path.Strings[i];
  finally
    if Assigned(Path) then
      FreeAndNil(Path);
  end;
end;




end.

