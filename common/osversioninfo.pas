unit osversioninfo;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,
  {$IFDEF WINDOWS}
  winpeimagereader,
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  elfreader,
  {$ENDIF UNIX}
  osencoding,
  fileinfo;

function getversioninfo : string;

implementation

function getversioninfo : string;
var
  i: integer;
  versionInfo: TFileVersionInfo;
  versionstr: string;
begin
  //from http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company
  versionInfo := TFileVersionInfo.Create(nil);
  try
    //versionInfo.FileName := reencode(ParamStr(0), 'system');
    versionInfo.FileName := ParamStr(0);
    versionInfo.ReadFileInfo;
    result := versionInfo.VersionStrings.Values['FileVersion'];
  finally
    versionInfo.Free;
  end;
end;

end.

