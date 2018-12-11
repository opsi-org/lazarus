program getwinfileversion;

// Read the file version info from a windows exe (pe) and give it to stdout

uses
  fileinfo,
  winpeimagereader, {need this for reading exe info}
  //elfreader, {needed for reading ELF executables}
  SysUtils;

var
  FileVerInfo: TFileVersionInfo;
  version, param: string;

begin
  param := ParamStr(1);
  if FileExists(param) then
  begin
    FileVerInfo := TFileVersionInfo.Create(nil);
    try
      FileVerInfo.FileName := Param;
      FileVerInfo.ReadFileInfo;
      version := FileVerInfo.VersionStrings.Values['FileVersion'];
      if pos(' ',version) > 0 then
        version := copy(version,0,pos(' ',version));
      writeln(trim(version));
    finally
      FileVerInfo.Free;
    end;
  end;
end.
