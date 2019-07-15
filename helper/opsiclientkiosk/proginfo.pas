unit proginfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileInfo;

type

  { TProgramVersion }

  TProgramInfo = Class(TObject)
    private
      MyVersion:String;
      procedure SetVersionFromFileVersionInfo;
    public
      constructor Create;virtual;
      property Version: String read MyVersion;
  end;

  var
    ProgramInfo : TProgramInfo;

implementation


procedure TProgramInfo.SetVersionFromFileVersionInfo;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := ParamStr(0);
    FileVerInfo.ReadFileInfo;
    MyVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;

end;

constructor TProgramInfo.Create;
begin
  inherited;
  SetVersionFromFileVersionInfo;
end;

initialization
  ProgramInfo := TProgramInfo.Create;
end.

