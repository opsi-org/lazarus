unit lazproginfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileInfo;

type

  { TProgramVersion }

  { TProgramInfo }

  TProgramInfo = Class(TObject)
    private
      MyVersion:String;
      MyInternalName: String;
      procedure SetVersionInfoFromFileVersionInfo;
    public
      constructor Create;virtual;
      destructor Destroy;override;
      property Version: String read MyVersion;
      property InternalName: String read MyInternalName;
  end;

  var
    ProgramInfo : TProgramInfo;

implementation


procedure TProgramInfo.SetVersionInfoFromFileVersionInfo;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    MyInternalName := FileVerInfo.VersionStrings.Values['InternalName'];
    MyVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;

end;

constructor TProgramInfo.Create;
begin
  inherited;
  SetVersionInfoFromFileVersionInfo;
end;

destructor TProgramInfo.Destroy;
begin
  inherited Destroy;
end;

initialization
  ProgramInfo := TProgramInfo.Create;

finalization
  ProgramInfo.Free;
end.

