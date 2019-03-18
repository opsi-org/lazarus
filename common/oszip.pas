//  Compress and Decompress .zip files.
unit OsZip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper;

// Decompress a zip file while preserving its directory structure.
function UnzipWithDirStruct(File2Unzip, OutputDir :String): Boolean;


implementation

// unzip to the output directory or to the zip file directory(if output Directory is not mentioned) while preserving its directory structure.
function UnzipWithDirStruct(File2Unzip, OutputDir :String): Boolean;
var
  UnzipperObj : TUnZipper;
begin
  Result := False;
  UnzipperObj := TUnZipper.Create;
  if FileExists(File2Unzip) then
  begin
    if (DirectoryExists(OutputDir)) or (OutputDir = '') then
    begin
      try
        UnzipperObj.FileName := File2Unzip;
        if OutputDir = '' then
          UnzipperObj.OutputPath := ExtractFileDir(File2Unzip)
        else
          UnzipperObj.OutputPath := OutputDir;
        UnzipperObj.Examine;
        UnzipperObj.UnZipAllFiles;
        Result := True;
      finally
        UnzipperObj.Free;
      end;
    end;
  end;
end;