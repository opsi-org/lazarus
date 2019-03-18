//  Compress and Decompress .zip files.
unit OsZip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper;

// Decompress a zip file while preserving its file structure.
function UnzipWithFileStruct(File2Unzip, OutputDir :String): Boolean;


implementation

// unzip to the output directory or to the current directory (if output Directory is not mentioned) while preserving its file structure.
function UnzipWithFileStruct(File2Unzip, OutputDir :String): Boolean;
var
  UnzipperObj : TUnZipper;
begin
  Result := False;
  UnzipperObj := TUnZipper.Create;
  if FileExists(File2Unzip) then
  begin
    try
      UnzipperObj.FileName := File2Unzip;
      if OutputDir = '' then
        UnzipperObj.OutputPath := GetCurrentDir
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
