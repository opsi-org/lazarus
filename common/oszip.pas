//  Compress and Decompress .zip files.
unit OsZip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper;

// Compress a folder which contains subfolders and files.
function ZipWithDirStruct(File2Zip, OutputDir : String): Boolean;

// Decompress a zip file while preserving its directory structure.
function UnzipWithDirStruct(File2Unzip, OutputDir :String): Boolean;


implementation

function ZipWithDirStruct(File2Zip, OutputDir : String): Boolean;
var
  ZipperObj       : TZipper;
  filecounter     : Integer;
  FileList        : TStringList;
  DiskFileName,
  fileName,
  ArchiveFileName : String;

begin
  Result := False;
  OutputDir:=includeTrailingPathDelimiter(OutputDir);
  if DirectoryExists(OutputDir) then
  begin
    ZipperObj := TZipper.Create;
    FileList     := TStringList.create;
    try
      ZipperObj.FileName := OutputDir + ExtractFileName(File2Zip) + '.zip';
      if not FileExists(ZipperObj.FileName) then
      begin
        FileList := FindAllFiles(File2Zip);
        for filecounter := 0 to FileList.Count-1 do
        begin
          DiskFileName := FileList.Strings[filecounter];
          ArchiveFileName:=StringReplace(FileList.Strings[filecounter],File2Zip,'',[rfReplaceall]);
          ArchiveFileName:=UnicodeString(ArchiveFileName);
          ArchiveFileName:=UTF8ToCP866(ArchiveFileName);
          ZipperObj.Entries.AddFileEntry(DiskFileName, ArchiveFileName);
        end;
        ZipperObj.ZipAllFiles;
        Result := True;
      end;
    finally
      ZipperObj.Free;
      FileList.Free;
    end;
  end;
end; 

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

end.
