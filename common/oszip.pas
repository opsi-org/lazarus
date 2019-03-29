//  Compress and Decompress .zip files.
unit OsZip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper, FileUtil, strutils, LConvEncoding;

// Zip a folder which contains subfolders and files.
function ZipWithDirStruct(sourcepath, searchmask, TargetFile : String): Boolean;

// Decompress a zip file while preserving its directory structure.
function UnzipWithDirStruct(File2Unzip, TargetDir :String): Boolean;

implementation

// zip a folder which contains subfolders and files to the target directory, while preserving its directory structure.
function ZipWithDirStruct(sourcepath, searchmask, TargetFile : String): Boolean;
var
  ZipperObj       : TZipper;
  filecounter     : Integer;
  FileList        : TStringList;
  DiskFileName,
  ArchiveFileName : String;
  TargetDir : string;
  //searchmask : string;
begin
  Result := False;
  TargetDir := ExtractFilePath(TargetFile);
  TargetDir:=includeTrailingPathDelimiter(TargetDir);
  if DirectoryExists(sourcepath) and DirectoryExists(TargetDir) then
  begin
    ZipperObj := TZipper.Create;
    FileList := TStringList.create;
    try
      //ZipperObj.FileName := TargetDir + ExtractFileName(File2Zip) + '.zip';
      ZipperObj.FileName := TargetFile;
      if not FileExists(ZipperObj.FileName) then
      begin
        FileList := FindAllFiles(sourcepath, searchmask);
        for filecounter := 0 to FileList.Count-1 do
        begin
          DiskFileName := FileList.Strings[filecounter];
          ArchiveFileName:=StringReplace(FileList.Strings[filecounter],sourcepath,'',[rfReplaceall]);
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

// unzip to the target directory or to the zip file directory(if target Directory is not mentioned),
// while preserving its directory structure.
function UnzipWithDirStruct(File2Unzip, TargetDir :String): Boolean;
var
  UnzipperObj : TUnZipper;
begin
  Result := False;
  UnzipperObj := TUnZipper.Create;
  if FileExists(File2Unzip) then
  begin
    if (DirectoryExists(TargetDir)) or (TargetDir = '') then
    begin
      try
        UnzipperObj.FileName := File2Unzip;
        if TargetDir = '' then
          UnzipperObj.OutputPath := ExtractFileDir(File2Unzip)
        else
          UnzipperObj.OutputPath := TargetDir;
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
