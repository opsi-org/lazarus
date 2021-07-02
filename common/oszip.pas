//  Compress and Decompress .zip files.
unit OsZip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper, FileUtil, strutils,
  {$IFDEF OSLOG}
  oslog,
  {$ENDIF OSLOG}
  LConvEncoding;

// Zip a folder which contains subfolders and files.
function ZipWithDirStruct(sourcepath, searchmask, TargetFile: string): boolean;

// Decompress a zip file while preserving its directory structure.
function UnzipWithDirStruct(File2Unzip, TargetDir: string): boolean;

function getFileListFromZip(zipfilename: string): TStringList;

implementation

function getFileListFromZip(zipfilename: string): TStringList;
  // inspired by http://lazplanet.blogspot.com/2013/05/how-to-get-filesfolders-inside-zip-file.html
var
  myunzip: TUnZipper;
  i: integer;
begin
  try
    try
      myunzip := TUnZipper.Create;
      myunzip.FileName := zipfilename;
      myunzip.Examine;

      Result := TStringList.Create;
      for i := 0 to myunzip.Entries.Count - 1 do
      begin
        Result.Add(myunzip.Entries.Entries[i].ArchiveFileName);
      end;
    except
    end;
  finally
    myunzip.Free;
  end;
end;

// zip a folder which contains subfolders and files to the target directory, while preserving its directory structure.
function ZipWithDirStruct(sourcepath, searchmask, TargetFile: string): boolean;
var
  ZipperObj: TZipper;
  filecounter: integer;
  FileList: TStringList;
  DiskFileName, ArchiveFileName: string;
  TargetDir: string;
  //searchmask : string;
begin
  Result := False;
  TargetDir := ExtractFilePath(TargetFile);
  TargetDir := includeTrailingPathDelimiter(TargetDir);
  if DirectoryExists(sourcepath) and DirectoryExists(TargetDir) then
  begin
    ZipperObj := TZipper.Create;
    FileList := TStringList.Create;
    try
      //ZipperObj.FileName := TargetDir + ExtractFileName(File2Zip) + '.zip';
      ZipperObj.FileName := TargetFile;
      if not FileExists(ZipperObj.FileName) then
      begin
        FileList := FindAllFiles(sourcepath, searchmask);
        for filecounter := 0 to FileList.Count - 1 do
        begin
          DiskFileName := FileList.Strings[filecounter];
          ArchiveFileName := StringReplace(FileList.Strings[filecounter],
            sourcepath, '', [rfReplaceall]);
          ZipperObj.Entries.AddFileEntry(DiskFileName, ArchiveFileName);
          //{$IFDEF OSLOG}
          //   LogDatei.log('zipped: '+ArchiveFileName+' to: '+TargetFile,LLDebug);
          //{$ENDIF OSLOG}
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
function UnzipWithDirStruct(File2Unzip, TargetDir: string): boolean;
var
  UnzipperObj: TUnZipper;
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
