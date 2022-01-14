//  Compress and Decompress .zip files.
unit OsZip;

{$mode objfpc}{$H+}

interface

// about zip progress:
// https://forum.lazarus.freepascal.org/index.php?topic=52499.15

uses
  Classes, SysUtils, Zipper,
  fileutil,
  lazFileUtils, strutils,
  //{$IFDEF OSLOG}
  oslog,
  //{$ENDIF OSLOG}
  LConvEncoding,
  lazutf8,
  osGUIControl;

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
  errorfound: boolean = False;
begin
  Result := False;
  TargetDir := ExtractFilePath(TargetFile);
  TargetDir := includeTrailingPathDelimiter(TargetDir);
  if DirectoryExists(sourcepath) and DirectoryExists(TargetDir) then
  begin
    ZipperObj := TZipper.Create;
    {$IFDEF GUI}
    {$IFDEF OPSISCRIPT}
    FBatchOberflaeche.SetElementVisible(True, eProgressBar); //showProgressBar(True);
    FBatchOberflaeche.SetProgress(0, pPercent);
    ZipperObj.OnProgress := @FBatchOberflaeche.ProgressBarHandler;
    {$ENDIF OPSISCRIPT}
    {$ENDIF GUI}
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
          ArchiveFileName := StringReplace(DiskFileName, sourcepath,
            '', [rfReplaceall]);
          if FileExistsUTF8(DiskFileName) then
          begin
            try
              ZipperObj.Entries.AddFileEntry((DiskFileName), (ArchiveFileName));
              LogDatei.log('ZipWithDirStruct adding entry: ' +
                DiskFileName + ' to: ' + TargetFile, LLDebug2);
            except
              on e: Exception do
              begin
                LogDatei.log('Exception in ZipWithDirStruct while adding entry: ' +
                  DiskFileName, LLError);
                LogDatei.log('Exception in ZipWithDirStruct: ' + e.message, LLError);
              end;
            end;
            LogDatei.log('zip: ' + ArchiveFileName + ' to: ' + TargetFile, LLDebug);
          end
          else
          begin
            LogDatei.log('ZipWithDirStruct file not found: ' + DiskFileName, LLError);
            errorfound := True;
          end;
        end;
        try
          ZipperObj.ZipAllFiles;
          if not errorfound then Result := True;
        except
          on e: Exception do
          begin
            LogDatei.log('Exception in ZipWithDirStruct while ZipAllFiles', LLError);
            LogDatei.log('Exception in ZipWithDirStruct: ' + e.message, LLError);
          end;
        end;
      end;
    finally
      {$IFDEF GUI}
        {$IFDEF OPSISCRIPT}
      FBatchOberflaeche.SetElementVisible(False, eProgressBar);
        {$ENDIF OPSISCRIPT}
        {$ENDIF GUI}
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
  {$IFDEF GUI}
  {$IFDEF OPSISCRIPT}
  FBatchOberflaeche.SetElementVisible(True, eProgressBar); //showProgressBar(True);
  FBatchOberflaeche.SetProgress(0, pPercent);
  UnzipperObj.OnProgress := @FBatchOberflaeche.ProgressBarHandler;
  {$ENDIF OPSISCRIPT}
  {$ENDIF GUI}
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
        {$IFDEF GUI}
        {$IFDEF OPSISCRIPT}
        FBatchOberflaeche.SetElementVisible(False, eProgressBar);
        Sleep(1000);
        {$ENDIF OPSISCRIPT}
        {$ENDIF GUI}
        UnzipperObj.Free;
      end;
    end;
  end;
end;


end.
