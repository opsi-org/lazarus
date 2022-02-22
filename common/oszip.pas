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
  {$IFDEF GUI}
  osGUIControl,
  {$ENDIF GUI}
  lazutf8;

type

  {TProgressDisplayer}
  TProgressDisplayer = class(TObject)
  private
    FNewProgress: integer;
  public
    procedure DisplayProgress;
    property NewProgress: integer write FNewProgress;
  end;

  {TUnzipperWithProgressHandler}
  TUnzipperWithProgressHandler = class(TUnZipper)
  private
    FProgressDisplayer: TProgressDisplayer;
  public
    constructor Create;
    procedure HandleProgressBar(Sender: TObject; const ATotPos, ATotSize: int64);
  end;

  {TZipperWithProgressHandler}
  TZipperWithProgressHandler = class(TZipper)
  private
    FProgressDisplayer: TProgressDisplayer;
    FTotalPosInFile: integer;
    FTotalSizeOfCurrentFile: integer;
    FFileNumber: integer;
    FSourcePath: string;
    // FATotSize, FATotPos as in TUnzipperWithProgressHandler.HandleProgressBar:
    FATotSize: integer;
    FATotPos: integer;
  public
    constructor Create;
    procedure CalculateOverallProgress(const FileToZip: string; const Pct: double);
    procedure CalculateOverallProgressWhileZipping(const Pct: double);
    procedure ResetTotalPosition;
    procedure IncreaseTotalPosition;
    procedure CheckEndOfFile(const Pct: double);
    procedure DisplayProgress;
    procedure HandleProgressBar(Sender: TObject; const Pct: double);
    property SourcePath: string write FSourcePath;
    property ATotSize: integer write FATotSize;
  end;


// Zip a folder which contains subfolders and files.
function ZipWithDirStruct(sourcepath, searchmask, TargetFile: string): boolean;

// Decompress a zip file while preserving its directory structure.
function UnzipWithDirStruct(File2Unzip, TargetDir: string): boolean;

function getFileListFromZip(zipfilename: string): TStringList;

implementation

{TProgressDisplayer}

procedure TProgressDisplayer.DisplayProgress;
begin
    {$IFDEF GUI}
    {$IFDEF OPSISCRIPT}
    FBatchOberflaeche.SetProgress(FNewProgress, pPercent);
    {$ENDIF OPSISCRIPT}
    {$ENDIF GUI}
end;

{TUnzipperWithProgressHandler}

constructor TUnzipperWithProgressHandler.Create;
begin
  inherited Create;
  FProgressDisplayer := TProgressDisplayer.Create;
end;

procedure TUnzipperWithProgressHandler.HandleProgressBar(Sender: TObject;
  const ATotPos, ATotSize: int64);
begin
  // ATotSize is total size of the zip file in bytes
  // ATotPos says which byte you are working on and therefore counts how many bytes you already worked on
  FProgressDisplayer.NewProgress := round(100 * (ATotPos / ATotSize));

  FProgressDisplayer.DisplayProgress;
end;

{TZipperWithProgressHandler}

constructor TZipperWithProgressHandler.Create;
begin
  inherited Create;
  FProgressDisplayer := TProgressDisplayer.Create;
  FATotPos := 0;
  FFileNumber := 0;
end;

procedure TZipperWithProgressHandler.CalculateOverallProgress(const FileToZip: string; const Pct: double);
begin
  FTotalSizeOfCurrentFile := FileSize(FileToZip);
  FTotalPosInFile := round(Pct * FTotalSizeOfCurrentFile / 100);
  FProgressDisplayer.NewProgress :=
    round(100 * ((FATotPos + FTotalPosInFile) / FATotSize));
end;

procedure TZipperWithProgressHandler.CalculateOverallProgressWhileZipping(const Pct: double);
begin
  CalculateOverallProgress(FSourcePath +
    Entries.Entries[FFileNumber].ArchiveFileName, Pct);
end;

procedure TZipperWithProgressHandler.ResetTotalPosition;
begin
    FATotPos := 0;
end;

procedure TZipperWithProgressHandler.IncreaseTotalPosition;
begin
    FATotPos += FTotalSizeOfCurrentFile;
end;

procedure TZipperWithProgressHandler.CheckEndOfFile(const Pct: double);
begin
  if Pct = 100 then
  begin
    Inc(FFileNumber);
    IncreaseTotalPosition;
  end;
end;

procedure TZipperWithProgressHandler.DisplayProgress;
begin
  FProgressDisplayer.DisplayProgress;
end;

procedure TZipperWithProgressHandler.HandleProgressBar(Sender: TObject;
  const Pct: double);
begin
  // For TZipper there is no OnProgressEx and therefore we have to use OnProgress,
  // which works with the percentage (Pct) of progress for each single file.
  // Note that for small files, Pct becomes only 0 and 100.

  // At end of all files OnProgress is executed but there are no files left
  if FFileNumber < Entries.Count then
  begin
    CalculateOverallProgressWhileZipping(Pct);
    DisplayProgress;
    CheckEndOfFile(Pct);
  end;
end;



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
  ZipperObj: TZipperWithProgressHandler;
  filecounter: integer;
  FileList: TStringList;
  DiskFileName, ArchiveFileName: string;
  TargetDir: string;
  //searchmask : string;
  errorfound: boolean = False;
  ATotSize: integer = 0;
begin
  Result := False;
  TargetDir := ExtractFilePath(TargetFile);
  TargetDir := includeTrailingPathDelimiter(TargetDir);
  if DirectoryExists(sourcepath) and DirectoryExists(TargetDir) then
  begin
    ZipperObj := TZipperWithProgressHandler.Create;
    {$IFDEF GUI}
    {$IFDEF OPSISCRIPT}
    FBatchOberflaeche.SetElementVisible(True, eProgressBar); //showProgressBar(True);
    FBatchOberflaeche.SetProgress(0, pPercent);
    FBatchOberflaeche.SetMessageText('Prepare zipping', mInfo);
    ZipperObj.OnProgress := @ZipperObj.HandleProgressBar;
    {$ENDIF OPSISCRIPT}
    {$ENDIF GUI}
    FileList := TStringList.Create;
    try
      //ZipperObj.FileName := TargetDir + ExtractFileName(File2Zip) + '.zip';
      ZipperObj.FileName := TargetFile;
      if not FileExists(ZipperObj.FileName) then
      begin
        FileList := FindAllFiles(sourcepath, searchmask);

        {$IFDEF GUI}
        {$IFDEF OPSISCRIPT}
        // count total number of bytes to zip (for showing a meaningful progressbar)
        for filecounter := 0 to FileList.Count - 1 do
          ATotSize += FileSize(FileList[filecounter]);
        ZipperObj.ATotSize := ATotSize;
        // ZipperObj needs sourcepath to calculate the size of each single file to zip
        ZipperObj.SourcePath := sourcepath;
        {$ENDIF OPSISCRIPT}
        {$ENDIF GUI}

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
            {$IFDEF GUI}
            {$IFDEF OPSISCRIPT}
            ZipperObj.CalculateOverallProgress(FileList[filecounter], 100);
            ZipperObj.DisplayProgress;
            ZipperObj.IncreaseTotalPosition;
            {$ENDIF OPSISCRIPT}
            {$ENDIF GUI}
          end
          else
          begin
            LogDatei.log('ZipWithDirStruct file not found: ' + DiskFileName, LLError);
            errorfound := True;
          end;
        end;
        try
          {$IFDEF GUI}
          {$IFDEF OPSISCRIPT}
          ZipperObj.ResetTotalPosition;
          FBatchOberflaeche.SetMessageText('Zipping files', mInfo);
          {$ENDIF OPSISCRIPT}
          {$ENDIF GUI}
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
      Sleep(500);
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
  UnzipperObj: TUnzipperWithProgressHandler;
begin
  Result := False;
  UnzipperObj := TUnzipperWithProgressHandler.Create;
  {$IFDEF GUI}
  {$IFDEF OPSISCRIPT}
  FBatchOberflaeche.SetElementVisible(True, eProgressBar); //showProgressBar(True);
  FBatchOberflaeche.SetProgress(0, pPercent);
  UnzipperObj.OnProgressEx := @UnzipperObj.HandleProgressBar;
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
        Sleep(500);
        {$ENDIF OPSISCRIPT}
        {$ENDIF GUI}
        UnzipperObj.Free;
      end;
    end;
  end;
end;


end.
