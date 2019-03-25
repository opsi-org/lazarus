unit ZipTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, FileUtil, OsZip;

type
  OsZipTest= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure When_UnzipWithDirStruct_ForFolderWithFiles_Then_UnzipAndReturnTrue;
    procedure When_UnzipWithDirStruct_ForFolderWithSubfoldersAndFiles_Then_UnzipAndReturnTrue;
    procedure When_UnzipWithDirStruct_ForNonExistingFile_Then_ReturnFalse;
  end;

implementation

// create temporary zip files with 'WithFiles' or 'WithFilesAndFolders'.
function CreateTempZip(Switch : String) : Boolean;
var
  TempDir1, TempDir2, TempFolder,
  Path11, Path12, Path21, Path22 : String;
begin
  Result := False;
  if Switch = 'WithFiles' then
  begin
    TempDir1 := IncludeTrailingPathDelimiter(GetCurrentDir) + 'ziptemp1';
    if not FileExists(TempDir1 + '.zip') then
    begin
      if not DirectoryExists(TempDir1) then
      begin
        CreateDir(TempDir1);
        Path11 := IncludeTrailingPathDelimiter(TempDir1) + 'ziptest11.txt';
        FileCreate(Path11);
        Path12 := IncludeTrailingPathDelimiter(TempDir1) + 'ziptest12.txt';
        FileCreate(Path12);
        if ZipWithDirStruct(TempDir1, GetCurrentDir) then
        begin
          DeleteDirectory(TempDir1, True);
          RemoveDir(TempDir1);
          Result := True;
        end;
      end;
    end;
  end
  else if Switch = 'WithFilesAndFolders' then
  begin
    TempDir2 := IncludeTrailingPathDelimiter(GetCurrentDir) + 'ziptemp2';
    if not FileExists(TempDir2 + '.zip') then
    begin
      if not DirectoryExists(TempDir2) then
      begin
        CreateDir(TempDir2);
        Path21 := IncludeTrailingPathDelimiter(TempDir2) + 'ziptest21.txt';
        FileCreate(Path21);

        TempFolder := IncludeTrailingPathDelimiter(TempDir2) + 'tempfolder1';
        CreateDir(TempFolder);
        Path22 := IncludeTrailingPathDelimiter(TempFolder) + 'ziptest22.txt';
        FileCreate(Path22);
        if ZipWithDirStruct(TempDir2, GetCurrentDir) then
        begin
          DeleteDirectory(TempDir2, True);
          RemoveDir(TempDir2);
          Result := True;
        end;
      end;
    end;
  end;
end;

// delete the temporary zip files with 'WithFiles' or 'WithFilesAndFolders'.
function DeleteTempZip(Switch : String) : Boolean;
var
  ZipFile1, ZipFile2 : String;
begin
  Result := False;
  if Switch = 'WithFiles' then
  begin
    ZipFile1 := IncludeTrailingPathDelimiter(GetCurrentDir) + 'ziptemp1.zip';
    if DeleteFile(ZipFile1) then
      Result := True;
  end
  else if Switch = 'WithFilesAndFolders' then
  begin
    ZipFile2 := IncludeTrailingPathDelimiter(GetCurrentDir) + 'ziptemp2.zip';
    if DeleteFile(ZipFile2) then
      Result := True;
  end;
end;

procedure OsZipTest.SetUp;
begin
  CreateTempZip('WithFiles');
  CreateTempZip('WithFilesAndFolders');
end;

procedure OsZipTest.TearDown;
begin
  DeleteTempZip('WithFiles');
  DeleteTempZip('WithFilesAndFolders');
end;

procedure OsZipTest.When_UnzipWithDirStruct_ForFolderWithFiles_Then_UnzipAndReturnTrue;
var
  File2Unzip, OutputDir : String;
begin
  try
    File2Unzip := IncludeTrailingPathDelimiter(GetCurrentDir) + 'ziptemp1.zip';
    OutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'unzip_temp1';
    CreateDir(OutputDir);
    AssertEquals('With a zip file with two text files as input', True, UnzipWithDirStruct(File2Unzip, OutputDir));
  finally
    DeleteDirectory(OutputDir, True);
    RemoveDir(OutputDir);
  end;
end;

procedure OsZipTest.When_UnzipWithDirStruct_ForFolderWithSubfoldersAndFiles_Then_UnzipAndReturnTrue;
var
  File2Unzip, OutputDir : String;
begin
  try
    File2Unzip := IncludeTrailingPathDelimiter(GetCurrentDir) + 'ziptemp2.zip';
    OutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'unzip_temp2';
    CreateDir(OutputDir);
    AssertEquals('With a zip file with a folder and two text files as input', True, UnzipWithDirStruct(File2Unzip, OutputDir));
  finally
    DeleteDirectory(OutputDir, True);
    RemoveDir(OutputDir);
  end;
end;


procedure OsZipTest.When_UnzipWithDirStruct_ForNonExistingFile_Then_ReturnFalse;
var
  NonExistFile : String;
begin
  NonExistFile := IncludeTrailingPathDelimiter(GetCurrentDir) + '***888***888***.zip';
  AssertEquals('With a non existing zip file as input', False, UnzipWithDirStruct(NonExistFile, ''));
end;

initialization

  RegisterTest(OsZipTest);
end.

