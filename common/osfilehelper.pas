unit osfilehelper;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  LazFileutils;


procedure MakeBakFiles(const FName: string; maxbaks: integer);



implementation



procedure MakeBakFiles(const FName: string; maxbaks: integer);
var
  bakcounter: integer;
  problem: string = '';
  rebootWanted: boolean;
  extension: string;
  basename: string;
  path: string;
  newfilename, newbakname: string;

begin
  path := ExtractFilePath(FName);
  basename := ExtractFileNameOnly(FName);
  extension := ExtractFileExt(FName);
  //if FileExists(FName) then
  //begin
    (*
    // this is old style (name.ext.num) and is here only for clean up old logs
    for bakcounter := maxbaks - 1 downto 0 do
    begin
      if FileExists(FName + '.' + IntToStr(bakcounter)) then
      begin
        newfilename := path + PathDelim + basename + '_' +
          IntToStr(bakcounter) + extension;
        FileCopy(FName + '.' + IntToStr(bakcounter), newfilename, problem,
          False, rebootWanted);
        DeleteFileUTF8(FName + '.' + IntToStr(bakcounter));
      end;
    end;
    *)
  // this is new style (name_num.ext)
  for bakcounter := maxbaks - 1 downto 0 do
  begin
    newfilename := path + PathDelim + basename + '_' +
      IntToStr(bakcounter) + extension;
    if FileExists(newfilename) then
    begin
      newbakname := path + PathDelim + basename + '_' +
        IntToStr(bakcounter + 1) + extension;
      if FileExists(newbakname) then
        DeleteFileUTF8(newbakname);
      RenameFileUTF8(newfilename, newbakname);
      //FileCopy(newfilename, newbakname, problem, False, rebootWanted);
    end;
  end;
  newfilename := path + PathDelim + basename + '_' + IntToStr(0) + extension;
  if FileExists(newfilename) then
    DeleteFileUTF8(newfilename);
  if FileExists(FName) then
  begin
    RenameFileUTF8(FName, newfilename);
    //FileCopy(FName, newfilename, problem, False, rebootWanted);
    DeleteFileUTF8(FName);
  end;
  //end;
end;


end.

