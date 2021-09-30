unit TOMLfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TOML, TOMLParser, TOMLTypes,
  FGL, FPJSON, fpJSONrtti;


function ReadTOMLFile(filePath: String): String;
function SaveToTOMLFile(TOMLcontents : String; filePath: String): boolean;
function ConvertTOMLtoJSON(TOMLfile: String; JSONfile: String): boolean;
function GetValueFromTOMLfile(TOMLfile: String; section: String; key: String; defaultValue: String): String;
function GetTOMLSectionNames(TOMLfile: String): TStringList;
function GetTOMLFile(filePath: String): TStringList;
function GetTOMLSection(TOMLfile: String; section : String): TStringList;

implementation

 
function ReadTOMLFile(filePath: String): String;
var
  myFile: TStringList;
begin
  myFile := TStringList.Create;
  filePath := ExpandFileName(filePath);
  try
  myFile.LoadFromFile(filePath);
  result := myFile.Text;
  except
    on E:Exception do
      writeln('Exception in LoadFromFile '+ filePath +': ', E.Message);
  end;
  myFile.Free;
end;

function SaveToTOMLFile(TOMLcontents : String; filePath: String): boolean;
var
  myFile: TStringList;
begin
  result := False;
  myFile := TStringList.Create;
  filePath := ExpandFileName(filePath);
  myFile.Add(TOMLcontents);
  //writeln('' + myFile.Text);
  try
  myFile.SaveToFile(filePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToFile '+ filePath +': ', E.Message);
  end;
  myFile.Free;
end;

function ConvertTOMLtoJSON(TOMLfile: String; JSONfile: String): boolean;
var
  myFile: TStringList;
  myTOML : TTOMLDocument;
  myJSON : TJSONData;
begin
  result := False;
  myFile := TStringList.Create;
  TOMLfile := ExpandFileName(TOMLfile);
  JSONfile := ExpandFileName(JSONfile);
  try
  myFile.LoadFromFile(TOMLfile);
  except
    on E:Exception do
      writeln('Exception in LoadFromFile '+ TOMLfile +': ', E.Message);
  end;
  myTOML := GetTOML(myFile.Text);
  myJSON := myTOML.AsJSON;
  myFile.Clear;
  myFile.Add(myJSON.FormatJSON);
  try
  myFile.SaveToFile(JSONfile);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToFile'+ JSONfile +': ', E.Message);
  end;
  myFile.Free;
end;

function GetValueFromTOMLfile(TOMLfile: String; section: String; key: String; defaultValue: String): String;
var
  myFile : String;
  myTOML : TTOMLDocument;
begin
  result := defaultValue;
  TOMLfile := ExpandFileName(TOMLfile);
  myFile := ReadTOMLFile(TOMLfile);
  myTOML := GetTOML(myFile);
  result := String(myTOML.GetItem(key));
  if result='' then
     result := defaultValue;
end;

function GetTOMLSectionNames(TOMLfile: String): TStringList;
var
  myTOMLfile : String;
  myTOML : TTOMLDocument;
  sectionNamesList : TStringList;
  i : integer;

begin
  sectionNamesList := TStringList.Create;
  TOMLfile := ExpandFileName(TOMLfile);
  myTOMLfile := ReadTOMLFile(TOMLfile);
  myTOML := GetTOML(myTOMLfile);

  for i := 0 to (Length(myTOML.Keys[i])-1) do
    if  (String(myTOML.Values[i]) = 'TTOMLTable') then
        begin
        //writeln('   [', myTOML.Keys[i], ']') ;
        sectionNamesList.Add(myTOML.Keys[i]);
        end
    else
        //writeln(' Not a section : ', myTOML.Keys[i] );
  result := sectionNamesList;
  //writeln(sectionNamesList.Text);
end;

function GetTOMLFile(filePath: String): TStringList;
var
  myTOMLStringList: TStringList;
begin
  result := TStringList.Create;
  myTOMLStringList := TStringList.Create;
  filePath := ExpandFileName(filePath);
  try
  myTOMLStringList.LoadFromFile(filePath);
  result.AddStrings(myTOMLStringList);
  except
    on E:Exception do
      writeln('Exception in LoadFromFile '+ filePath +': ', E.Message);
  end;
  myTOMLStringList.Free;
end;

function GetTOMLSection(TOMLfile: String; section : String): TStringList;
var
  myTOMLfile : TStringList;
  mySectionList : TStringList;
  sectionIndex : integer;
  sectionNameString, line : String;

begin
  result := TStringList.Create;
  mySectionList := TStringList.Create;
  TOMLfile := ExpandFileName(TOMLfile);
  myTOMLfile := GetTOMLFile(TOMLfile);

  sectionIndex := myTOMLfile.IndexOf('['+section+']');
  sectionNameString := '['+section+'.';

  repeat
  sectionIndex := sectionIndex + 1 ;
  line := myTOMLfile[sectionIndex] ;
  if LeftStr(line,1)='[' then
    if Pos(sectionNameString, line) = 0 then
      break;
  mySectionList.Add(line);
  until sectionIndex = myTOMLfile.Count -1;

  result := mySectionList;
end;


end.

