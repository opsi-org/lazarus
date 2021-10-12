unit TOMLfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TOML, TOMLParser, TOMLTypes,
  FPJSON;


function ReadTOMLFile(filePath: String): String;
function SaveToTOMLFile(TOMLcontents : String; filePath: String): boolean;
function GetTOMLDocument(filePath: String): TTOMLDocument;
function ConvertTOMLtoJSON(TOMLfile: String; JSONfile: String): boolean;
function HasSubTables(myTOML : TTOMLDocument): integer;
function GetValueFromTOMLfile(TOMLfile: String; keyPath: String; defaultValue: String): String;
function GetTOMLTableNames(TOMLfile: String): TStringList;
function GetTOMLFile(filePath: String): TStringList;
function GetTOMLTable(TOMLfile: String; table : String): TStringList;

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

function GetTOMLDocument(filePath: String): TTOMLDocument;
var
  myFile: String;
begin
  filePath := ExpandFileName(filePath);
  try
  myFile := ReadTOMLFile(filePath);
  result := GetTOML(myFile);
  except
    on E:Exception do
      writeln('Exception in ReadTOMLFile '+ filePath +': ', E.Message);
  end;
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

function HasSubTables(myTOML : TTOMLDocument): integer;
var
  i, nb : integer;
begin
  result := 0;
  nb := 0;
  for i := 0 to myTOML.Count -1 do
    if (String(myTOML.Values[i]) = 'TTOMLTable') then
      nb := nb+1 ;
  result := nb;
end;

function GetValueFromTOMLfile(TOMLfile: String; keyPath: String; defaultValue: String): String;
var
  myFile, tablePath : String;
  myTOML : TTOMLDocument;
  keysArray : TStringList;
  myValue: TTOMLData;
  myTOMLTable : TTOMLTable;
  i, j : integer;
begin
  result := defaultValue;
  TOMLfile := ExpandFileName(TOMLfile);
  myFile := ReadTOMLFile(TOMLfile);
  myTOML := GetTOML(myFile);

  keysArray := TStringList.Create;
  keysArray.Delimiter := '.';
  keysArray.StrictDelimiter := True;
  keysArray.DelimitedText := keyPath;

  if keysArray.Count=1 then
    //myValue := myTOML[key]
    myValue := myTOML.Find(keyPath);

  (*
  if keysArray.Count=2 then
    //myValue := myTOML[table][key];
    begin
      j := 0;
      repeat
        if (String(myTOML.Values[j]) = 'TTOMLTable') then
            if myTOML.Keys[j] = keysArray[0] then
              begin
              myTOMLTable := TTOMLTable(myTOML.Items[j]) ;
              myValue := myTOMLTable.Find(keysArray[1]);
              break;
              end;
        j := j + 1 ;
      until j = myTOML.Count;
    end;
   *)

   if keysArray.Count>=2 then
   begin
      myTOMLTable := TTOMLTable(myTOML);
      for i := 0 to keysArray.Count -2 do
        begin
        tablePath := keysArray[i];
        j := myTOMLTable.Count - HasSubTables(TTOMLDocument(myTOMLTable)); //or j := 0;
        repeat
          if (myTOMLTable.Keys[j]=tablePath) then
             begin
             myTOMLTable := TTOMLTable(myTOMLTable.Items[j]);
             break;
             end
          else
            j:= j+1;
        until j = myTOMLTable.Count;
        end;
    myValue := myTOMLTable.Find(keysArray[keysArray.Count-1]);
  end;

  result := String(myValue);
  if result='' then
     result := defaultValue;
end;

function GetTOMLTableNames(TOMLfile: String): TStringList;
var
  myTOMLfile : String;
  myTOML : TTOMLDocument;
  tableNamesList : TStringList;
  i : integer;

begin
  tableNamesList := TStringList.Create;
  TOMLfile := ExpandFileName(TOMLfile);
  myTOMLfile := ReadTOMLFile(TOMLfile);
  myTOML := GetTOML(myTOMLfile);

  for i := 0 to myTOML.Count -1 do
    if  (String(myTOML.Values[i]) = 'TTOMLTable') then
        begin
        tableNamesList.Add(myTOML.Keys[i]);
        end;
  result := tableNamesList;
  //writeln(tableNamesList.Text);
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

function GetTOMLTable(TOMLfile: String; table : String): TStringList;
var
  myTOMLfile : TStringList;
  myTableList : TStringList;
  tableIndex : integer;
  tableNameString, line : String;

begin
  result := TStringList.Create;
  myTableList := TStringList.Create;
  TOMLfile := ExpandFileName(TOMLfile);
  myTOMLfile := GetTOMLFile(TOMLfile);

  tableIndex := myTOMLfile.IndexOf('['+table+']');
  //writeln('tableIndex :', tableIndex);
  tableNameString := '['+table+'.';

  repeat
  tableIndex := tableIndex + 1 ;
  line := myTOMLfile[tableIndex] ;
  if LeftStr(line,1)='[' then
    if Pos(tableNameString, line) = 0 then
      break;
  myTableList.Add(line);
  until tableIndex = myTOMLfile.Count -1;

  result := myTableList;
end;


end.

