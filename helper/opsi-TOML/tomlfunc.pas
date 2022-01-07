{
    Copyright (c) uib GmbH 2021 by Jinene Laajili

    Unit for fpTOML library
}

unit TOMLfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TOML, TOMLParser, TOMLTypes,
  FPJSON;

function LoadTOMLFile(filePath: String): TStringList;
function ReadTOMLFile(filePath: String): String;
function GetTOMLDocument(filePath: String): TTOMLDocument;

function SaveToTOMLFile(TOMLcontents : String; filePath: String): boolean;
//function SaveToTOMLFile(myTOML : TTOMLDocument; filePath: String): boolean;

function ConvertTOMLtoJSON(TOMLfile: String; JSONfile: String): boolean;

function HasTables(myTOML : TTOMLDocument): integer;
function GetTOMLTableNames(myTOML: TTOMLTable): TStringList;
function GetTOMLTableNames(TOMLfile: String): TStringList;
function GetTOMLTable(myTOML: TTOMLDocument; table : String): TTOMLTable;
function GetTOMLTable(TOMLfile: String; table : String): TStringList;

function GetValueFromTOMLfile(TOMLfile: String; keyPath: String; defaultValue: String): String;

procedure AddKeyValueToTOML(myTOML: TTOMLDocument; keyPath : TTOMLKeyType; value : TTOMLValueType);
procedure AddKeyValueToTOML(myTOML: TTOMLDocument; keyPath : TTOMLKeyType; value : TTOMLData);

implementation


function LoadTOMLFile(filePath: String): TStringList;
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

(*       // Once TTOMLData.AsTOML.FormatTOML exists
function SaveToTOMLFile(myTOML : TTOMLDocument; filePath: String): boolean;
var
  myFile: TStringList;
begin
  result := False;
  myFile := TStringList.Create;
  filePath := ExpandFileName(filePath);
  myFile.Add(myTOML.AsTOML.FormatTOML);
  try
  myFile.SaveToFile(filePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToFile '+ filePath +': ', E.Message);
  end;
  myFile.Free;
end;
*)

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

function HasTables(myTOML: TTOMLDocument): integer;
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

function GetTOMLTableNames(myTOML: TTOMLTable): TStringList;
var
  tableNamesList : TStringList;
  i : integer;
begin
  tableNamesList := TStringList.Create;
  for i := 0 to myTOML.Count -1 do
    if  (String(myTOML.Values[i]) = 'TTOMLTable') then
        begin
        tableNamesList.Add(myTOML.Keys[i]);
        end;
  result := tableNamesList;
  //writeln(tableNamesList.Text);
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

function GetTOMLTable(myTOML: TTOMLDocument; table : String): TTOMLTable;
var
  myTOMLTable : TTOMLTable;
  j : integer;

begin
  j := 0;
  repeat
    if (String(myTOML.Values[j]) = 'TTOMLTable') then
        if myTOML.Keys[j] = table then
          begin
          myTOMLTable := TTOMLTable(myTOML.Items[j]);
          break;
          end;
    j := j + 1 ;
  until j = myTOML.Count;
  result := myTOMLTable;
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
  myTOMLfile := LoadTOMLFile(TOMLfile);

  tableIndex := myTOMLfile.IndexOf('['+table+']');
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

function GetValueFromTOMLfile(TOMLfile: String; keyPath: String; defaultValue: String): String;
var
  myFile, tablePath : String;
  myTOML : TTOMLDocument;
  keysArray : TStringList;
  myValue: TTOMLData;
  myTOMLTable : TTOMLTable;
  i, j : integer;
begin
  //result := defaultValue;
  TOMLfile := ExpandFileName(TOMLfile);
  myFile := ReadTOMLFile(TOMLfile);
  myTOML := GetTOML(myFile);

  keysArray := TStringList.Create;
  keysArray.Delimiter := '.';
  keysArray.StrictDelimiter := True;
  keysArray.DelimitedText := keyPath;

  if keysArray.Count=1 then
    //myValue := myTOML[key]
    try
       if (myTOML.Find(keyPath) = nil)  then
          result := defaultValue
      else
        begin
          myValue := myTOML.Find(keyPath);
          result := String(myValue);
      end;
    except
    on E:Exception do
          writeln('Exception in GetValueFromTOMLfile in Find(keyPath) = Key does not exist : ', E.Message);
    end;

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
        try
          tablePath := keysArray[i];
          //j := myTOMLTable.Count - HasTables(TTOMLDocument(myTOMLTable));
          j := 0;
          repeat
            if (myTOMLTable.Keys[j]=tablePath) then
               begin
               myTOMLTable := TTOMLTable(myTOMLTable.Items[j]);
               break;
               end
            else
              j:= j+1;
          until j = myTOMLTable.Count;
        except
        on E:Exception do
          writeln('Exception in GetValueFromTOMLfile : ', E.Message);
        end;
        end;

      if (myTOMLTable.Find(keysArray[keysArray.Count-1]) = nil ) then
        result := defaultValue
      else
        begin
          myValue := myTOMLTable.Find(keysArray[keysArray.Count-1]);
          result := String(myValue);
        end;
  end;

  if result='' then
     result := defaultValue;
end;

procedure AddKeyValueToTOML(myTOML: TTOMLDocument; keyPath : TTOMLKeyType; value : TTOMLValueType);
var
  tablePath : String;
  keysArray : TStringList;
  myTOMLTable : TTOMLTable;
  i, j : integer;
begin

  keysArray := TStringList.Create;
  keysArray.Delimiter := '.';
  keysArray.StrictDelimiter := True;
  keysArray.DelimitedText := keyPath;

  if keysArray.Count=1 then
    myTOML.Add(keyPath, value);;

  if keysArray.Count>=2 then
  begin
    myTOMLTable := TTOMLTable(myTOML);
    for i := 0 to keysArray.Count -2 do
      begin
      try
        tablePath := keysArray[i];
        //j := myTOMLTable.Count - HasTables(TTOMLDocument(myTOMLTable));
        j := 0;
        repeat
          if (myTOMLTable.Keys[j]=tablePath) then
             begin
             myTOMLTable := TTOMLTable(myTOMLTable.Items[j]);
             break;
             end
          else
            j:= j+1;
        until j = myTOMLTable.Count;
        myTOMLTable.Add(keysArray[keysArray.Count-1],value);
      except
      on E:Exception do
        writeln('Exception in AddKeyValueToTOML : ', E.Message);
      end;
      end;
  end;
end;

procedure AddKeyValueToTOML(myTOML: TTOMLDocument; keyPath : TTOMLKeyType; value : TTOMLData);
var
  tablePath : String;
  keysArray : TStringList;
  myTOMLTable : TTOMLTable;
  i, j : integer;
begin

  keysArray := TStringList.Create;
  keysArray.Delimiter := '.';
  keysArray.StrictDelimiter := True;
  keysArray.DelimitedText := keyPath;

  myTOMLTable := TTOMLTable(myTOML);
  for i := 0 to keysArray.Count -2 do
    begin
    try
      tablePath := keysArray[i];
      //j := myTOMLTable.Count - HasTables(TTOMLDocument(myTOMLTable));
      j := 0;
      repeat
        if (myTOMLTable.Keys[j]=tablePath) then
           begin
           myTOMLTable := TTOMLTable(myTOMLTable.Items[j]);
           break;
           end
        else
          j:= j+1;
      until j = myTOMLTable.Count;
      myTOMLTable.Add(keysArray[keysArray.Count-1],value);
    except
    on E:Exception do
      writeln('Exception in AddKeyValueToTOML : ', E.Message);
    end;
    end;
end;

end.


