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
  FPJSON,
  osencoding;

function LoadTOMLFile(tomlFilePath: String): TStringList;
function ReadTOMLFile(tomlFilePath: String): String;
function GetTOMLDocument(tomlFilePath: String): TTOMLDocument;

function SaveToTOMLFile(TOMLcontents: String; tomlFilePath: String): boolean;
function SaveToTOMLFile(myTOML: TTOMLDocument; tomlFilePath: String): boolean;

function ConvertTOMLfileToJSONfile(tomlFilePath: String; jsonFilePath: String): boolean;
function ConvertTOMLtoJSON(TOMLcontents: String): String;

function HasTables(myTOML: TTOMLTable): integer;

function GetTOMLTableNames(myTOML: TTOMLTable): TStringList;
function GetTOMLTableNames(TOMLcontents: String): TStringList;

function GetTOMLTable(myTOML: TTOMLTable; table : String): TTOMLTable;
function GetTOMLTableAsString(myTOML: TTOMLTable; table : String): String;
function GetTOMLTableAsString(TOMLcontents: String; table : String): String;
function GetTOMLTable(tomlFilePath: String; table : String): TStringList;

function GetValueFromTOML(TOMLcontents: String; keyPath: String; defaultValue: String): String;

function ModifyTOML(tomlContents: String; command : String; keyPath: String; value: String): String;
function DeleteTableFromTOML(tomlContents: String; tablePath: String): String;

function AddKeyValueToTOMLFile(tomlFilePath: String; keyPath : String; value : String): boolean;
procedure AddKeyValueToTOML(myTOML: TTOMLDocument; keyPath : TTOMLKeyType; value : TTOMLValueType);
procedure AddKeyValueToTOML(myTOML: TTOMLDocument; keyPath : TTOMLKeyType; value : TTOMLData);


implementation


function LoadTOMLFile(tomlFilePath: String): TStringList;
var
  myTOMLStringList: TStringList;
begin
  result := TStringList.Create;
  myTOMLStringList := TStringList.Create;
  tomlFilePath := ExpandFileName(tomlFilePath);
  try
  myTOMLStringList.LoadFromFile(tomlFilePath);
  //myTOMLStringList:= loadTextFileWithEncoding(tomlFilePath,'utf8');
  result.AddStrings(myTOMLStringList);
  except
    on E:Exception do
      writeln('Exception in LoadFromFile '+ tomlFilePath +': ', E.Message);
  end;
  myTOMLStringList.Free;
end;

function ReadTOMLFile(tomlFilePath: String): String;
var
  myFile: TStringList;
begin
  myFile := TStringList.Create;
  tomlFilePath := ExpandFileName(tomlFilePath);
  try
  //myFile.LoadFromFile(tomlFilePath);
  myFile:= loadTextFileWithEncoding(tomlFilePath,'utf8');
  result := myFile.Text;
  except
    on E:Exception do
      writeln('Exception in ReadTOMLFile '+ tomlFilePath +': ', E.Message);
  end;
  myFile.Free;
end;

function GetTOMLDocument(tomlFilePath: String): TTOMLDocument;
var
  myFile: String;
begin
  tomlFilePath := ExpandFileName(tomlFilePath);
  try
  myFile := ReadTOMLFile(tomlFilePath);
  result := GetTOML(myFile);
  except
    on E:Exception do
      writeln('Exception in GetTOMLDocument '+ tomlFilePath +': ', E.Message);
  end;
end;

function SaveToTOMLFile(TOMLcontents : String; tomlFilePath: String): boolean;
var
  myFile: TStringList;
begin
  result := False;
  myFile := TStringList.Create;
  tomlFilePath := ExpandFileName(tomlFilePath);
  myFile.Add(TOMLcontents);
  try
  myFile.SaveToFile(tomlFilePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToTOMLFile '+ tomlFilePath +': ', E.Message);
  end;
  myFile.Free;
end;

function SaveToTOMLFile(myTOML : TTOMLDocument; tomlFilePath: String): boolean;
var
  myFile: TStringList;
begin
  result := False;
  myFile := TStringList.Create;
  tomlFilePath := ExpandFileName(tomlFilePath);
  myFile.Add(myTOML.AsTOMLString);
  try
  myFile.SaveToFile(tomlFilePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToFile '+ tomlFilePath +': ', E.Message);
  end;
  myFile.Free;
end;

function ConvertTOMLfileToJSONfile(tomlFilePath: String; jsonFilePath: String): boolean;
var
  myFile: TStringList;
  myTOML : TTOMLDocument;
  myJSON : TJSONData;
begin
  result := False;
  myFile := TStringList.Create;
  tomlFilePath := ExpandFileName(tomlFilePath);
  jsonFilePath := ExpandFileName(jsonFilePath);
  try
  myFile.LoadFromFile(tomlFilePath);
  except
    on E:Exception do
      writeln('Exception in ConvertTOMLtoJSON in LoadFromFile '+ tomlFilePath +': ', E.Message);
  end;
  myTOML := GetTOML(myFile.Text);
  myJSON := myTOML.AsJSON;
  myFile.Clear;
  myFile.Add(myJSON.FormatJSON);
  try
  myFile.SaveToFile(jsonFilePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in ConvertTOMLtoJSON in SaveToFile'+ jsonFilePath +': ', E.Message);
  end;
  myFile.Free;
end;

function ConvertTOMLtoJSON(TOMLcontents: String): String;
var
  myTOML : TTOMLDocument;
begin
  myTOML := GetTOML(TOMLcontents);
  result := myTOML.AsJSON.FormatJSON;
end;

function HasTables(myTOML: TTOMLTable): integer;
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
end;

function GetTOMLTableNames(TOMLcontents: String): TStringList;
var
  myTOML : TTOMLDocument;
  tableNamesList : TStringList;
  i : integer;
begin
  tableNamesList := TStringList.Create;
  myTOML := GetTOML(TOMLcontents);
  for i := 0 to myTOML.Count -1 do
    if  (String(myTOML.Values[i]) = 'TTOMLTable') then
        tableNamesList.Add(myTOML.Keys[i]);
  result := tableNamesList;
end;

function GetTOMLTable(myTOML: TTOMLTable; table : String): TTOMLTable;
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

function GetTOMLTableAsString(myTOML: TTOMLTable; table : String): String;
var
  myTOMLTable : TTOMLTable;
begin
  myTOMLTable:= GetTOMLTable(myTOML, table);
  result := myTOMLTable.AsTOMLString;
end;

function GetTOMLTableAsString(TOMLcontents: String; table : String): String;
var
  myTOML : TTOMLDocument;
begin
  myTOML := GetTOML(TOMLcontents);
  result := GetTOMLTableAsString(myTOML, table);
end;

function GetTOMLTable(tomlFilePath: String; table : String): TStringList;
var
  myTOMLfile : TStringList;
  myTableList : TStringList;
  tableIndex : integer;
  tableNameString, line : String;
begin
  result := TStringList.Create;
  myTableList := TStringList.Create;
  tomlFilePath := ExpandFileName(tomlFilePath);
  myTOMLfile := LoadTOMLFile(tomlFilePath);

  tableIndex := myTOMLfile.IndexOf('['+table+']');
  tableNameString := '['+table+'.';

  repeat
  tableIndex := tableIndex + 1 ;
  line := myTOMLfile[tableIndex] ;
  if LeftStr(trim(line),1)='[' then
    if Pos(tableNameString, line) = 0 then
      break;
  myTableList.Add(line);
  until tableIndex = myTOMLfile.Count -1;

  result := myTableList;
end;

function GetValueFromTOML(TOMLcontents: String; keyPath: String; defaultValue: String): String;
var
  tablePath : String;
  myTOML : TTOMLDocument;
  keysArray : TStringList;
  myValue: TTOMLData;
  myTOMLTable : TTOMLTable;
  i, j : integer;
begin
  //result := defaultValue;
  myTOML := GetTOML(TOMLcontents);

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

function ModifyTOML(tomlContents: String; command : String; keyPath: String; value: String): String;
var
  myTOML : TTOMLDocument;
  keysArray : TStringList;
  myValue : TTOMLData;
  myTOMLTable, newTable : TTOMLTable;
  tableName : String;
  i : integer;
begin
  keysArray := TStringList.Create;
  keysArray.Delimiter := '.';
  keysArray.StrictDelimiter := True;
  keysArray.DelimitedText := keyPath;

  if uppercase(command) <> 'DEL' then
    begin
        myTOML := GetTOML('key = '+ value);
        myValue := myTOML['key'];
    end;

  myTOML := GetTOML(tomlContents);

  case uppercase(command) of
  'ADD':
      begin
         if keysArray.Count=1 then
            if myTOML.Find(keyPath) = nil then
              myTOML.Add(keyPath, myValue)
            else
              writeln('Key already exists in root table, nothing to be done with command ADD ');

         if keysArray.Count>=2 then
          begin
            myTOMLTable := TTOMLTable(myTOML);
            try
             for i := 0 to keysArray.Count -2 do
              begin
                  tableName := keysArray[i];
                  if myTOMLTable.Find(tableName) = nil then
                     begin
                     newTable := TTOMLTable.Create(tableName);
                     myTOMLTable.Add(tableName,newTable);
                     myTOMLTable := TTOMLTable(newTable);
                     end
                  else
                    myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName));
              end;

             if myTOMLTable.Find(keysArray[keysArray.Count -1]) = nil then
                myTOMLTable.Add(keysArray[keysArray.Count-1],myValue)
             else
                writeln('Key already exists, nothing to be done with command ADD ');
            except
            on E:Exception do
              writeln('Exception in ModifyTOML : ', E.Message);
            end;
         end;
      end;
  'SET' :
      begin
         if keysArray.Count=1 then
            myTOML.Put(keyPath, myValue);

         if keysArray.Count>=2 then
          begin
            myTOMLTable := TTOMLTable(myTOML);
            try
             for i := 0 to keysArray.Count -2 do
              begin
                  tableName := keysArray[i];
                  if myTOMLTable.Find(tableName) = nil then
                     begin
                     newTable := TTOMLTable.Create(tableName);
                     myTOMLTable.Add(tableName,newTable);
                     myTOMLTable := TTOMLTable(newTable);
                     end
                  else
                    myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName));
              end;

             myTOMLTable.Put(keysArray[keysArray.Count-1],myValue)

            except
            on E:Exception do
              writeln('Exception in ModifyTOML : ', E.Message);
            end;
         end;
      end;
  'CHANGE' :
      begin
         myTOMLTable := TTOMLTable(myTOML);
         if keysArray.Count>=2 then
          begin
             for i := 0 to keysArray.Count -2 do
              begin
                 tableName := keysArray[i];
                 if myTOMLTable.Find(tableName) <> nil then
                    myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName))
                 else
                    writeln('KeyPath does not exist, nothing to be done with command CHANGE ');
              end;
          end;
         i:= 0;
         repeat
            if (myTOMLTable.Keys[i]=keysArray[keysArray.Count-1]) then
               begin
               myTOMLTable.PutValue(i,myValue);
               break;
               end
            else
              i:= i+1;
         until i = myTOMLTable.Count;
         if i = myTOMLTable.Count then
            writeln('Key does not exist, nothing to be done with command CHANGE ');
      end;
  'DEL' :
      begin
         myTOMLTable := TTOMLTable(myTOML);
         if keysArray.Count>=2 then
          begin
             for i := 0 to keysArray.Count -2 do
              begin
                 tableName := keysArray[i];
                 if myTOMLTable.Find(tableName) <> nil then
                    myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName))
                 else
                    writeln('KeyPath does not exist, nothing to be done with command DEL ');
              end;
          end;
         if myTOMLTable.Find(keysArray[keysArray.Count-1]) <> nil then
             myTOMLTable.Remove(keysArray[keysArray.Count-1])
         else
            writeln('Key does not exist, nothing to be done with command DEL ');
      end;
  otherwise
      writeln('ModifyTOML command unkown ');
  end;
  result := myTOML.AsTOMLString ;
end;

function DeleteTableFromTOML(tomlContents: String; tablePath: String): String;
var
  tableName : String;
  myTOML : TTOMLDocument;
  tablesArray : TStringList;
  myTOMLTable : TTOMLTable;
  i : integer;
begin
  myTOML := GetTOML(tomlContents);
  tablesArray := TStringList.Create;
  tablesArray.Delimiter := '.';
  tablesArray.StrictDelimiter := True;
  tablesArray.DelimitedText := tablePath;

  myTOMLTable := TTOMLTable(myTOML);
   if tablesArray.Count>=2 then
    begin
       for i := 0 to tablesArray.Count -2 do
        begin
           tableName := tablesArray[i];
           if myTOMLTable.Find(tableName) <> nil then
              myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName))
           else
              writeln('TablePath does not exist, nothing to be done ');
        end;
    end;
   if myTOMLTable.Find(tablesArray[tablesArray.Count-1]) <> nil then
       myTOMLTable.Remove(tablesArray[tablesArray.Count-1])
   else
      writeln('Table does not exist, nothing to be done ');
   result := myTOML.AsTOMLString ;
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
    myTOML.Add(keyPath, value);

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

// WORKING BUT NOT COMPLETE
function AddKeyValueToTOMLFile(tomlFilePath: String; keyPath : String; value : String): boolean;
var
  myTOMLfile : TStringList;
  keysArray : TStringList;
  myTOML : TTOMLDocument;
  myTOMLTable : TTOMLTable;
  lineToAdd, line, tablePath, key, table, tableSection: String;
  i, j, k : integer;
begin
  result := false;
  tomlFilePath := ExpandFileName(tomlFilePath);
  myTOMLfile := LoadTOMLFile(tomlFilePath);
  myTOML:= GetTOML(myTOMLfile.Text);

  keysArray := TStringList.Create;
  keysArray.Delimiter := '.';
  keysArray.StrictDelimiter := True;
  keysArray.DelimitedText := keyPath;

  if keysArray.Count=1 then
    begin
    if (keyPath='') or (value='') then
      begin
      result := false;
      writeln('Error in AddKeyValueToTOMLFile : Key or value or both is/are empty');
      end
    else
      begin
      lineToAdd := keyPath +' = '+value;
      try
        if (myTOML.Find(keyPath) <> nil) then
          writeln('Key : "'+ keyPath + '" already exists in :' +tomlFilePath)
        else
          begin
          i := 0;
          j := 0;
          repeat
            line := myTOMLfile[i];
            if LeftStr(trim(line),1)='[' then
              begin
              j := i -1;
              myTOMLfile.Insert(j, lineToAdd);
              writeln('Key-value inserted in line : ',j);
              break;
              end;
            i := i + 1;
          until i = myTOMLfile.Count -1;
          if i = myTOMLfile.Count -1 then
            begin
            myTOMLfile.Add(lineToAdd);
            writeln('Key-value added in line : ',i) ;
            end;
          myTOMLfile.SaveToFile(tomlFilePath);
          result := True;
          myTOMLfile.Free;
          end;
      except
        on E:Exception do
          writeln('Exception in AddKeyValueToTOMLFile '+ tomlFilePath +': ', E.Message);
      end;
      end;
    end;


  if keysArray.Count>=2 then
  begin
    key := keysArray[keysArray.Count-1];
    tablePath := LeftStr(keyPath, length(keyPath)- length(keysArray[keysArray.Count-1]) - 1);
    if ((String(key)='') or (value='')) then
      begin
      result := false;
      writeln('Error in AddKeyValueToTOMLFile : Key or value or both is/are empty');
      end
    else
      begin
      lineToAdd := key+' = '+value;
      if keysArray.Count>2 then
        for i:=3 to keysArray.Count do
            lineToAdd := '  '+lineToAdd;
      myTOMLTable := TTOMLTable(myTOML);
      for i:=0 to keysArray.Count-2 do
      begin
        table := keysArray[i];
        j := 0;
        repeat
          if (myTOMLTable.Keys[j]=table) then
             begin
             myTOMLTable := TTOMLTable(myTOMLTable.Items[j]);
             break;
             end
          else
            j:= j+1;
        until j = myTOMLTable.Count;
      end;
      if (myTOMLTable.Find(key) <> nil) then
        writeln('Key : "'+ keyPath + '" already exists in :' +tomlFilePath)
      else
        begin
          try
            tableSection := '['+tablePath+']' ;
            i:=0;
            repeat
              line := myTOMLfile[i];
              if trim(line) = tableSection then
                begin
                j:= i+1;
                break;
                end;
              i := i + 1;
            until i = myTOMLfile.Count -1;
            if i = myTOMLfile.Count -1 then
              begin
              writeln('KeyPath : "'+ tablePath + '" does not exist in :' +tomlFilePath)
              end;

            repeat
              line := myTOMLfile[j];
              if LeftStr(trim(line),1)='[' then
                begin
                k := j -1;
                myTOMLfile.Insert(k, lineToAdd);
                writeln('Key-value inserted in line : ',k);
                break;
                end;
              j := j + 1;
            until j = myTOMLfile.Count -1;
            if j = myTOMLfile.Count -1 then
              begin
              myTOMLfile.Add(lineToAdd);
              writeln('Key-value added in line : ',j) ;
              end;
            myTOMLfile.SaveToFile(tomlFilePath);
            result := True;
            myTOMLfile.Free;
          except
          on E:Exception do
            writeln('Exception in AddKeyValueToTOML : ', E.Message);
          end;


        end;

      end;
  end;

end;

end.


