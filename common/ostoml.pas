{
    Copyright (c) uib GmbH 2021 by Jinene Laajili

    Unit for fpTOML library
}

unit osTOML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strUtils,
  TOML, TOMLParser, TOMLTypes,
  FPJSON,
  osencoding;

function LoadTOMLFile(TOMLfilePath: String): TStringList;
function ReadTOMLFile(TOMLfilePath: String): String;
//function GetTOMLDocument(TOMLfilePath: String): TTOMLDocument;

function SaveToTOMLFile(TOMLcontents: String; TOMLfilePath: String): boolean;
//function SaveToTOMLFile(myTOML: TTOMLDocument; TOMLfilePath: String): boolean;

function ConvertTOMLfileToJSONfile(TOMLfilePath: String; JSONfilePath: String): boolean;
function ConvertTOMLtoJSON(TOMLcontents: String): String;

function GetTOMLAsString(TOMLcontents: String): String;
function GetTOMLAsStringList(TOMLcontents: String): TStringList;

//function GetTOMLKeys(myTOML: TTOMLTable): TStringList;
function GetTOMLKeys(TOMLcontents: String): TStringList;

//function HasTables(myTOML: TTOMLTable): integer;
//function GetTOMLTableNames(myTOML: TTOMLTable): TStringList;
function GetTOMLTableNames(TOMLcontents: String): TStringList;

//function GetTOMLTable(myTOML: TTOMLTable; table : String): TTOMLTable;
function GetTOMLTable(TOMLcontents: String; table : String): TStringList;
//function GetTOMLTableAsString(myTOML: TTOMLTable; table : String): String;
function GetTOMLTableAsString(TOMLcontents: String; table : String): String;
//function GetTOMLTable(TOMLfilePath: String; table : String): TStringList;

function GetValueFromTOML(TOMLcontents: String; keyPath: String; defaultValue: String): String;

function ModifyTOML(TOMLcontents: String; command : String; keyPath: String; value: String): String;
function DeleteTableFromTOML(TOMLcontents: String; tablePath: String): String;

//function AddKeyValueToTOMLFile(TOMLfilePath: String; keyPath : String; value : String): boolean;
//procedure AddKeyValueToTOML(myTOML: TTOMLDocument; keyPath : TTOMLKeyType; value : TTOMLValueType);
//procedure AddKeyValueToTOML(myTOML: TTOMLDocument; keyPath : TTOMLKeyType; value : TTOMLData);


implementation

// GetTOML is not used due to its memory-related problems

function LoadTOMLFile(TOMLfilePath: String): TStringList;
var
  myTOMLStringList: TStringList;
begin
  result := TStringList.Create;
  myTOMLStringList := TStringList.Create;
  TOMLfilePath := ExpandFileName(TOMLfilePath);
  try
  myTOMLStringList.LoadFromFile(TOMLfilePath);
  //myTOMLStringList:= loadTextFileWithEncoding(tomlFilePath,'utf8');
  result.Assign(myTOMLStringList);
  except
    on E:Exception do
      writeln('Exception in LoadFromFile '+ TOMLfilePath +': ', E.Message);
  end;
  myTOMLStringList.Free;
end;

function ReadTOMLFile(TOMLfilePath: String): String;
var
  myFile: TStringList;
begin
  myFile := TStringList.Create;
  TOMLfilePath := ExpandFileName(TOMLfilePath);
  try
  //myFile.LoadFromFile(tomlFilePath);
  myFile:= loadTextFileWithEncoding(TOMLfilePath,'utf8');
  result := myFile.Text;
  except
    on E:Exception do
      writeln('Exception in ReadTOMLFile '+ TOMLfilePath +': ', E.Message);
  end;
  myFile.Free;
end;

(*
// Memory problem expected : May add a second var parameter TTOMLScanner to store the result
function GetTOMLDocument(TOMLfilePath: String): TTOMLDocument;
var
  myFile: String;
  myTOMLScanner: TTOMLScanner;
begin
  TOMLfilePath := ExpandFileName(TOMLfilePath);
  try
  myFile := ReadTOMLFile(TOMLfilePath);
  //result := GetTOML(myFile);
  myTOMLScanner := TTOMLScanner.Create(myFile);
  myTOMLScanner.Parse;
  result := myTOMLScanner.TOMLDocument;
  except
    on E:Exception do
      writeln('Exception in GetTOMLDocument '+ TOMLfilePath +': ', E.Message);
  end;
end;
*)

function SaveToTOMLFile(TOMLcontents : String; TOMLfilePath: String): boolean;
var
  myFile: TStringList;
begin
  result := False;
  myFile := TStringList.Create;
  TOMLfilePath := ExpandFileName(TOMLfilePath);
  myFile.Add(TOMLcontents);
  try
  myFile.SaveToFile(TOMLfilePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToTOMLFile '+ TOMLfilePath +': ', E.Message);
  end;
  myFile.Free;
end;

function SaveToTOMLFile(myTOML : TTOMLDocument; TOMLfilePath: String): boolean;
var
  myFile: TStringList;
begin
  result := False;
  myFile := TStringList.Create;
  tomlFilePath := ExpandFileName(TOMLfilePath);
  myFile.Add(myTOML.AsTOMLString);
  try
  myFile.SaveToFile(TOMLfilePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in SaveToFile '+ TOMLfilePath +': ', E.Message);
  end;
  myFile.Free;
end;

function ConvertTOMLfileToJSONfile(TOMLfilePath: String; JSONfilePath: String): boolean;
var
  myTOMLScanner: TTOMLScanner;
  myFile: TStringList;
  myTOML : TTOMLDocument;
  myJSON : TJSONData;
begin
  result := False;
  myFile := TStringList.Create;
  TOMLfilePath := ExpandFileName(TOMLfilePath);
  JSONfilePath := ExpandFileName(JSONfilePath);
  try
  myFile.LoadFromFile(TOMLfilePath);
  except
    on E:Exception do
      writeln('Exception in ConvertTOMLtoJSON in LoadFromFile '+ TOMLfilePath +': ', E.Message);
  end;
  //myTOML := GetTOML(myFile.Text);
  myTOMLScanner := TTOMLScanner.Create(myFile.Text);
  myTOMLScanner.Parse;
  myTOML := myTOMLScanner.TOMLDocument;
  myJSON := myTOML.AsJSON;
  myFile.Clear;
  myFile.Add(myJSON.FormatJSON);
  try
  myFile.SaveToFile(JSONfilePath);
  result := True;
  except
    on E:Exception do
      writeln('Exception in ConvertTOMLtoJSON in SaveToFile'+ JSONfilePath +': ', E.Message);
  end;
  if Assigned(myTOMLScanner) then
     FreeAndNil(myTOMLScanner);
  myTOML := nil; //should be enough because myTOMLScanner is already freed which contains the document
  //and myTOML is just a pointer to this document
  myFile.Free;
end;

function ConvertTOMLtoJSON(TOMLcontents: String): String;
var
  //myTOML : TTOMLDocument;
  myTOMLScanner : TTOMLScanner;
begin
  //myTOML := TTOMLDocument.Create;
  //myTOML := GetTOML(TOMLcontents);
  //result := myTOML.AsJSON.FormatJSON;
  //myTOML.Free;
  myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
  myTOMLScanner.Parse;
  result := myTOMLScanner.TOMLDocument.AsJSON.FormatJSON;
  FreeAndNil(myTOMLScanner);
end;

function GetTOMLAsString(TOMLcontents: String): String;
var
  //myTOML : TTOMLDocument;
  myTOMLScanner : TTOMLScanner;
begin
  //myTOML := TTOMLDocument.Create;
  //myTOML := GetTOML(TOMLcontents);
  //result := myTOML.AsTOMLString;
  //myTOML.Free;
  myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
  myTOMLScanner.Parse;
  result := myTOMLScanner.TOMLDocument.AsTOMLString;
  FreeAndNil(myTOMLScanner);
end;

function GetTOMLAsStringList(TOMLcontents: String): TStringList;
var
  //myTOML : TTOMLDocument;
  myTOMLScanner : TTOMLScanner;
begin
  result := TStringList.Create;
  //myTOML := TTOMLDocument.Create;
  //myTOML := GetTOML(TOMLcontents);
  //myTOML.Free;
  myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
  myTOMLScanner.Parse;
  result.Assign(myTOMLScanner.TOMLDocument.AsTOMLStringList);
  FreeAndNil(myTOMLScanner);
end;

function GetTOMLKeys(myTOML: TTOMLTable): TStringList;
var
  keysList : TStringList;
  i : integer;
begin
  result := TStringList.Create;
  keysList := TStringList.Create;
  for i := 0 to myTOML.Count -1 do
      keysList.Add(myTOML.Keys[i]);
  result.Assign(keysList);
  keysList.Free;
end;

function GetTOMLKeys(TOMLcontents: String): TStringList;
var
  myTOMLScanner : TTOMLScanner;
  //myTOML : TTOMLDocument;
  keysList : TStringList;
  i : integer;
begin
  result := TStringList.Create;
  keysList := TStringList.Create;
  //myTOML := TTOMLDocument.Create;
  //myTOML := GetTOML(TOMLcontents);
  myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
  myTOMLScanner.Parse;
  keysList := GetTOMLKeys(myTOMLScanner.TOMLDocument);
  result.Assign(keysList);
  keysList.Free;
  //myTOML.Free;
  FreeAndNil(myTOMLScanner);
end;

function HasTables(myTOML: TTOMLTable): integer;
var
  i, nb : integer;
begin
  result := 0;
  nb := 0;
  for i := 0 to myTOML.Count -1 do
    if myTOML.Values[i].ClassName = 'TTOMLTable' then
      nb := nb+1 ;
  result := nb;
end;

function GetTOMLTableNames(myTOML: TTOMLTable): TStringList;
var
  tableNamesList : TStringList;
  i : integer;
begin
  result := TStringList.Create;
  tableNamesList := TStringList.Create;
  for i := 0 to myTOML.Count -1 do
    if  myTOML.Values[i].ClassName = 'TTOMLTable' then
        begin
        tableNamesList.Add(myTOML.Keys[i]);
        end;
  result.Assign(tableNamesList);
  tableNamesList.Free;
end;

function GetTOMLTableNames(TOMLcontents: String): TStringList;
var
  myTOMLScanner : TTOMLScanner;
  //myTOML : TTOMLDocument;
  tableNamesList : TStringList;
  i : integer;
begin
  result := TStringList.Create;
  //myTOML := TTOMLDocument.Create;
  tableNamesList := TStringList.Create;
  //myTOML := GetTOML(TOMLcontents);
  myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
  myTOMLScanner.Parse;

  for i := 0 to myTOMLScanner.TOMLDocument.Count -1 do
    if  (myTOMLScanner.TOMLDocument.Values[i].ClassName = 'TTOMLTable') then
        tableNamesList.Add(myTOMLScanner.TOMLDocument.Keys[i]);
  result.AddStrings(tableNamesList);
  tableNamesList.Free;
  //myTOML.Free;
  FreeAndNil(myTOMLScanner);
end;

function GetTOMLTable(myTOML: TTOMLTable; table : String): TTOMLTable;
var
  myTOMLTable : TTOMLTable;
  j : integer;
begin
  myTOMLTable := TTOMLTable.Create(table);
  try
    j := 0;
    repeat
      if (myTOML.Values[j].ClassName = 'TTOMLTable') then
          if myTOML.Keys[j] = table then
            begin
            myTOMLTable := TTOMLTable(myTOML.Items[j]);
            break;
            end;
      j := j + 1 ;
    until j = myTOML.Count;
  except
  on E:Exception do
        writeln('Exception in GetTOMLTable : ', E.Message);
  end;
  result := myTOMLTable;
  //myTOMLTable.Free;     //causes SIGSEGV exception
end;

function GetTOMLTable(TOMLcontents: String; table : String): TStringList;
var
  myTOMLScanner : TTOMLScanner;
  //myTOML : TTOMLDocument;
  myTOMLTable : TTOMLTable;
begin
  result := TStringList.Create;
  //myTOML := TTOMLDocument.Create;
  myTOMLTable := TTOMLTable.Create;
  try
    //myTOML := GetTOML(TOMLcontents);
    myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
    myTOMLScanner.Parse;
    myTOMLTable := GetTOMLTable(myTOMLScanner.TOMLDocument, table);
    result.Assign(myTOMLTable.AsTOMLStringList);
  except
  on E:Exception do
        writeln('Exception in GetTOMLTable : ', E.Message);
  end;
  //myTOMLTable.Free;     //causes SIGSEGV exception
  //myTOML.Free;
  FreeAndNil(myTOMLScanner);
end;

function GetTOMLTableAsString(myTOML: TTOMLTable; table : String): String;
var
  myTOMLTable : TTOMLTable;
begin
  myTOMLTable := TTOMLTable.Create(table);
  try
    myTOMLTable:= GetTOMLTable(myTOML, table);
    result := myTOMLTable.AsTOMLString;
  except
  on E:Exception do
        writeln('Exception in GetTOMLTableAsString : ', E.Message);
  end;
  //myTOMLTable.Free;      //causes SIGSEGV exception
end;

function GetTOMLTableAsString(TOMLcontents: String; table : String): String;
var
  myTOMLScanner : TTOMLScanner;
  //myTOML : TTOMLDocument;
begin
  //myTOML := TTOMLDocument.Create;
  try
    //myTOML := GetTOML(TOMLcontents);
    myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
    myTOMLScanner.Parse;
    result := GetTOMLTableAsString(myTOMLScanner.TOMLDocument, table);
  except
  on E:Exception do
        writeln('Exception in GetTOMLTableAsString : ', E.Message);
  end;
  //myTOML.Free;
  FreeAndNil(myTOMLScanner);
end;

(*
function GetTOMLTable(TOMLfilePath: String; table : String): TStringList;
var
  myTOMLfile : TStringList;
  myTableList : TStringList;
  tableIndex : integer;
  tableNameString, line : String;
begin
  result := TStringList.Create;
  myTableList := TStringList.Create;
  TOMLfilePath := ExpandFileName(TOMLfilePath);
  myTOMLfile := LoadTOMLFile(TOMLfilePath);

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

  result.Assign(myTableList);
  myTableList.Free;
  myTOMLfile.Free;
end;
*)

function GetValueFromTOML(TOMLcontents: String; keyPath: String; defaultValue: String): String;
var
  myTOMLScanner : TTOMLScanner;
  //myTOML : TTOMLDocument;
  keysArray : TStringList;
  myValue: TTOMLData;
  myTOMLTable : TTOMLTable;
  tablePath : String;
  i, j : integer;
begin
  //Keep result := String(myValue) don't use myValue.ClassName
  //result := defaultValue;
  //myTOML := TTOMLDocument.Create;
  myTOMLTable := TTOMLTable.Create;
  //myTOML := GetTOML(TOMLcontents);
  myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
  myTOMLScanner.Parse;

  keysArray := TStringList.Create;
  keysArray.Delimiter := '.';
  keysArray.StrictDelimiter := True;
  keysArray.DelimitedText := keyPath;

  if keysArray.Count=1 then
    //myValue := myTOML[key]
    try
       if not myTOMLScanner.TOMLDocument.Contains(keyPath)  then
          result := defaultValue
      else
      begin
          myValue := myTOMLScanner.TOMLDocument.Find(keyPath);
          result := String(myValue);
          if result='TTOMLArray' then
             result := TTOMLArray(myValue).AsTOMLString
          else
            begin
              if (TTOMLValue(myValue).TypeString = 'Dynamic string')
               or (TTOMLValue(myValue).TypeString = 'UnicodeString') then
                 result := '"'+result+'"'
              else
                if (TTOMLValue(myValue).TypeString = 'Double') then
                   result := ReplaceStr(result, ',', '.');
            end;
      end;
    except
    on E:Exception do
          writeln('Exception in GetValueFromTOMLfile in Find(keyPath) : Key does not exist : ', E.Message);
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
     try
      myTOMLTable := TTOMLTable(myTOMLScanner.TOMLDocument);
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

      if not myTOMLTable.Contains(keysArray[keysArray.Count-1]) then
        result := defaultValue
      else
        begin
          myValue := myTOMLTable.Find(keysArray[keysArray.Count-1]);
          result := String(myValue);
          if result='TTOMLArray' then
             result := TTOMLArray(myValue).AsTOMLString
          else
            if (TTOMLValue(myValue).TypeString = 'Dynamic string')
             or (TTOMLValue(myValue).TypeString = 'UnicodeString') then
               result := '"'+result+'"'
          else
            if (TTOMLValue(myValue).TypeString = 'Double') then
               result := ReplaceStr(result, ',', '.');
        end;
    except
    on E:Exception do
          writeln('Exception in GetValueFromTOMLfile : ', E.Message);
    end;
  end;

  if (result='TTOMLTable') then
     begin
     writeln('Warning in GetValueFromTOMLfile : A Table path was entered as a Key Path. ');
     result := defaultValue;
     end;
  if (trim(keyPath)='') then
     begin
     writeln('Warning in GetValueFromTOMLfile : An empty Key path was entered. ');
     result := defaultValue;
     end;
  if (result='') then
     result := defaultValue;

  keysArray.Free;
  //myTOML.Free;
  FreeAndNil(myTOMLScanner);
end;

function ModifyTOML(TOMLcontents: String; command : String; keyPath: String; value: String): String;
var
  myTOMLScanner, myValueScanner : TTOMLScanner;
  //myTOML : TTOMLDocument;
  keysArray : TStringList;
  myValue : TTOMLData;
  myTOMLTable, newTable : TTOMLTable;
  tableName : String;
  i : integer;
begin
  //myTOML := TTOMLDocument.Create;
  if trim(keyPath) = '' then
     begin
     writeln('Key is empty, nothing to be done with ModifyTOML ');
     result := TOMLcontents;
     end
  else
  begin
    keysArray := TStringList.Create;
    keysArray.Delimiter := '.';
    keysArray.StrictDelimiter := True;
    keysArray.DelimitedText := keyPath;

    if uppercase(command) <> 'DEL' then
      begin
          //myTOML := GetTOML('key = '+ value);
          myValueScanner := TTOMLScanner.Create('key = '+ value);
          myValueScanner.Parse;
          myValue := myValueScanner.TOMLDocument['key'];
      end;

    //myTOML := GetTOML(TOMLcontents);
    myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
    myTOMLScanner.Parse;

    case uppercase(command) of
    'ADD':
        begin
           if keysArray.Count=1 then
              if myTOMLScanner.TOMLDocument.Contains(keyPath) then
                writeln('Key already exists in root table, nothing to be done with command ADD ')
              else
                myTOMLScanner.TOMLDocument.Insert(keyPath, myValue);

           if keysArray.Count>=2 then
            begin
              myTOMLTable := myTOMLScanner.TOMLDocument;
              try
               for i := 0 to keysArray.Count -2 do
                begin
                    tableName := keysArray[i];
                    if myTOMLTable.Contains(tableName) then
                       myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName))
                    else
                       begin
                       newTable := TTOMLTable.Create(tableName);
                       myTOMLTable.Add(tableName,newTable);
                       myTOMLTable := newTable;
                       end;
                end;
               if myTOMLTable.Contains(keysArray[keysArray.Count-1]) then
                  writeln('Key already exists, nothing to be done with command ADD ')
                  else
                    if HasTables(myTOMLTable)>0 then
                     myTOMLTable.Insert(keysArray[keysArray.Count-1],myValue)
                    else
                     myTOMLTable.Add(keysArray[keysArray.Count-1],myValue);
             except
              on E:Exception do
                writeln('Exception in ModifyTOML : ', E.Message);
             end;
           end;
        end;
    'SET' :
        begin
           if keysArray.Count=1 then
              if myTOMLScanner.TOMLDocument.Contains(keyPath) then
                myTOMLScanner.TOMLDocument.Put(keyPath, myValue)
              else
                myTOMLScanner.TOMLDocument.Insert(keyPath, myValue);

           if keysArray.Count>=2 then
            begin
              myTOMLTable := myTOMLScanner.TOMLDocument;
              try
               for i := 0 to keysArray.Count -2 do
                begin
                    tableName := keysArray[i];
                    if myTOMLTable.Contains(tableName) then
                       myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName))
                    else
                       begin
                       newTable := TTOMLTable.Create(tableName);
                       myTOMLTable.Add(tableName,newTable);
                       myTOMLTable := newTable;
                       end;
                end;
                if myTOMLTable.Contains(keysArray[keysArray.Count-1]) then
                   myTOMLTable.Put(keysArray[keysArray.Count-1],myValue)
                else
                   if HasTables(myTOMLTable)>0 then
                     myTOMLTable.Insert(keysArray[keysArray.Count-1],myValue)
                   else
                     myTOMLTable.Add(keysArray[keysArray.Count-1],myValue);
              except
              on E:Exception do
                writeln('Exception in ModifyTOML : ', E.Message);
              end;
           end;
        end;
    'CHANGE' :
        begin
           try
            myTOMLTable := myTOMLScanner.TOMLDocument;
            if keysArray.Count>=2 then
            begin
               for i := 0 to keysArray.Count -2 do
                begin
                   tableName := keysArray[i];
                   if myTOMLTable.Contains(tableName) then
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
           except
            on E:Exception do
              writeln('Exception in ModifyTOML : ', E.Message);
           end;
        end;
    'DEL' :
        begin
           myTOMLTable := myTOMLScanner.TOMLDocument;
           try
            if keysArray.Count>=2 then
            begin
               for i := 0 to keysArray.Count -2 do
                begin
                   tableName := keysArray[i];
                   if myTOMLTable.Contains(tableName) then
                      myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName))
                   else
                      writeln('KeyPath does not exist, nothing to be done with command DEL ');
                end;
            end;
            if myTOMLTable.Find(keysArray[keysArray.Count-1]) <> nil then
               myTOMLTable.Remove(keysArray[keysArray.Count-1])
            else
               writeln('Key does not exist, nothing to be done with command DEL ');
           except
              on E:Exception do
                writeln('Exception in ModifyTOML : ', E.Message);
           end;
        end;
    otherwise
        writeln('ModifyTOML command unkown ');
    end;
    result := myTOMLScanner.TOMLDocument.AsTOMLString;
    keysArray.Free;
    //myTOML.Free;
    FreeAndNil(myTOMLScanner);
  end;
end;

function DeleteTableFromTOML(TOMLcontents: String; tablePath: String): String;
var
  myTOMLScanner : TTOMLScanner;
  //myTOML : TTOMLDocument;
  tablesArray : TStringList;
  myTOMLTable : TTOMLTable;
  tableName : String;
  i : integer;
begin
  //myTOML := TTOMLDocument.Create;
  //myTOML := GetTOML(TOMLcontents);
  myTOMLScanner := TTOMLScanner.Create(TOMLcontents);
  myTOMLScanner.Parse;
  //myTOML := myTOMLScanner.TOMLDocument;

  tablesArray := TStringList.Create;
  tablesArray.Delimiter := '.';
  tablesArray.StrictDelimiter := True;
  tablesArray.DelimitedText := tablePath;

  myTOMLTable := myTOMLScanner.TOMLDocument;
  try
    if tablesArray.Count>=2 then
    begin
       for i := 0 to tablesArray.Count -2 do
        begin
           tableName := tablesArray[i];
           if myTOMLTable.Contains(tableName) then
              myTOMLTable := TTOMLTable(myTOMLTable.Find(tableName))
           else
              writeln('TablePath does not exist, nothing to be done ');
        end;
    end;
   if myTOMLTable.Find(tablesArray[tablesArray.Count-1]) <> nil then
       myTOMLTable.Remove(tablesArray[tablesArray.Count-1])
   else
      writeln('Table does not exist, nothing to be done ');
  except
    on E:Exception do
      writeln('Exception in DeleteTableFromTOML : ', E.Message);
  end;
  result := myTOMLScanner.TOMLDocument.AsTOMLString ;
  tablesArray.Free;
  //myTOML.Free;
  FreeAndNil(myTOMLScanner);
end;

(*
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
*)
(*
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
*)
(*
// WORKING BUT NOT COMPLETE
function AddKeyValueToTOMLFile(TOMLfilePath: String; keyPath : String; value : String): boolean;
var
  myTOMLfile : TStringList;
  keysArray : TStringList;
  myTOML : TTOMLDocument;
  myTOMLTable : TTOMLTable;
  lineToAdd, line, tablePath, key, table, tableSection: String;
  i, j, k : integer;
begin
  result := false;
  TOMLfilePath := ExpandFileName(TOMLfilePath);
  myTOMLfile := LoadTOMLFile(TOMLfilePath);
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
          writeln('Key : "'+ keyPath + '" already exists in :' +TOMLfilePath)
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
          myTOMLfile.SaveToFile(TOMLfilePath);
          result := True;
          myTOMLfile.Free;
          end;
      except
        on E:Exception do
          writeln('Exception in AddKeyValueToTOMLFile '+ TOMLfilePath +': ', E.Message);
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
        writeln('Key : "'+ keyPath + '" already exists in :' +TOMLfilePath)
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
              writeln('KeyPath : "'+ tablePath + '" does not exist in :' +TOMLfilePath)
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
            myTOMLfile.SaveToFile(TOMLfilePath);
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
*)
end.


