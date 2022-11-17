unit osjson;

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel



{$mode delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


interface

uses
  //Classes, SysUtils, FpJson, JsonParser;
  {$IFDEF OSLOG}
  oslog,
  {$ENDIF OSLOG}
  Classes,
  SysUtils,
  superobject,
  FpJson,
  JsonParser,
  RegExpr,
  strutils;

type
  TSuperTypeNames = array [TSuperType] of string [50];
  TPSuperTypeNames = ^TSuperTypeNames;


function jsonIsValid(str: string): boolean;
function jsonGetValidString(str: string; var strresult: string): boolean;
function jsonISOElementGetType(str: string): string;
function jsonTJElementGetType(str: string): string;
function str2jsondata(str: string): TJsondata;
function jsonClearString(str: string): string;
function checkNumber(const str: string): boolean;
function checkIfIPAddress(const str: string): boolean;

function jsonIsArray(str: string): boolean;
function jsonAsArrayCountElements(str: string): integer; ///
function jsonAsArrayToStringList(str: string;
  var strListResult: TStringList): boolean; ///
// TODO: result: boolean
function jsonAsArrayGetElementByIndex(str: string; index: cardinal;
  var elemstr: string): boolean; /// implement
function jsonAsArrayPutObjectByIndex(elemstr: string; var arraystr: string;
  index: cardinal): boolean;///
function jsonAsArrayDeleteObjectByIndex(var arraystr: string;
  index: cardinal): boolean;///

function jsonIsObject(str: string): boolean;
function jsonIsString(str: string): boolean;
function jsonAsObjectCountElements(str: string): integer; ///
function jsonAsObjectGetKeyList(str: string;
  var strListResult: TStringList): boolean; ///
function stringListToJsonArray(strlist: TStringList;
  var strresult: string): boolean;  ///
function jsonAsObjectHasKey(str: string; key: string): boolean;///
function jsonAsObjectSetValueByKey(str: string; key: string; Value: string;
  var stringToSet: string): boolean;  ///
function jsonAsObjectSetStringtypeValueByKey(str: string; key: string;
  Value: string; var stringToSet: string): boolean;
function jsonAsObjectAddKeyAndValue(str: string; key: string;
  Value: string; var stringToSet: string): boolean;  ///
function jsonAsObjectGetValueByKey(str: string; key: string;
  var Value: string): boolean; ///
function jsonAsObjectDeleteByKey(var str: string; key: string): boolean; ///


//function jsonAsObjectGetValueByKey(str:string;key:string; var valresult:string) : boolean;

(*
function jsonGetCount(str:string) : integer;
jsonAsArrayCountElements
jsonAsObjectCountElements

function jsonGetPartByIndex(str:string; index:integer) : String;
jsonAsArrayGetElementByIndex fürs Array?


function jsonGetNames(str:string) : TStringlist;
getKeyList?

function jsonGetIndexByKey(str:string; key : string) : integer;

*)

//https://superobject.googlecode.com/git/readme.html
implementation

{$IFNDEF OSLOG}
const
  LLnothing = 0;
  LLessential = 1;
  LLcritical = 2;
  LLerror = 3;
  LLwarning = 4;
  LLnotice = 5;
  LLinfo = 6;
  LLdebug = 7;
  LLdebug2 = 8;
  LLdebug3 = 9;
  LLconfidential = LLdebug3;
  {$ENDIF OSLOG}

var
  PSuperTypeNames: TPSuperTypeNames;

procedure log(str: string; level: integer);
begin
  // log only if logging unit is included
  {$IFDEF OSLOG}
  LogDatei.log(str, level);
  {$ENDIF OSLOG}

end;

function escapeControlChars(t: string): string;
begin
  t := stringreplace(t, char($5c), '\\', [rfReplaceAll, rfIgnoreCase]); //  \
  t := stringreplace(t, char($22), '\"', [rfReplaceAll, rfIgnoreCase]); //  "
  //t := stringreplace(t, char($27), '\''', [rfReplaceAll, rfIgnoreCase]); //  '
  t := stringreplace(t, char($2f), '\/', [rfReplaceAll, rfIgnoreCase]); //  /
  t := stringreplace(t, #0, '\u0000', [rfReplaceAll, rfIgnoreCase]); //  null
  t := stringreplace(t, #2, '\c', [rfReplaceAll, rfIgnoreCase]); //  #10
  t := stringreplace(t, #3, '\c', [rfReplaceAll, rfIgnoreCase]); //  #10
  t := stringreplace(t, #4, '\c', [rfReplaceAll, rfIgnoreCase]); //  #10
  t := stringreplace(t, #5, '\c', [rfReplaceAll, rfIgnoreCase]); //  #10
  t := stringreplace(t, #6, '\c', [rfReplaceAll, rfIgnoreCase]); //  #10
  t := stringreplace(t, #7, '\a', [rfReplaceAll, rfIgnoreCase]); //  #10
  t := stringreplace(t, #8, '\b', [rfReplaceAll, rfIgnoreCase]); //  backspace
  t := stringreplace(t, #9, '\t', [rfReplaceAll, rfIgnoreCase]); //  tab
  t := stringreplace(t, #10, '\n', [rfReplaceAll, rfIgnoreCase]); //  newline
  t := stringreplace(t, #11, '\v', [rfReplaceAll, rfIgnoreCase]); //  vertical tab
  t := stringreplace(t, #12, '\f', [rfReplaceAll, rfIgnoreCase]); //  form feed
  t := stringreplace(t, #13, '\r', [rfReplaceAll, rfIgnoreCase]); //  carriage return
  t := stringreplace(t, #14, '\c', [rfReplaceAll, rfIgnoreCase]); //  SO
  t := stringreplace(t, #15, '\c', [rfReplaceAll, rfIgnoreCase]); //  SI
  t := stringreplace(t, #16, '\c', [rfReplaceAll, rfIgnoreCase]); //  DLE
  t := stringreplace(t, #17, '\c', [rfReplaceAll, rfIgnoreCase]); //  DC1
  t := stringreplace(t, #18, '\c', [rfReplaceAll, rfIgnoreCase]); //  DC2
  t := stringreplace(t, #19, '\c', [rfReplaceAll, rfIgnoreCase]); //  DC3
  t := stringreplace(t, #20, '\c', [rfReplaceAll, rfIgnoreCase]); //  DC4
  t := stringreplace(t, #21, '\c', [rfReplaceAll, rfIgnoreCase]); //  NAK
  t := stringreplace(t, #22, '\c', [rfReplaceAll, rfIgnoreCase]); //  SYN
  t := stringreplace(t, #23, '\c', [rfReplaceAll, rfIgnoreCase]); //  ETB
  t := stringreplace(t, #24, '\c', [rfReplaceAll, rfIgnoreCase]); //  Cancel
  t := stringreplace(t, #25, '\c', [rfReplaceAll, rfIgnoreCase]); //  EM
  t := stringreplace(t, #26, '\c', [rfReplaceAll, rfIgnoreCase]); //  SUB
  t := stringreplace(t, #27, '\c', [rfReplaceAll, rfIgnoreCase]); //  ESC
  t := stringreplace(t, #28, '\c', [rfReplaceAll, rfIgnoreCase]); //  FS
  t := stringreplace(t, #29, '\c', [rfReplaceAll, rfIgnoreCase]); //  GS
  t := stringreplace(t, #30, '\c', [rfReplaceAll, rfIgnoreCase]); //  RS
  t := stringreplace(t, #31, '\c', [rfReplaceAll, rfIgnoreCase]); //  US
  t := stringreplace(t, #127, '\c', [rfReplaceAll, rfIgnoreCase]); //  DEL
  Result := t;
end;


function jsonIsValid(str: string): boolean;
var
  new_obj: ISuperObject;
begin
  new_obj := SO(str);
  if new_obj <> nil then
    Result := True
  else
    Result := False;
end;

function jsonGetValidString(str: string; var strresult: string): boolean;
var
  new_obj: ISuperObject;
begin
  Result := False;
  new_obj := SO(str);
  if new_obj <> nil then
  begin
    strresult := new_obj.asJson;
    Result := True;
  end;
end;

function jsonClearString(str: string): string;
  // TODO delete cr/lf - others?
var
  cleared_string: string;
begin
  cleared_string := str;
  cleared_string := StringReplace(str, '\"', '', [rfReplaceAll, rfIgnoreCase]);
  cleared_string := StringReplace(cleared_string, '\/', '/',
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(cleared_string, '\n', '', [rfReplaceAll, rfIgnoreCase]);
end;

function jsonISOElementGetType(str: string): string;
var
  new_obj: ISuperObject;
begin
  Result := 'new_obj may be nil';
  new_obj := SO(str);
  if new_obj <> nil then
  begin
    case new_obj.GetDataType of
      stArray: Result := 'Array st';
      stObject: Result := 'Object st';
      stBoolean: Result := 'Boolean st';
      stString: Result := 'String st';
      stNull: Result := 'Null st';
      // TODO check if really number
      stInt: Result := 'Integer st';
      stDouble: Result := 'Double st';
      stCurrency: Result := 'Currency st';
      else
        Result := '';
    end;
  end
  else
    Result := 'not a valid json';
end;

function jsonTJElementGetType(str: string): string;
var
  myjd: TJSONData;
begin
  Result := 'myjd may be nil';
  try
    myjd := str2jsondata(str);
    if myjd <> nil then
      case myjd.JSONType of
        jtString: Result := 'String jt';
        jtNumber:
        begin
          if checkNumber(str) then
            Result := 'Number jt'
          else
            Result := 'String jt, but jtNumber';
        end;
        jtArray: Result := 'Array jt';
        jtObject: Result := 'Object jt';
        jtBoolean: Result := 'Boolean jt';
        jtNull: Result := 'Null jt';
        else
          Result := '???';
      end;
  except
    //result:= '';
  end;
end;

function checkIfIPAddress(const str: string): boolean;
  // todo: use regex?
begin
  Result := True;
end;

function checkNumber(const str: string): boolean;
var
  d: double;
begin
  if TryStrToFloat(str, d) then
    Result := True
  else
    Result := False;
end;

function jsonIsArray(str: string): boolean;
var
  new_obj: ISuperObject;
begin
  Result := False;
  new_obj := SO(str);
  if (new_obj <> nil) then
    Result := new_obj.IsType(stArray);
end;

function jsonAsArrayCountElements(str: string): integer;
var
  new_obj: ISuperObject;
begin
  Result := -1;
  new_obj := SO(str);
  if new_obj <> nil then
    if new_obj.IsType(stArray) then
      Result := new_obj.AsArray.Length
    else
      Result := -1;
end;

function jsonAsArrayGetElementByIndex(str: string; index: cardinal;
  var elemstr: string): boolean;
var
  new_obj: ISuperObject;
begin
  elemstr := '';
  Result := False;
  new_obj := SO(str);
  if new_obj <> nil then
    if new_obj.IsType(stArray) then
      if ((index >= 0) or (index < new_obj.AsArray.Length)) then
      begin
        elemstr := new_obj.AsArray.S[index];
        Result := True;
      end;
end;

function jsonAsArrayPutObjectByIndex(elemstr: string; var arraystr: string;
  index: cardinal): boolean;
var
  new_obj: ISuperObject;
  new_elem: ISuperObject;
begin
  Result := False;
  new_obj := SO(arraystr);
  new_elem := SO(elemstr);
  if (new_obj <> nil) and (new_elem <> nil) then
    if new_obj.IsType(stArray) then
      if ((index >= 0) or (index < new_obj.AsArray.Length)) then
      begin
        new_obj.AsArray.O[index] := new_elem;
        arraystr := new_obj.AsString;
        Result := True;
      end;
end;

function jsonAsArrayDeleteObjectByIndex(var arraystr: string; index: cardinal): boolean;
var
  new_obj: ISuperObject;
begin
  Result := False;
  new_obj := SO(arraystr);
  if (new_obj <> nil) then
    if new_obj.IsType(stArray) then
      if ((index >= 0) or (index < new_obj.AsArray.Length)) then
      begin
        new_obj.AsArray.Delete(index);
        arraystr := new_obj.AsString;
        Result := True;
      end;

end;

function stringListToJsonArray(strlist: TStringList; var strresult: string): boolean;
var
  j: integer;
  jsonstring: string;
  new_obj: ISuperObject;
begin
  try
    jsonstring := '[';
    Result := False;
    if (strlist <> nil) and (strlist.Count > 0) then
      for j := 0 to strlist.Count - 1 do
      begin
        if EndsStr(strlist.Strings[j], '"') and Startsstr(strlist.Strings[j], '"') then
          AppendStr(jsonstring, strlist.Strings[j])
        else
          AppendStr(jsonstring, '"' + strlist.Strings[j] + '"');
        if (j < strlist.Count - 1) then
          AppendStr(jsonstring, ',');
      end;
    AppendStr(jsonstring, ']');
    new_obj := SO(jsonstring);
    if new_obj.IsType(stArray) then
    begin
      strresult := new_obj.AsJson;
      Result := True;
    end;
  except
    Result := False;
  end;
end;

function jsonAsArrayToStringList(str: string; var strListResult: TStringList): boolean;
var
  new_obj: ISuperObject;
  i: integer;
  objstr: string;
  testbool: boolean;
begin
  Result := False;
  strListResult := TStringList.Create;
  new_obj := SO(str);
  if new_obj <> nil then
    if new_obj.IsType(stArray) then
    begin
      Result := True;
      for i := 0 to new_obj.AsArray.Length - 1 do
      begin
        objstr := new_obj.AsArray.S[i];
        //objstr := escapeControlChars(objstr);
        objstr := stringreplace(objstr, #10, '\n', [rfReplaceAll, rfIgnoreCase]);
        if jsonIsObject(objstr) or jsonIsString(objstr) or
          TryStrToBool(objstr, testbool) then
          strListResult.Append(objstr);
      end;
    end;
end;

function jsonIsObject(str: string): boolean;
var
  new_obj: ISuperObject;
begin
  Result := False;
  try
    if str <> '' then
    begin
      new_obj := SO(str);
      if new_obj <> nil then
        Result := new_obj.IsType(stObject);
    end
    else
      log('Error in jsonIsObject: Empty string is no JSON', LLerror);
  except
    on e: Exception do
    begin
      log('Exception in jsonIsObject: ' + e.message + ' with string: ' + str, LLerror);
    end;
  end;
end;

function jsonIsString(str: string): boolean;
var
  new_obj: ISuperObject;
begin
  Result := False;
  new_obj := SO(str);
  if new_obj <> nil then
    Result := new_obj.IsType(stString);
end;


function jsonAsObjectCountElements(str: string): integer;
var
  new_obj: ISuperObject;
begin
  new_obj := SO(str);
  if new_obj <> nil then
    if new_obj.IsType(stObject) then
      Result := new_obj.AsObject.Count
    else if new_obj.IsType(stArray) then
      Result := new_obj.AsArray.Length
    else
      Result := -1;
end;

function jsonAsObjectAddKeyAndValue(str: string; key: string;
  Value: string; var stringToSet: string): boolean;
var
  value_obj, new_obj: ISuperObject;
begin
  new_obj := SO(str);
  value_obj := SO(Value);
  if (new_obj <> nil) and (value_obj <> nil) then
    case value_obj.GetDataType of
      stString:
      begin
        new_obj.S[key] := value_obj.AsString;
        Result := True;
      end;
      stBoolean:
      begin
        new_obj.B[key] := value_obj.AsBoolean;
        Result := True;
      end;
      stDouble, stCurrency:   // no currency or double needed, set as String
      begin
        new_obj.S[key] := value_obj.AsString;
        Result := True;
      end;
      stArray, stObject:
      begin
        new_obj.O[key] := value_obj;
        Result := True;
      end;
      stInt:
      begin
        new_obj.I[key] := value_obj.AsInteger;
        Result := True;
      end;
      else
      begin
        Result := False;
      end
    end;
  if Result then
    stringToSet := new_obj.AsJson
  else
    stringToSet := '';
end;

function jsonAsObjectSetValueByKey(str: string; key: string; Value: string;
  var stringToSet: string): boolean;
var
  value_obj, new_obj: ISuperObject;
  mykeylist: TStringList;
begin
  Result := False;
  mykeylist := TStringList.Create;
  new_obj := SO(str);
  value_obj := SO(Value);
  if jsonAsObjectGetKeyList(str, mykeylist) then
    if (new_obj <> nil) and new_obj.IsType(stObject) then
    begin
      if (mykeylist.IndexOf(key) >= 0) then
      begin
        case value_obj.GetDataType of
          stArray, stObject:
            new_obj.O[key] := value_obj;
          stDouble, stCurrency:   // no currency or double needed, set as String
            new_obj.S[key] := Value;
          stInt:
            new_obj.I[key] := value_obj.AsInteger;
          stBoolean:
            new_obj.B[key] := value_obj.AsBoolean;
          else
            new_obj.S[key] := Value;
        end;
        //stringToSet := new_obj.AsJson;
        stringToSet := new_obj.AsString;
        Result := True;
      end
      else
      begin
        // key does not exist (case insensitive) - create it
        case value_obj.GetDataType of
          stString:
          begin
            new_obj.S[key] := value_obj.AsString;
            Result := True;
          end;
          stBoolean:
          begin
            new_obj.B[key] := value_obj.AsBoolean;
            Result := True;
          end;
          stDouble, stCurrency:   // no currency or double needed, set as String
          begin
            new_obj.S[key] := value_obj.AsString;
            Result := True;
          end;
          stArray, stObject:
          begin
            new_obj.O[key] := value_obj;
            Result := True;
          end;
          stInt:
          begin
            new_obj.I[key] := value_obj.AsInteger;
            Result := True;
          end;
          else
          begin
            Result := False;
          end
        end;
        if Result then
          stringToSet := new_obj.AsString
        //stringToSet := new_obj.AsJson
        else
          stringToSet := '';
      end;
    end
    else
      Result := False;
end;

function jsonAsObjectDeleteByKey(var str: string; key: string): boolean; ///
var
  new_obj: ISuperObject;
  mykeylist: TStringList;
begin
  Result := False;
  mykeylist := TStringList.Create;
  new_obj := SO(str);
  if jsonAsObjectHasKey(str, key) then
    if (new_obj <> nil) and new_obj.IsType(stObject) then
    begin
      new_obj.Delete(key);
      str := new_obj.AsString;
      Result := True;
    end;
  mykeylist.Clear;
end;

function jsonAsObjectSetStringtypeValueByKey(str: string; key: string;
  Value: string; var stringToSet: string): boolean;
var
  value_obj, new_obj: ISuperObject;
  mykeylist: TStringList;
begin
  try
    try
      Result := False;
      mykeylist := TStringList.Create;
      new_obj := SO(str);
      value_obj := SO('"' + escapeControlChars(Value) + '"');
      if jsonAsObjectGetKeyList(str, mykeylist) then
        if (new_obj <> nil) and new_obj.IsType(stObject) then
        begin
          if (mykeylist.IndexOf(key) >= 0) then
            new_obj.S[key] := Value
          else  // key does not exist (case insensitive) - create it
            new_obj.S[key] := value_obj.AsString;
          //stringToSet := new_obj.AsJson;
          stringToSet := new_obj.AsString;
          Result := True;
        end
        else
        begin
          stringToSet := '';
          Result := False;
        end;
    except
      on e: Exception do
      begin
        log('Exception in jsonAsObjectSetStringtypeValueByKey: ' + e.message, LLerror);
      end;
    end;
  finally
    FreeAndNil(mykeylist);
  end;
end;


function jsonAsObjectGetKeyList(str: string; var strListResult: TStringList): boolean;
var
  new_obj: ISuperObject;
  i: integer;
begin
  Result := False;
  //strListResult := TStringList.Create;
  new_obj := SO(str);
  if new_obj <> nil then
    if new_obj.AsObject.GetNames.IsType(stArray) then
    begin
      Result := True;
      for i := 0 to new_obj.AsObject.GetNames.AsArray.Length - 1 do
      begin
        strListResult.Append(new_obj.AsObject.GetNames.AsArray.S[i]);
        //Result := True;
      end;
    end;
end;

function jsonAsObjectHasKey(str: string; key: string): boolean;
var
  keyList: TStringList;
begin
  Result := False;
  keyList := TStringList.Create;
  if jsonIsValid(str) then
  begin
    if jsonAsObjectGetKeyList(str, keyList) then
      if keyList.IndexOf(key) >= 0 then
        Result := True;
  end;
  keyList.Free;
end;

function jsonAsObjectGetValueByKey(str: string; key: string; var Value: string): boolean;
var
  myjd: TJSONData;
  myjo: TJSONObject;
begin
  Result := False;
  if jsonIsObject(str) then
  begin
    if jsonAsObjectHasKey(str, key) then
      try
        try
          myjd := str2jsondata(str);
          myjo := TJSONObject.Create;
          myjo := TJSONObject(myjd);
          if myjo <> nil then
          begin
            case myjo.Elements[key].JSONType of
              jtString: Value := myjo.Elements[key].AsString;
              jtNumber: Value := '"' + myjo.Elements[key].AsString + '"';
                //jtBoolean: value:= myjo.Elements[key].AsBoolean ;
              else
                Value := myjo.Elements[key].AsJson;
            end;
            Result := True;
          end;
        except
          //result:= false;
        end;
      finally
        myjo.Free;
      end;
  end
  else
  begin
    {$IFDEF OSLOG}
    LogDatei.log('Error no valid JSON given', LLError);
    {$ENDIF OSLOG}
    Value := '';
  end;
end;

function str2jsondata(str: string): TJSONData;
var
  myparser: TJSONParser;
  strresult: string;
begin
  Result := nil;
  try
    if jsonGetValidString(str, strresult) then
    begin
      try
        myparser := TJSONParser.Create(strresult);
        Result := myparser.Parse;
      finally
        myparser.Free;
      end;
    end
  except
    // not json
  end;
end;


(*
var
jo : TJSONObject;
ja : TJSONArray;
jp : TJSONParser;
jd : TJSONData;
function jsonGetIndexByKey(str:string; key : string) : integer;
var
myjd : TJSONData;
myjo : TJSONObject;
i : integer;
begin
  try
    try
      myjd := str2jsondata(str);
      myjo := TJSONObject.Create;
      myjo := TJSONObject(myjd);
      result := myjo.IndexOfName(key);
    except
      // not json
    end;
  finally
    myjo.Free;
  end;
end;


function jsonGetNames(str:string) : TStringlist;
var
myjd : TJSONData;
myjo : TJSONObject;
i : integer;
begin
  try
    try
      result := TStringlist.Create;
      myjd := str2jsondata(str);
      myjo := TJSONObject.Create;
      myjo := TJSONObject(myjd);
      for i := 0 to myjo.Count-1 do
       result.Append(myjo.Names[i]);
    except
      // not json
    end;
  finally
    myjo.Free;
  end;
end;


function jsonGetValueByKey(str:string; key:string) : String;
var
myjd : TJSONData;
myjo : TJSONObject;
begin
  try
    try
      myjd := str2jsondata(str);
      myjo := TJSONObject.Create;
      myjo := TJSONObject(myjd);
      result := myjo.Elements[key].AsString;
    except
      // not json
    end;
  finally
    myjo.Free;
  end;
end;


function jsonGetPartByIndex(str:string; index:integer) : String;
var
  myjd : TJSONData;
begin
  myjd := str2jsondata(str);
  //myjo := TJSONObject.Create;
  //myja := TJSONArray.Create;
  //myja.Add(myjd);
  if (index >= 0) and (index < myjd.Count) then
    result := myjd.Items[index].AsJSON;
end;


function jsonGetCount(str:string) : integer;
var
  myjd : TJSONData;
begin
  myjd := str2jsondata(str);
  result := myjd.Count;
end;

function jsonIsValid(str:string) : boolean;
var
myparser : TJSONParser;
myjo : TJSONObject;
myjd : TJSONData;
begin
  result := false;
  try
    try
      myjo := TJSONObject.Create;
      myparser := TJSONParser.Create(str);
      myjd := myparser.Parse;
      result := true;
    except
      // not json
    end;
  finally
    myparser.Free;
    myjo.Free;
  end;
end;

*)
(*
function jsonAsObjectGetValueByKey(str:string;key:string; var valresult:string) : boolean;
// funktioniert nicht mit . im key
var
  new_obj: ISuperObject;
  value: string;
  mykeylist : TStringlist;
begin
  result:=false;
  new_obj := SO(str);
  if new_obj.IsType(stObject) then
    begin
      mykeylist:=  jsonAsObjectGetKeyList(str);
      if (mykeylist.IndexOf(key)>=0) then
        begin
          //valresult:=slashHelper(new_obj.O[key].AsJson);
          //valresult:=new_obj.O[key].AsJson;
          //value := new_obj.O[key].AsJson;
          case new_obj.O[key].GetDataType of
             stString: valresult := new_obj.S[key];
             stInt, stCurrency: valresult := '"' + new_obj.S[key] + '"';
          else valresult := new_obj.O[key].AsJson
          //valresult := new_obj.O[key].AsJson;
          end;
          result:=true;
        end
      else result:= false;
    end;
  // else new_obj is not an object, return == false
end;
*)
begin
  (*
    TSuperType = (
    stNull,
    stBoolean,
    stDouble,
    stCurrency,
    stInt,
    stObject,
    stArray,
    stString
{$IFDEF SUPER_METHOD}
    ,stMethod
{$ENDIF}
  );
*)

  GetMem(PSuperTypeNames, sizeof(TSuperTypeNames));
  PSuperTypeNames^ [stNull] := 'stNull';
  PSuperTypeNames^ [stBoolean] := 'stBoolean';
  PSuperTypeNames^ [stDouble] := 'stDouble';
  PSuperTypeNames^ [stCurrency] := 'stCurrency';
  PSuperTypeNames^ [stInt] := 'stInt';
  PSuperTypeNames^ [stObject] := 'stObject';
  PSuperTypeNames^ [stArray] := 'stArray';
  PSuperTypeNames^ [stString] := 'stString';

end.
