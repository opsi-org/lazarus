unit osGetRegistryFunctions;

{$mode Delphi}

interface

uses
  Classes, SysUtils, osregistry, ostxstringlist, oslog;

function IsGetRegistryListOrMapFunction(FunctionName: string): boolean;
procedure RunGetRegistryListOrMapFunction(RegistryKey: string; FunctionName: string;
  var list: TXStringList);

function CheckAccessString(AccessString: string): boolean;
procedure GetSummedRegistryListOrMap(RegistryKey: string; AccessString: string;
  FunctionName: string; var list: TXStringList);

implementation

function IsGetRegistryListOrMapFunction(FunctionName: string): boolean;
var
  LowerCaseFunctionName: string;
begin
  LowerCaseFunctionName := LowerCase(FunctionName);
  if (LowerCaseFunctionName = LowerCase('getRegistryKeyList32')) or
    (LowerCaseFunctionName = LowerCase('getRegistryKeyList64')) or
    (LowerCaseFunctionName = LowerCase('getRegistryKeyListSysnative')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarList32')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarList64')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarListSysnative')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarMap32')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarMap64')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarMapSysnative'))or
    (LowerCaseFunctionName = LowerCase('getRegistryKeyList')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarList')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarMap')) then
    Result := True
  else
    Result := False;
end;

procedure RunGetRegistryListOrMapFunction(RegistryKey: string; FunctionName: string;
  var list: TXStringList);
var
  LowerCaseFunctionName: string;
begin
  LowerCaseFunctionName := LowerCase(FunctionName);
  if (LowerCaseFunctionName = LowerCase('getRegistryKeyList32')) then
    list.AddStrings(GetRegistryKeyList(RegistryKey, False))

  else if (LowerCaseFunctionName = LowerCase('getRegistryKeyList64')) or
    (LowerCaseFunctionName = LowerCase('getRegistryKeyListSysnative')) then
    list.AddStrings(GetRegistryKeyList(RegistryKey, True))

  else if LowerCaseFunctionName = LowerCase('getRegistryVarList32') then
    list.AddStrings(GetRegistryVarList(RegistryKey, False))

  else if (LowerCaseFunctionName = LowerCase('getRegistryVarList64')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarListSysnative')) then
    list.AddStrings(GetRegistryVarList(RegistryKey, True))

  else if LowerCaseFunctionName = LowerCase('getRegistryVarMap32') then
    list.AddStrings(GetRegistryVarMap(RegistryKey, False))

  else if (LowerCaseFunctionName = LowerCase('getRegistryVarMap64')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarMapSysnative')) then
    list.AddStrings(GetRegistryVarMap(RegistryKey, True));
end;


function CheckAccessString(AccessString: string): boolean;
var
  LowerCaseAccessString: string;
begin
  LowerCaseAccessString := LowerCase(AccessString);
  if (LowerCaseAccessString = LowerCase('32Bit')) or (LowerCaseAccessString =
    LowerCase('64Bit')) or (LowerCaseAccessString = LowerCase('Sysnative')) then
    Result := True
  else
    Result := False;
end;

procedure GetSummedRegistryListOrMap(RegistryKey: string; AccessString: string;
  FunctionName: string; var list: TXStringList);
var
  LowerCaseAccessString: string;
  LowerCaseFunctionName: string;
begin
  LowerCaseAccessString := LowerCase(AccessString);
  LowerCaseFunctionName := LowerCase(FunctionName);
  if (LowerCaseFunctionName = LowerCase('getRegistryKeyList')) then
  begin
    if LowerCaseAccessString = LowerCase('32Bit') then
      list.AddStrings(GetRegistryKeyList(RegistryKey, False))
    else if (LowerCaseAccessString = LowerCase('64Bit')) or
      (LowerCaseAccessString = LowerCase('Sysnative')) then
      list.AddStrings(GetRegistryKeyList(RegistryKey, True))
  end

  else if (LowerCaseFunctionName = LowerCase('getRegistryVarList')) then
  begin
    if LowerCaseAccessString = LowerCase('32Bit') then
      list.AddStrings(GetRegistryVarList(RegistryKey, False))
    else if (LowerCaseAccessString = LowerCase('64Bit')) or
      (LowerCaseAccessString = LowerCase('Sysnative')) then
      list.AddStrings(GetRegistryVarList(RegistryKey, True))
  end

  else if (LowerCaseFunctionName = LowerCase('getRegistryVarMap')) then
  begin
    if LowerCaseAccessString = LowerCase('32Bit') then
      list.AddStrings(GetRegistryVarMap(RegistryKey, False))
    else if (LowerCaseAccessString = LowerCase('64Bit')) or
      (LowerCaseAccessString = LowerCase('Sysnative')) then
      list.AddStrings(GetRegistryVarMap(RegistryKey, True))
  end;
end;

end.
