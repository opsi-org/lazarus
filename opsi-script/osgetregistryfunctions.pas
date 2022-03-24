unit osGetRegistryFunctions;

{$mode Delphi}

interface

uses
  Classes, SysUtils, osregistry, ostxstringlist, oslog;

function IsGetRegistryListOrMapFunction(FunctionName: string): boolean;

function IsGetRegistryKeyList32(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryKeyList64(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryKeyListSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryKeyList32or64orSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string; var noredirect: boolean): boolean;

function IsGetRegistryVarList32(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryVarList64(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryVarListSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryVarList32or64orSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string; var noredirect: boolean): boolean;

function IsGetRegistryVarMap32(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryVarMap64(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryVarMapSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
function IsGetRegistryVarMap32or64orSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string; var noredirect: boolean): boolean;

procedure RunGetRegistryListOrMapFunction(FunctionName: string;
  RegistryKey: string; AccessString: string; var list: TXStringList);

function CheckAccessString(AccessString: string): boolean;

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
    (LowerCaseFunctionName = LowerCase('getRegistryVarMapSysnative')) or
    (LowerCaseFunctionName = LowerCase('getRegistryKeyList')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarList')) or
    (LowerCaseFunctionName = LowerCase('getRegistryVarMap')) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryKeyList32(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryKeyList32')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryKeyList')) and
    (LowerCaseAccessString = LowerCase('32Bit'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryKeyList64(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryKeyList64')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryKeyList')) and
    (LowerCaseAccessString = LowerCase('64Bit'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryKeyListSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryKeyListSysnative')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryKeyList')) and
    (LowerCaseAccessString = LowerCase('Sysnative'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryKeyList32or64orSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string; var NoRedirect: boolean): boolean;
begin
  Result := True;
  if IsGetRegistryKeyList32(LowerCaseFunctionName, LowerCaseAccessString) then
    NoRedirect := False
  else if IsGetRegistryKeyList64(LowerCaseFunctionName, LowerCaseAccessString) or
    IsGetRegistryKeyListSysnative(LowerCaseFunctionName, LowerCaseAccessString) then
    NoRedirect := True
  else
    Result := False;
end;

function IsGetRegistryVarList32(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryVarList32')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryVarList')) and
    (LowerCaseAccessString = LowerCase('32Bit'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryVarList64(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryVarList64')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryVarList')) and
    (LowerCaseAccessString = LowerCase('64Bit'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryVarListSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryVarListSysnative')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryVarList')) and
    (LowerCaseAccessString = LowerCase('Sysnative'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryVarList32or64orSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string; var NoRedirect: boolean): boolean;
begin
  Result := True;
  if IsGetRegistryVarList32(LowerCaseFunctionName, LowerCaseAccessString) then
    NoRedirect := False
  else if IsGetRegistryVarList64(LowerCaseFunctionName, LowerCaseAccessString) or
    IsGetRegistryVarListSysnative(LowerCaseFunctionName, LowerCaseAccessString) then
    NoRedirect := True
  else
    Result := False;
end;

function IsGetRegistryVarMap32(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryVarMap32')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryVarMap')) and
    (LowerCaseAccessString = LowerCase('32Bit'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryVarMap64(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryVarMap64')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryVarMap')) and
    (LowerCaseAccessString = LowerCase('64Bit'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryVarMapSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string): boolean;
begin
  if ((LowerCaseFunctionName = LowerCase('getRegistryVarMapSysnative')) and
    (LowerCaseAccessString = '')) or
    ((LowerCaseFunctionName = LowerCase('getRegistryVarMap')) and
    (LowerCaseAccessString = LowerCase('Sysnative'))) then
    Result := True
  else
    Result := False;
end;

function IsGetRegistryVarMap32or64orSysnative(LowerCaseFunctionName: string;
  LowerCaseAccessString: string; var NoRedirect: boolean): boolean;
begin
  Result := True;
  if IsGetRegistryVarMap32(LowerCaseFunctionName, LowerCaseAccessString) then
    NoRedirect := False
  else if IsGetRegistryVarMap64(LowerCaseFunctionName, LowerCaseAccessString) or
    IsGetRegistryVarMapSysnative(LowerCaseFunctionName, LowerCaseAccessString) then
    NoRedirect := True
  else
    Result := False;
end;


procedure RunGetRegistryListOrMapFunction(FunctionName: string;
  RegistryKey: string; AccessString: string; var list: TXStringList);
var
  LowerCaseFunctionName: string;
  LowerCaseAccessString: string;
  NoRedirect: boolean;
begin
  LowerCaseFunctionName := LowerCase(FunctionName);
  LowerCaseAccessString := LowerCase(AccessString);

  if IsGetRegistryKeyList32or64orSysnative(LowerCaseFunctionName,
    LowerCaseAccessString, NoRedirect) then
    list.AddStrings(GetRegistryKeyList(RegistryKey, NoRedirect))

  else if IsGetRegistryVarList32or64orSysnative(LowerCaseFunctionName,
    LowerCaseAccessString, NoRedirect) then
    list.AddStrings(GetRegistryVarList(RegistryKey, NoRedirect))

  else if IsGetRegistryVarMap32or64orSysnative(LowerCaseFunctionName,
    LowerCaseAccessString, NoRedirect) then
    list.AddStrings(GetRegistryVarMap(RegistryKey, NoRedirect));
end;


function CheckAccessString(AccessString: string): boolean;
var
  LowerCaseAccessString: string;
begin
  LowerCaseAccessString := LowerCase(AccessString);
  if (LowerCaseAccessString = LowerCase('32Bit')) or
    (LowerCaseAccessString = LowerCase('64Bit')) or
    (LowerCaseAccessString = LowerCase('Sysnative')) then
    Result := True
  else
    Result := False;
end;


end.
