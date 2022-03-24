unit osGetRegistryFunctions;

{$mode Delphi}

interface

uses
  Classes, SysUtils, osregistry, ostxstringlist, oslog;

type
  TGetRegistryListOrMapCall = class(TObject)
  private
    FLowerCaseFunctionName: string;
    FLowerCaseAccessString: string;

    function IsGetRegistryKeyList32: boolean;
    function IsGetRegistryKeyList64: boolean;
    function IsGetRegistryKeyListSysnative: boolean;
    function IsGetRegistryKeyList32or64orSysnative(var noredirect: boolean): boolean;

    function IsGetRegistryVarList32: boolean;
    function IsGetRegistryVarList64: boolean;
    function IsGetRegistryVarListSysnative: boolean;
    function IsGetRegistryVarList32or64orSysnative(var noredirect: boolean): boolean;

    function IsGetRegistryVarMap32: boolean;
    function IsGetRegistryVarMap64: boolean;
    function IsGetRegistryVarMapSysnative: boolean;
    function IsGetRegistryVarMap32or64orSysnative(var noredirect: boolean): boolean;

  protected
    constructor Create(FunctionName: string; AccessString: string); overload;
  end;


function IsGetRegistryListOrMapFunction(FunctionName: string): boolean;
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

procedure RunGetRegistryListOrMapFunction(FunctionName: string;
  RegistryKey: string; AccessString: string; var list: TXStringList);
var
  NoRedirect: boolean = True;
  GetRegistryListOrMapCall: TGetRegistryListOrMapCall;
begin
  GetRegistryListOrMapCall := TGetRegistryListOrMapCall.Create(FunctionName,
    AccessString);

  if GetRegistryListOrMapCall.IsGetRegistryKeyList32or64orSysnative(NoRedirect) then
    list.AddStrings(GetRegistryKeyList(RegistryKey, NoRedirect))

  else if GetRegistryListOrMapCall.IsGetRegistryVarList32or64orSysnative(NoRedirect) then
    list.AddStrings(GetRegistryVarList(RegistryKey, NoRedirect))

  else if GetRegistryListOrMapCall.IsGetRegistryVarMap32or64orSysnative(NoRedirect) then
    list.AddStrings(GetRegistryVarMap(RegistryKey, NoRedirect));

  GetRegistryListOrMapCall.Free;
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


{GetRegistryListOrMapCall}

constructor TGetRegistryListOrMapCall.Create(FunctionName: string;
  AccessString: string); overload;
begin
  FLowerCaseFunctionName := LowerCase(FunctionName);
  FLowerCaseAccessString := LowerCase(AccessString);
end;


function TGetRegistryListOrMapCall.IsGetRegistryKeyList32: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryKeyList32')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryKeyList')) and
    (FLowerCaseAccessString = LowerCase('32Bit'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryKeyList64: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryKeyList64')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryKeyList')) and
    (FLowerCaseAccessString = LowerCase('64Bit'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryKeyListSysnative: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryKeyListSysnative')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryKeyList')) and
    (FLowerCaseAccessString = LowerCase('Sysnative'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryKeyList32or64orSysnative(
  var NoRedirect: boolean): boolean;
begin
  Result := True;
  if IsGetRegistryKeyList32 then
    NoRedirect := False
  else if IsGetRegistryKeyList64 or
    IsGetRegistryKeyListSysnative then
    NoRedirect := True
  else
    Result := False;
end;


function TGetRegistryListOrMapCall.IsGetRegistryVarList32: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryVarList32')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryVarList')) and
    (FLowerCaseAccessString = LowerCase('32Bit'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryVarList64: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryVarList64')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryVarList')) and
    (FLowerCaseAccessString = LowerCase('64Bit'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryVarListSysnative: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryVarListSysnative')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryVarList')) and
    (FLowerCaseAccessString = LowerCase('Sysnative'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryVarList32or64orSysnative(
  var NoRedirect: boolean): boolean;
begin
  Result := True;
  if IsGetRegistryVarList32 then
    NoRedirect := False
  else if IsGetRegistryVarList64 or
    IsGetRegistryVarListSysnative then
    NoRedirect := True
  else
    Result := False;
end;


function TGetRegistryListOrMapCall.IsGetRegistryVarMap32: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryVarMap32')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryVarMap')) and
    (FLowerCaseAccessString = LowerCase('32Bit'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryVarMap64: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryVarMap64')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryVarMap')) and
    (FLowerCaseAccessString = LowerCase('64Bit'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryVarMapSysnative: boolean;
begin
  if ((FLowerCaseFunctionName = LowerCase('getRegistryVarMapSysnative')) and
    (FLowerCaseAccessString = '')) or
    ((FLowerCaseFunctionName = LowerCase('getRegistryVarMap')) and
    (FLowerCaseAccessString = LowerCase('Sysnative'))) then
    Result := True
  else
    Result := False;
end;

function TGetRegistryListOrMapCall.IsGetRegistryVarMap32or64orSysnative(
  var NoRedirect: boolean): boolean;
begin
  Result := True;
  if IsGetRegistryVarMap32 then
    NoRedirect := False
  else if IsGetRegistryVarMap64 or
    IsGetRegistryVarMapSysnative then
    NoRedirect := True
  else
    Result := False;
end;

end.
