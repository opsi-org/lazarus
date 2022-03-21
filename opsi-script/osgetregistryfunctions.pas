unit osGetRegistryFunctions;

{$mode Delphi}

interface

uses
  Classes, SysUtils, oslog, osregistry, ostxstringlist;

function CheckRegistryListOrMap(SmallRegistryKey: string): boolean;
procedure GetRegistryListOrMap(s1: string; SmallRegistryKey: string; var list: TXStringList);

implementation

function CheckRegistryListOrMap(SmallRegistryKey: string): boolean;
begin
  if (SmallRegistryKey = LowerCase('getRegistryKeyList32')) or
    (SmallRegistryKey = LowerCase('getRegistryKeyList64')) or
    (SmallRegistryKey = LowerCase('getRegistryKeyListSysnative')) or
    (SmallRegistryKey = LowerCase('getRegistryVarList32')) or
    (SmallRegistryKey = LowerCase('getRegistryVarList64')) or
    (SmallRegistryKey = LowerCase('getRegistryVarListSysnative')) or
    (SmallRegistryKey = LowerCase('getRegistryVarMap32')) or
    (SmallRegistryKey = LowerCase('getRegistryVarMap64')) or
    (SmallRegistryKey = LowerCase('getRegistryVarMapSysnative')) then Result := True
  else
    Result := False;
end;

procedure GetRegistryListOrMap(s1: string; SmallRegistryKey: string; var list: TXStringList);
begin
  if (SmallRegistryKey = LowerCase('getRegistryKeyList32')) then
    list.AddStrings(GetRegistryKeyList(s1, False))

  else if (SmallRegistryKey = LowerCase('getRegistryKeyList64')) or
    (SmallRegistryKey = LowerCase('getRegistryKeyListSysnative')) then
    list.AddStrings(GetRegistryKeyList(s1, True))

  else if SmallRegistryKey = LowerCase('getRegistryVarList32') then
    list.AddStrings(GetRegistryVarList(s1, False))

  else if (SmallRegistryKey = LowerCase('getRegistryVarList64')) or
    (SmallRegistryKey = LowerCase('getRegistryVarListSysnative')) then
    list.AddStrings(GetRegistryVarList(s1, True))

  else if SmallRegistryKey = LowerCase('getRegistryVarMap32') then
    list.AddStrings(GetRegistryVarMap(s1, False))

  else if (SmallRegistryKey = LowerCase('getRegistryVarMap64')) or
    (SmallRegistryKey = LowerCase('getRegistryVarMapSysnative')) then
    list.AddStrings(GetRegistryVarMap(s1, True));
end;

end.
