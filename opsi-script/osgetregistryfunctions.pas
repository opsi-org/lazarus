unit osGetRegistryFunctions;

{$mode Delphi}

interface

uses
  Classes, SysUtils, osregistry, ostxstringlist, oslog;

function CheckRegistryListOrMap(SmallRegistryCommand: string): boolean;
procedure GetRegistryListOrMap(RegistryKey: string; SmallRegistryCommand: string;
  var list: TXStringList);

function CheckSummedRegistryListOrMap(SmallRegistryCommand: string): boolean;
function CheckAccessString(SmallAccessString: string): boolean;
procedure GetSummedRegistryListOrMap(RegistryKey: string; SmallAccessString: string;
  SmallRegistryCommand: string; var list: TXStringList);

implementation

function CheckRegistryListOrMap(SmallRegistryCommand: string): boolean;
begin
  if (SmallRegistryCommand = LowerCase('getRegistryKeyList32')) or
    (SmallRegistryCommand = LowerCase('getRegistryKeyList64')) or
    (SmallRegistryCommand = LowerCase('getRegistryKeyListSysnative')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarList32')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarList64')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarListSysnative')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarMap32')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarMap64')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarMapSysnative')) then
    Result := True
  else
    Result := False;
end;

procedure GetRegistryListOrMap(RegistryKey: string; SmallRegistryCommand: string;
  var list: TXStringList);
begin
  if (SmallRegistryCommand = LowerCase('getRegistryKeyList32')) then
    list.AddStrings(GetRegistryKeyList(RegistryKey, False))

  else if (SmallRegistryCommand = LowerCase('getRegistryKeyList64')) or
    (SmallRegistryCommand = LowerCase('getRegistryKeyListSysnative')) then
    list.AddStrings(GetRegistryKeyList(RegistryKey, True))

  else if SmallRegistryCommand = LowerCase('getRegistryVarList32') then
    list.AddStrings(GetRegistryVarList(RegistryKey, False))

  else if (SmallRegistryCommand = LowerCase('getRegistryVarList64')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarListSysnative')) then
    list.AddStrings(GetRegistryVarList(RegistryKey, True))

  else if SmallRegistryCommand = LowerCase('getRegistryVarMap32') then
    list.AddStrings(GetRegistryVarMap(RegistryKey, False))

  else if (SmallRegistryCommand = LowerCase('getRegistryVarMap64')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarMapSysnative')) then
    list.AddStrings(GetRegistryVarMap(RegistryKey, True));
end;


function CheckSummedRegistryListOrMap(SmallRegistryCommand: string): boolean;
begin
  if (SmallRegistryCommand = LowerCase('getRegistryKeyList')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarList')) or
    (SmallRegistryCommand = LowerCase('getRegistryVarMap')) then
    Result := True
  else
    Result := False;
end;

function CheckAccessString(SmallAccessString: string): boolean;
begin
  if (SmallAccessString = LowerCase('32Bit')) or (SmallAccessString =
    LowerCase('64Bit')) or (SmallAccessString = LowerCase('Sysnative')) then
    Result := True
  else
    Result := False;
end;

procedure GetSummedRegistryListOrMap(RegistryKey: string; SmallAccessString: string;
  SmallRegistryCommand: string; var list: TXStringList);
begin
  if (SmallRegistryCommand = LowerCase('getRegistryKeyList')) then
  begin
    if SmallAccessString = LowerCase('32Bit') then
      list.AddStrings(GetRegistryKeyList(RegistryKey, False))
    else if (SmallAccessString = LowerCase('64Bit')) or
      (SmallAccessString = LowerCase('Sysnative')) then
      list.AddStrings(GetRegistryKeyList(RegistryKey, True))
  end

  else if (SmallRegistryCommand = LowerCase('getRegistryVarList')) then
  begin
    if SmallAccessString = LowerCase('32Bit') then
      list.AddStrings(GetRegistryVarList(RegistryKey, False))
    else if (SmallAccessString = LowerCase('64Bit')) or
      (SmallAccessString = LowerCase('Sysnative')) then
      list.AddStrings(GetRegistryVarList(RegistryKey, True))
  end

  else if (SmallRegistryCommand = LowerCase('getRegistryVarMap')) then
  begin
    if SmallAccessString = LowerCase('32Bit') then
      list.AddStrings(GetRegistryVarMap(RegistryKey, False))
    else if (SmallAccessString = LowerCase('64Bit')) or
      (SmallAccessString = LowerCase('Sysnative')) then
      list.AddStrings(GetRegistryVarMap(RegistryKey, True))
  end;
end;

end.
