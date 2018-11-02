unit oswmi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  utilwmi,
  contnrs;

function osGetWMI(WMIClass : string; WMIPropertyList : Tstrings; WMICondition : string; var WMIResultList : Tstringlist; var ErrorMsg : string) : boolean;

implementation

function osGetWMI(WMIClass : string; WMIPropertyList : Tstrings; WMICondition : string; var WMIResultList : Tstringlist; var ErrorMsg : string) : boolean;
var
  WMIResult : TFPObjectList;
  i : integer;
  PropNames         : Array of String;
  Request       : String;
begin
  result := false;
  try
  try
    for i := 0 to WMIPropertyList.count -1 do
    begin
     SetLength(PropNames, i + 1);
     PropNames[i] := WMIPropertyList.strings[i];
    end;
    //WMIResult := GetWMIInfo(WMIClass, PropNames,WMICondition,Request);
    WMIResult := GetWMIInfoClass(WMIClass);
    WMIResultList.Clear;
    for i := 0 to Pred(WMIResult.Count) do
    begin
      WMIResultList.Add(TStringlist(WMIResult.Items[i]).Text);
    end;
    result := true;
  except
    on E : Exception  do
    begin
      ErrorMsg := ErrorMsg + 'Exception while calling WMI: '+Request+' Message: '+E.Message;
      WMIResult := nil;
    end;
  end;
  finally
    if Assigned(WMIResult) then FreeandNil(WMIResult);
    SetLength(PropNames, 0);
  end;

end;

end.

