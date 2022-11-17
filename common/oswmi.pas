unit oswmi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  utilwmi,
  contnrs;

function osGetWMI(WMINamespace: string; WMIClass: string;
  WMIPropertyList: TStrings; WMICondition: string; var WMIResultList: TStringList;
  var ErrorMsg: string): boolean;

implementation

function osGetWMI(WMINamespace: string; WMIClass: string;
  WMIPropertyList: TStrings; WMICondition: string; var WMIResultList: TStringList;
  var ErrorMsg: string): boolean;
var
  WMIResult: TFPObjectList;
  i: integer;
  PropNames: array of string;
  Request: string;
begin
  Result := False;
  try
    try
      for i := 0 to WMIPropertyList.Count - 1 do
      begin
        SetLength(PropNames, i + 1);
        PropNames[i] := WMIPropertyList.strings[i];
      end;
      if (WMICondition = '') and (WMIPropertyList.Count = 0) then
        WMIResult := GetWMIInfoClass(WMINamespace, WMIClass)
      else
        WMIResult := GetWMIInfo(WMINamespace, WMIClass, PropNames, WMICondition, Request);
      WMIResultList.Clear;
      for i := 0 to Pred(WMIResult.Count) do
      begin
        //WMIResultList.Add(TStringList(WMIResult.Items[i]).Text);
        WMIResultList.AddStrings(TStringList(WMIResult.Items[i]));
      end;
      Result := True;
    except
      on E: Exception do
      begin
        ErrorMsg := ErrorMsg + 'Exception while calling WMI: ' +
          Request + ' Message: ' + E.Message;
        WMIResult := nil;
      end;
    end;
  finally
    if Assigned(WMIResult) then
      FreeAndNil(WMIResult);
    SetLength(PropNames, 0);
  end;

end;

end.
