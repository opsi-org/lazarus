unit jan_wmi;

{$mode objfpc}{$H+}

interface

uses
  Classes;//, contnrs;

type

  { TWMIClass }

  TWMIClass = class(TObject)
  constructor Create;overload;
  //constructor Create(const fWMIClassName:String; const fWMINameSpace:WideString='root\CIMV2');overload;
  destructor Destroy;override;
  function ConnectToWMIService(const fComputer:WideString='localhost';
     const fNameSpace:WideString='\root\CIMV2'; const fUser:WideString='';const fPassword:WideString=''):boolean;
  function VarArrayToStr(Value: Variant): String;
  procedure RequestToWMIService(fWMIProperties:String;fWMIClassName:String; const fWMICondition:String='');
  procedure InitWMIClassNames();
  procedure InitWMIProperties(fWMIClassName:String);
  private
    { private declaration }
    FSWbemLocator : OLEVariant;
    FWMIService   : OLEVariant;
    //ServiceConnected : boolean;
  public
    { public declaration }
    WMIClassNames:TStringList;//holding WMI Classnames with Alias: Alias=WMIClassname
    WMIPropertyNames:TStringList;//holding WMI PropertyNames of selected WMI Class
    WMIRequestResult:TStringList;//holding result of query to WMI
    ErrorMsg: String;
  end;

implementation
uses
  SysUtils,
  ActiveX,
  ComObj,
  Variants;


{ TWMIClass }

constructor TWMIClass.Create;
begin
  inherited;
  //ServiceConnected := false;
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');//OLE Object
  WMIClassNames := TStringList.Create;
  WMIPropertyNames := TStringList.Create;
  WMIRequestResult := TStringList.Create;
  InitWMIClassNames();//getting WMI Class names
  //ServiceConnected := ConnectToWMIService();
end;


destructor TWMIClass.Destroy;
begin
  //cleaning activities
  WMIClassNames.Free;
  WMIPropertyNames.Free;
  WMIRequestResult.Free;
  inherited;
end;

function TWMIClass.ConnectToWMIService(const fComputer: WideString;
  const fNameSpace: WideString; const fUser: WideString; const fPassword: WideString
  ): boolean;
begin
  try
    FWMIService := FSWbemLocator.ConnectServer(fComputer, fNameSpace, fUser, fPassword);
    ConnectToWMIService := true;
  except
    on e: Exception do
    begin
      ConnectToWMIService := false;
      ErrorMsg := 'Could not connect to WMI Service';
      Raise;
    end;
  end;
end;

(* VarArrayToStr modified from https://forum.lazarus.freepascal.org/index.php?topic=24490.0
https://forum.lazarus.freepascal.org/index.php?action=dlattach;topic=24490.0;attach=17388
Copyright (c) 2016 Jurassic Pork - Molly*)
function TWMIClass.VarArrayToStr(Value: Variant): String;
//Converts variant array to string
var
  i : Integer;
begin
  try
    Result := '[';
    for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
    begin
      if Result <> '[' then Result := Result + ',';
      if not VarIsNull(Value[i]) then
      begin
        if VarIsArray(Value[i])
        then Result := Result + VarArrayToStr(Value[i])
        else Result := Result + VarToStr(Value[i])
      end
      else Result := Result + '<null>';
    end;
    Result := Result + ']';
  except
    on E: Exception do
    begin
      ErrorMsg := 'Exception while converting variant array to string : ' + Result +
          ' Message: ' + E.Message;
      Raise;
    end;
  end;
end;

procedure TWMIClass.RequestToWMIService(fWMIProperties: String;
  fWMIClassName: String; const fWMICondition: String='' );
const
  FlagForwardOnly = $00000020;//see
var
  WMIObjectSet: OLEVariant;
  WMIObject   : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;
  Query         : WideString;
  WMIPropertiesArray: TStringArray;
  i : integer;
  ResultString : String;
begin
  try
    WMIRequestResult.clear;
    if fWMIProperties <> '' then
    begin
      WMIPropertiesArray:= fWMIProperties.Split(',');
      // Construct Query without or with condition
      if fWMICondition = '' then
         Query := WideString(Format('SELECT %s FROM %s',[fWMIProperties, fWMIClassName]))
      else
        Query := WideString(Format('SELECT %s FROM %s %s',[fWMIProperties, fWMIClassName,fWMICondition]));
      //Query to WMI Service
      WMIObjectSet:= FWMIService.ExecQuery(Query,'WQL',FlagForwardOnly);
      //Enumerate on WMIObjectSet
      oEnum         := IUnknown(WMIObjectSet._NewEnum) as IEnumVariant;
      while oEnum.Next(1, WMIObject, iValue) = 0 do
      begin
        for i := 0 to length(WMIPropertiesArray)-1 do
        begin
          // A value need not to be available for every WMI Property
          If not VarIsNull(WMIObject.Properties_.Item(WideString(WMIPropertiesArray[i])).value) then
          begin
            //value is array or not?
            if VarIsArray(WMIObject.Properties_.Item(WideString(WMIPropertiesArray[i])).value)
            then ResultString := VarArrayToStr(WMIObject.Properties_.Item(WideString(WMIPropertiesArray[i])).value)
            else ResultString := VarToStr(WMIObject.Properties_.Item(WideString(WMIPropertiesArray[i])).value)
          end
          else ResultString := '<null>';
          WMIRequestResult.Add(Format(WMIPropertiesArray[i] +'=%s',[ResultString]));
        //fResultList.Add(Format(WMIObject.Name +'=%s',[String(WMIObject.Properties_.Item('Manufacturer').value)]));// String
        //fResultList.Add(Format('Model=%s',[String(WMIObject.Model)]));// String
        //fResultList.Add(Format('Model=%s',[String(WMIObject.Properties_.Items)]));// String
        end;
        WMIObject:=Unassigned;
      end;
    end
    else
    begin
      WMIRequestResult.Add('No WMI Property selected! Please select at least one.');
    end;
  except
    on E: Exception do
    begin
      ErrorMsg := 'Exception while requesting WMI: ' + String(Query) +
          ' Message: ' + E.Message;
      Raise;
    end;
  end;
end;

procedure TWMIClass.InitWMIClassNames();
var
  i :integer;
begin
  try
    WMIClassNames.Clear;
    if ConnectToWMIService('localhost','\root\cli') then
    begin
      RequestToWMIService('FriendlyName,Target','Msft_CliAlias');
      WMIClassNames.Assign(WMIRequestResult);
      WMIRequestResult.Clear;
      i := 0; //counter for loop
      //extraction Aliases and WMIClassNames from resulting StringList
      while i < WMIClassNames.Count-1 do
      begin
        WMIClassNames[i] := WMIClassNames[i].Remove(0,Length('FriendlyName='));
        WMIClassNames[i] := WMIClassNames[i] + '='
                         + WMIClassNames[i+1].Remove(0,Length('Target=Select * from '));
        WMIClassNames.Delete(i+1);
        i := i + 1;
      end;
    end
    else WMIClassNames.Add('No connection to service!');
  except
    on E: Exception do
    begin
      ErrorMsg := 'Exception while reading class names from WMI: '
               +'localhost,\root\cli,FriendlyName,Target,Msft_CliAlias'
               +' Message: ' + E.Message;
      Raise;
    end;
  end;
end;

procedure TWMIClass.InitWMIProperties(fWMIClassName:String);
const
  FlagForwardOnly = $00000020;
var
  WMIObjectSet: OLEVariant;
  WMIObject,WMI_Item   : OLEVariant;
  oEnum,oEnum2         : IEnumvariant;
  iValue,iValue2        : LongWord;
  Query         : WideString;
begin
  WMIPropertyNames.Clear;//if there where Property Names in list clear list
  try
    Query := WideString(Format('SELECT %s FROM %s'   , ['*', fWMIClassName]));//select all properties
    WMIObjectSet:= FWMIService.ExecQuery(Query,'WQL',FlagForwardOnly);
    oEnum         := IUnknown(WMIObjectSet._NewEnum) as IEnumVariant;
    if oEnum.Next(1, WMIObject, iValue) = 0 then   //only first Instance of WMIObjectSet
    begin
      oEnum2 := IUnknown(WMIObject.Properties_._NewEnum) as IEnumVariant;//enumerate Properties
      while oEnum2.Next(1, WMI_Item, iValue2) = 0 do
      begin
        WMIPropertyNames.Add(WMI_Item.Name);
        WMI_Item:=Unassigned;
      end;
      WMIObject:=Unassigned;
    end
    else
    begin
      WMIPropertyNames.Add('Not available.');
      WMIPropertyNames.Add('No instance of ' + fWMIClassName + '.');
    end;
  except
    on E: Exception do
    begin
      WMIObjectSet := Unassigned;
      ErrorMsg := 'Exception while reading WMI property names: ' + String(Query)+#13#10 +
          'Message: ' + E.Message;
      Raise;
    end;
  end;
end;

end.

//Container
{procedure  GetWin32_VideoControllerInfo;
const
  WbemUser            ='';
  WbemPassword        ='';
  WbemComputer        ='localhost';
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject   : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;
begin;
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer(WbemComputer, 'root\CIMV2', WbemUser, WbemPassword);
  FWbemObjectSet:= FWMIService.ExecQuery('SELECT Name,PNPDeviceID  FROM Win32_VideoController','WQL',wbemFlagForwardOnly);
  oEnum         := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  while oEnum.Next(1, FWbemObject, iValue) = 0 do
  begin
    Writeln(Format('Name           %s',[String(FWbemObject.Name)]));// String
    Writeln(Format('PNPDeviceID    %s',[String(FWbemObject.PNPDeviceID)]));// String
    Writeln;
    FWbemObject:=Unassigned;
  end;
end;}

