// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 288 $
// $Author: oertel $
// $Date: 2015-01-29 18:19:14 +0100 (Do, 29 Jan 2015) $
//***************************************************************************

// unit uSMBIOS (TSMBIOS) (https://code.google.com/p/tsmbios/)
// integrated mit explicit permission of the author Rodrigo Ruz

unit opsihwbiosinfo;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, TypInfo, SysUtils, uSMBIOS , CustApp , INIFiles
  { you can add units after this };

function getHwBiosShortlist : TStringlist;

implementation
var
outlist : Tstringlist;


//http://tondrej.blogspot.com/2007/10/settostring-stringtoset.html
function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
begin
  Result := 0;

  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Result := Byte(SetParam);
    otSWord, otUWord:
      Result := Word(SetParam);
    otSLong, otULong:
      Result := Integer(SetParam);
  end;
end;


function SetToString(Info: PTypeInfo; const SetParam; Brackets: Boolean): String;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result := '';

  Integer(S) := GetOrdValue(Info, SetParam);
  TypeInfo := GetTypeData(Info)^.CompType;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

function ByteToBinStr(AValue:Byte):string;
const
  Bits : array[1..8] of byte = (128,64,32,16,8,4,2,1);
  var i: integer;
begin
  Result:='00000000';
  if (AValue<>0) then
  for i:=1 to 8 do
    if (AValue and Bits[i])<>0 then Result[i]:='1';
end;

procedure GetElectricalCurrProbeInfo;
Var
  SMBios : TSMBios;
  LElectCurrProbeInfo  : TElectricalCurrentProbeInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Electrical Current Probe Information');
      WriteLn('-----------------------------------');
      if SMBios.HasElectricalCurrentProbeInfo then
      for LElectCurrProbeInfo in SMBios.ElectricalCurrentProbeInformation do
      begin
        WriteLn(Format('Description    %s',[LElectCurrProbeInfo.GetDescriptionStr]));
        WriteLn(Format('Location and Status %s',[ByteToBinStr(LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.LocationandStatus)]));
        WriteLn(Format('Location       %s',[LElectCurrProbeInfo.GetLocation]));
        WriteLn(Format('Status         %s',[LElectCurrProbeInfo.GetStatus]));

        if LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.MaximumValue=$8000 then
          WriteLn(Format('Maximum Value  %s',['Unknown']))
        else
          WriteLn(Format('Maximum Value  %d milliamps.°',[LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.MaximumValue]));
        if LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.MinimumValue=$8000 then
          WriteLn(Format('Minimum Value  %s',['Unknown']))
        else
          WriteLn(Format('Minimum Value  %d milliamps.',[LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.MinimumValue]));

        if LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.Resolution=$8000 then
          WriteLn(Format('Resolution     %s',['Unknown']))
        else
          WriteLn(Format('Resolution     %d milliamps.',[LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.Resolution div 10]));

        if LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.Tolerance=$8000 then
          WriteLn(Format('Tolerance      %s',['Unknown']))
        else
          WriteLn(Format('Tolerance      %n milliamps.',[LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.Tolerance]));
        WriteLn(Format('OEM Specific   %.8x',[LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.OEMdefined]));

        if LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.Header.Length>$14 then
        if LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.NominalValue=$8000 then
          WriteLn(Format('Nominal Value  %s',['Unknown']))
        else
          WriteLn(Format('Nominal Value  %d milliamps.',[LElectCurrProbeInfo.RAWElectricalCurrentProbeInfo^.NominalValue]));
        WriteLn;
      end
      else
      Writeln('No Electrical Current Probe Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetTempProbeInfo;
Var
  SMBios : TSMBios;
  LTempProbeInfo  : TTemperatureProbeInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Temperature Probe Information');
      WriteLn('-----------------------------');
      if SMBios.HasTemperatureProbeInfo then
      for LTempProbeInfo in SMBios.TemperatureProbeInformation do
      begin
        WriteLn(Format('Description    %s',[LTempProbeInfo.GetDescriptionStr]));
        WriteLn(Format('Location and Status %s',[ByteToBinStr(LTempProbeInfo.RAWTemperatureProbeInfo^.LocationandStatus)]));
        WriteLn(Format('Location       %s',[LTempProbeInfo.GetLocation]));
        WriteLn(Format('Status         %s',[LTempProbeInfo.GetStatus]));

        if LTempProbeInfo.RAWTemperatureProbeInfo^.MaximumValue=$8000 then
          WriteLn(Format('Maximum Value  %s',['Unknown']))
        else
          WriteLn(Format('Maximum Value  %d C°',[LTempProbeInfo.RAWTemperatureProbeInfo^.MaximumValue div 10]));
        if LTempProbeInfo.RAWTemperatureProbeInfo^.MinimumValue=$8000 then
          WriteLn(Format('Minimum Value  %s',['Unknown']))
        else
          WriteLn(Format('Minimum Value  %d C°',[LTempProbeInfo.RAWTemperatureProbeInfo^.MinimumValue div 10]));

        if LTempProbeInfo.RAWTemperatureProbeInfo^.Resolution=$8000 then
          WriteLn(Format('Resolution     %s',['Unknown']))
        else
          WriteLn(Format('Resolution     %d C°',[LTempProbeInfo.RAWTemperatureProbeInfo^.Resolution div 1000]));

        if LTempProbeInfo.RAWTemperatureProbeInfo^.Tolerance=$8000 then
          WriteLn(Format('Tolerance      %s',['Unknown']))
        else
          WriteLn(Format('Tolerance      %n C°',[LTempProbeInfo.RAWTemperatureProbeInfo^.Tolerance / 10]));
        WriteLn(Format('OEM Specific   %.8x',[LTempProbeInfo.RAWTemperatureProbeInfo^.OEMdefined]));

        if LTempProbeInfo.RAWTemperatureProbeInfo^.Header.Length>$14 then
        if LTempProbeInfo.RAWTemperatureProbeInfo^.NominalValue=$8000 then
          WriteLn(Format('Nominal Value  %s',['Unknown']))
        else
          WriteLn(Format('Nominal Value  %d C°',[LTempProbeInfo.RAWTemperatureProbeInfo^.NominalValue div 10]));
        WriteLn;
      end
      else
      Writeln('No Temperature Probe Info was found');
  finally
   SMBios.Free;
  end;
end;



procedure GetCoolingDeviceInfo;
Var
  SMBios : TSMBios;
  LCoolingDevice  : TCoolingDeviceInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Cooling Device Information');
      WriteLn('--------------------------');
      if SMBios.HasCoolingDeviceInfo then
      for LCoolingDevice in SMBios.CoolingDeviceInformation do
      begin
        if LCoolingDevice.RAWCoolingDeviceInfo^.TemperatureProbeHandle<>$FFFF then
          WriteLn(Format('Temperature Probe Handle %.4x',[LCoolingDevice.RAWCoolingDeviceInfo^.TemperatureProbeHandle]));

        WriteLn(Format('Device Type and Status   %s',[ByteToBinStr(LCoolingDevice.RAWCoolingDeviceInfo^.DeviceTypeandStatus)]));
        WriteLn(Format('Type                     %s',[LCoolingDevice.GetDeviceType]));
        WriteLn(Format('Status                   %s',[LCoolingDevice.GetStatus]));

        WriteLn(Format('Cooling Unit Group       %d',[LCoolingDevice.RAWCoolingDeviceInfo^.CoolingUnitGroup]));
        WriteLn(Format('OEM Specific             %.8x',[LCoolingDevice.RAWCoolingDeviceInfo^.OEMdefined]));
        if LCoolingDevice.RAWCoolingDeviceInfo^.NominalSpeed=$8000 then
          WriteLn(Format('Nominal Speed            %s',['Unknown']))
        else
          WriteLn(Format('Nominal Speed            %d rpm',[LCoolingDevice.RAWCoolingDeviceInfo^.NominalSpeed]));
        if SMBios.SmbiosVersion>='2.7' then
        WriteLn(Format('Description    %s',[LCoolingDevice.GetDescriptionStr]));

        WriteLn;
      end
      else
      Writeln('No Cooling Device Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetVoltageProbeInfo;
Var
  SMBios : TSMBios;
  LVoltageProbeInfo  : TVoltageProbeInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Voltage Probe Information');
      WriteLn('-------------------------');
      if SMBios.HasVoltageProbeInfo then
      for LVoltageProbeInfo in SMBios.VoltageProbeInformation do
      begin
        WriteLn(Format('Description    %s',[LVoltageProbeInfo.GetDescriptionStr]));
        WriteLn(Format('Location and Status %s',[ByteToBinStr(LVoltageProbeInfo.RAWVoltageProbeInfo^.LocationandStatus)]));
        WriteLn(Format('Location       %s',[LVoltageProbeInfo.GetLocation]));
        WriteLn(Format('Status         %s',[LVoltageProbeInfo.GetStatus]));

        if LVoltageProbeInfo.RAWVoltageProbeInfo^.MaximumValue=$8000 then
          WriteLn(Format('Maximum Value  %s',['Unknown']))
        else
          WriteLn(Format('Maximum Value  %d',[LVoltageProbeInfo.RAWVoltageProbeInfo^.MaximumValue]));
        if LVoltageProbeInfo.RAWVoltageProbeInfo^.MinimumValue=$8000 then
          WriteLn(Format('Minimum Value  %s',['Unknown']))
        else
          WriteLn(Format('Minimum Value  %d',[LVoltageProbeInfo.RAWVoltageProbeInfo^.MinimumValue]));

        if LVoltageProbeInfo.RAWVoltageProbeInfo^.Resolution=$8000 then
          WriteLn(Format('Resolution     %s',['Unknown']))
        else
          WriteLn(Format('Resolution     %d',[LVoltageProbeInfo.RAWVoltageProbeInfo^.Resolution]));

        if LVoltageProbeInfo.RAWVoltageProbeInfo^.Tolerance=$8000 then
          WriteLn(Format('Tolerance      %s',['Unknown']))
        else
          WriteLn(Format('Tolerance      %d',[LVoltageProbeInfo.RAWVoltageProbeInfo^.Tolerance]));
        WriteLn(Format('OEM Specific   %.8x',[LVoltageProbeInfo.RAWVoltageProbeInfo^.OEMdefined]));

        if LVoltageProbeInfo.RAWVoltageProbeInfo^.Header.Length>$14 then
        if LVoltageProbeInfo.RAWVoltageProbeInfo^.NominalValue=$8000 then
          WriteLn(Format('Nominal Value  %s',['Unknown']))
        else
          WriteLn(Format('Nominal Value  %d',[LVoltageProbeInfo.RAWVoltageProbeInfo^.NominalValue]));
        WriteLn;
      end
      else
      Writeln('No Voltage Probe Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetBatteryInfo;
Var
  SMBios : TSMBios;
  LBatteryInfo  : TBatteryInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Battery Information');
      WriteLn('-------------------');
      if SMBios.HasBatteryInfo then
      for LBatteryInfo in SMBios.BatteryInformation do
      begin
        WriteLn('Location           '+LBatteryInfo.GetLocationStr);
        WriteLn('Manufacturer       '+LBatteryInfo.GetManufacturerStr);
        WriteLn('Manufacturer Date  '+LBatteryInfo.GetManufacturerDateStr);
        WriteLn('Serial Number      '+LBatteryInfo.GetSerialNumberStr);
        WriteLn('Device Name        '+LBatteryInfo.GetDeviceNameStr);
        WriteLn('Device Chemistry   '+LBatteryInfo.GetDeviceChemistry);
        WriteLn(Format('Design Capacity    %d mWatt/hours',[LBatteryInfo.RAWBatteryInfo^.DesignCapacity*LBatteryInfo.RAWBatteryInfo^.DesignCapacityMultiplier]));
        WriteLn(Format('Design Voltage     %d mVolts',[LBatteryInfo.RAWBatteryInfo^.DesignVoltage]));
        WriteLn('SBDS Version Number  '+LBatteryInfo.GetSBDSVersionNumberStr);
        WriteLn(Format('Maximum Error in Battery Data %d%%',[LBatteryInfo.RAWBatteryInfo^.MaximumErrorInBatteryData]));
        WriteLn(Format('SBDS Version Number           %.4x',[LBatteryInfo.RAWBatteryInfo^.SBDSSerialNumber]));
        WriteLn('SBDS Manufacture Date  '+FormatDateTime('dd-mm-yyy', LBatteryInfo.GetSBDSManufacturerDate));
        WriteLn('SBDS Device Chemistry  '+LBatteryInfo.GetSBDSDeviceChemistryStr);
        WriteLn(Format('OEM Specific                  %.8x',[LBatteryInfo.RAWBatteryInfo^.OEM_Specific]));
        WriteLn;
      end
      else
      Writeln('No Battery Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetPointingDeviceInfo;
Var
  SMBios : TSMBios;
  LPointDevice  : TBuiltInPointingDeviceInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Built-in Pointing Device Information');
      WriteLn('------------------------------------');
      if SMBios.HasBuiltInPointingDeviceInfo then
      for LPointDevice in SMBios.BuiltInPointingDeviceInformation do
      begin
        WriteLn(Format('Type              %s',[LPointDevice.GetType]));
        WriteLn(Format('Interface         %s',[LPointDevice.GetInterface]));
        WriteLn(Format('Number of Buttons %d',[LPointDevice.RAWBuiltInPointingDeviceInfo^.NumberofButtons]));
        WriteLn;
      end
      else
      Writeln('No Built-in Pointing Device Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetMemDeviceMappedInfo;
Var
  SMBios : TSMBios;
  LMemDevMappedAddress  : TMemoryDeviceMappedAddressInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Memory Device Mapped Address Information');
      WriteLn('----------------------------------------');
      if SMBios.HasMemoryDeviceMappedAddressInfo then
      for LMemDevMappedAddress in SMBios.MemoryDeviceMappedAddressInformation do
      begin
        WriteLn(Format('Starting Address      %.8x',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.StartingAddress]));
        WriteLn(Format('Ending   Address      %.8x',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.EndingAddress]));
        WriteLn(Format('Memory Device Handle  %.4x',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.MemoryDeviceHandle]));
        WriteLn(Format('Memory Array Mapped Address Handle %.4x',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.MemoryArrayMappedAddressHandle]));
        WriteLn(Format('Partition Row Position  %d',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.PartitionRowPosition]));
        WriteLn(Format('Interleave Position     %d',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.InterleavePosition]));
        WriteLn(Format('Interleaved Data Depth  %d',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.InterleavedDataDepth]));

        if SMBios.SmbiosVersion>='2.7' then
        begin
          WriteLn(Format('Extended Starting Address  %x',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.ExtendedStartingAddress]));
          WriteLn(Format('Extended Ending   Address  %x',[LMemDevMappedAddress.RAWMemoryDeviceMappedAddressInfo^.ExtendedEndingAddress]));
        end;

        WriteLn;
      end
      else


      Writeln('No Memory Device Mapped Address Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetMemArrayMappedInfo;
Var
  SMBios : TSMBios;
  LMemArrMappedAddress  : TMemoryArrayMappedAddressInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Memory Array Mapped Address Information');
      WriteLn('---------------------------------------');
      if SMBios.HasMemoryArrayMappedAddressInfo then
      for LMemArrMappedAddress in SMBios.MemoryArrayMappedAddressInformation do
      begin
        WriteLn(Format('Starting Address    %.8x ',[LMemArrMappedAddress.RAWMemoryArrayMappedAddressInfo^.StartingAddress]));
        WriteLn(Format('Ending   Address    %.8x ',[LMemArrMappedAddress.RAWMemoryArrayMappedAddressInfo^.EndingAddress]));
        WriteLn(Format('Memory Array Handle %.4x ',[LMemArrMappedAddress.RAWMemoryArrayMappedAddressInfo^.MemoryArrayHandle]));
        WriteLn(Format('Partition Width     %d ',[LMemArrMappedAddress.RAWMemoryArrayMappedAddressInfo^.PartitionWidth]));
        if SMBios.SmbiosVersion>='2.7' then
        begin
          WriteLn(Format('Extended Starting Address  %x',[LMemArrMappedAddress.RAWMemoryArrayMappedAddressInfo^.ExtendedStartingAddress]));
          WriteLn(Format('Extended Ending   Address  %x',[LMemArrMappedAddress.RAWMemoryArrayMappedAddressInfo^.ExtendedEndingAddress]));
        end;

        WriteLn;
      end
      else


      Writeln('No Memory Array Mapped Address Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetMemoryDeviceInfo;
Var
  SMBios : TSMBios;
  LMemoryDevice  : TMemoryDeviceInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Memory Device Information');
      WriteLn('-------------------------');

      if SMBios.HasMemoryDeviceInfo then
      for LMemoryDevice in SMBios.MemoryDeviceInfo do
      begin
        WriteLn(Format('Total Width    %d bits',[LMemoryDevice.RAWMemoryDeviceInfo^.TotalWidth]));
        WriteLn(Format('Data Width     %d bits',[LMemoryDevice.RAWMemoryDeviceInfo^.DataWidth]));
        WriteLn(Format('Size           %d Mbytes',[LMemoryDevice.GetSize]));
        WriteLn(Format('Form Factor    %s',[LMemoryDevice.GetFormFactor]));
        WriteLn(Format('Device Locator %s',[LMemoryDevice.GetDeviceLocatorStr]));
        WriteLn(Format('Bank Locator   %s',[LMemoryDevice.GetBankLocatorStr]));
        WriteLn(Format('Memory Type    %s',[LMemoryDevice.GetMemoryTypeStr]));
        WriteLn(Format('Speed          %d MHz',[LMemoryDevice.RAWMemoryDeviceInfo^.Speed]));
        WriteLn(Format('Manufacturer   %s',[LMemoryDevice.ManufacturerStr]));
        WriteLn(Format('Serial Number  %s',[LMemoryDevice.SerialNumberStr]));
        WriteLn(Format('Asset Tag      %s',[LMemoryDevice.AssetTagStr]));
        WriteLn(Format('Part Number    %s',[LMemoryDevice.PartNumberStr]));

        WriteLn;

        if LMemoryDevice.RAWMemoryDeviceInfo^.PhysicalMemoryArrayHandle>0 then
        begin
          WriteLn('  Physical Memory Array');
          WriteLn('  ---------------------');
          WriteLn('  Location         '+LMemoryDevice.PhysicalMemoryArray.GetLocationStr);
          WriteLn('  Use              '+LMemoryDevice.PhysicalMemoryArray.GetUseStr);
          WriteLn('  Error Correction '+LMemoryDevice.PhysicalMemoryArray.GetErrorCorrectionStr);
          if LMemoryDevice.PhysicalMemoryArray.RAWPhysicalMemoryArrayInformation^.MaximumCapacity<>$80000000 then
            WriteLn(Format('  Maximum Capacity %d Kb',[LMemoryDevice.PhysicalMemoryArray.RAWPhysicalMemoryArrayInformation^.MaximumCapacity]))
          else
            WriteLn(Format('  Maximum Capacity %d bytes',[LMemoryDevice.PhysicalMemoryArray.RAWPhysicalMemoryArrayInformation^.ExtendedMaximumCapacity]));

          WriteLn(Format('  Memory devices   %d',[LMemoryDevice.PhysicalMemoryArray.RAWPhysicalMemoryArrayInformation^.NumberofMemoryDevices]));
        end;
        WriteLn;
      end
      else
      Writeln('No Memory Device Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetPhysicalMemArrayInfo;
Var
  SMBios : TSMBios;
  LPhysicalMemArr  : TPhysicalMemoryArrayInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Physical Memory Array Information');
      WriteLn('--------------------------------');
      if SMBios.HasPhysicalMemoryArrayInfo then
      for LPhysicalMemArr in SMBios.PhysicalMemoryArrayInfo do
      begin
        WriteLn('Location         '+LPhysicalMemArr.GetLocationStr);
        WriteLn('Use              '+LPhysicalMemArr.GetUseStr);
        WriteLn('Error Correction '+LPhysicalMemArr.GetErrorCorrectionStr);
        if LPhysicalMemArr.RAWPhysicalMemoryArrayInformation^.MaximumCapacity<>$80000000 then
          WriteLn(Format('Maximum Capacity %d Kb',[LPhysicalMemArr.RAWPhysicalMemoryArrayInformation^.MaximumCapacity]))
        else
          WriteLn(Format('Maximum Capacity %d bytes',[LPhysicalMemArr.RAWPhysicalMemoryArrayInformation^.ExtendedMaximumCapacity]));

        WriteLn(Format('Memory devices   %d',[LPhysicalMemArr.RAWPhysicalMemoryArrayInformation^.NumberofMemoryDevices]));
        WriteLn;
      end
      else
      Writeln('No Physical Memory Array Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetBIOSLanguageInfo;
Var
  SMBios : TSMBios;
  LBIOSLng  : TBIOSLanguageInformation;
  i: integer;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('BIOS Language Information');
      if SMBios.HasBIOSLanguageInfo then
      for LBIOSLng in SMBios.BIOSLanguageInfo do
      begin
        WriteLn('Installable Languages  '+IntToStr( LBIOSLng.RAWBIOSLanguageInformation^.InstallableLanguages));
        WriteLn('Flags                  '+ByteToBinStr(LBIOSLng.RAWBIOSLanguageInformation^.Flags));
        WriteLn('Current Language       '+LBIOSLng.GetCurrentLanguageStr);

        if LBIOSLng.RAWBIOSLanguageInformation^.InstallableLanguages>1 then
        begin
          WriteLn('BIOS Languages');
          WriteLn('--------------');
          for i:=1 to LBIOSLng.RAWBIOSLanguageInformation^.InstallableLanguages do
            WriteLn('  '+LBIOSLng.GetLanguageString(i));
        end;

        WriteLn;
      end
      else
      Writeln('No BIOS Language Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetSystemSlotInfo;
Var
  SMBios : TSMBios;
  LSlot  : TSystemSlotInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('System Slot Information');
      WriteLn('--------------------------');
      if SMBios.HasSystemSlotInfo then
      for LSlot in SMBios.SystemSlotInfo do
      begin
        WriteLn('Slot Designation    '+LSlot.SlotDesignationStr);
        WriteLn('Slot Type           '+LSlot.GetSlotType);
        WriteLn('Slot Data Bus Width '+LSlot.GetSlotDataBusWidth);
        WriteLn('Current Usage       '+LSlot.GetCurrentUsage);
        WriteLn('Slot Length         '+LSlot.GetSlotLength);
        WriteLn(Format('Slot ID             %.4x',[LSlot.RAWSystemSlotInformation^.SlotID]));
        WriteLn('Characteristics 1   '+ByteToBinStr(LSlot.RAWSystemSlotInformation^.SlotCharacteristics1));
        WriteLn('Characteristics 2   '+ByteToBinStr(LSlot.RAWSystemSlotInformation^.SlotCharacteristics2));
        if SMBios.SmbiosVersion>='2.6' then
        begin
          WriteLn(Format('Segment Group Number %.4x',[LSlot.RAWSystemSlotInformation^.SegmentGroupNumber]));
          WriteLn(Format('Bus Number           %d',[LSlot.RAWSystemSlotInformation^.BusNumber]));
        end;
        WriteLn;
      end
      else
      Writeln('No System Slot  Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetPortConnectorInfo;
Var
  SMBios : TSMBios;
  LPort  : TPortConnectorInformation;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Port Connector Information');
      WriteLn('--------------------------');
      if SMBios.HasPortConnectorInfo then
      for LPort in SMBios.PortConnectorInfo do
      begin
        WriteLn('Internal Reference Designator '+LPort.InternalReferenceDesignatorStr);
        WriteLn('Internal Connector Type       '+LPort.GetConnectorType(LPort.RAWPortConnectorInformation^.InternalConnectorType));
        WriteLn('External Reference Designator '+LPort.ExternalReferenceDesignatorStr);
        WriteLn('External Connector Type       '+LPort.GetConnectorType(LPort.RAWPortConnectorInformation^.ExternalConnectorType));
        WriteLn('Port Type                     '+LPort.PortTypeStr);
        WriteLn;
      end
      else
      Writeln('No Port Connector Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetProcessorInfo;
Var
  SMBios             : TSMBios;
  LProcessorInfo     : TProcessorInformation;
  LSRAMTypes         : TCacheSRAMTypes;
begin
  SMBios:=TSMBios.Create;
  try
      WriteLn('Processor Information');
      if SMBios.HasProcessorInfo then
      for LProcessorInfo in SMBios.ProcessorInfo do
      begin
        WriteLn('Manufacter         '+LProcessorInfo.ProcessorManufacturerStr);
        WriteLn('Socket Designation '+LProcessorInfo.SocketDesignationStr);
        WriteLn('Type               '+LProcessorInfo.ProcessorTypeStr);
        WriteLn('Familiy            '+LProcessorInfo.ProcessorFamilyStr);
        WriteLn('Version            '+LProcessorInfo.ProcessorVersionStr);
        WriteLn(Format('Processor ID       %x',[LProcessorInfo.RAWProcessorInformation^.ProcessorID]));
        WriteLn(Format('Voltaje            %n',[LProcessorInfo.GetProcessorVoltaje]));
        WriteLn(Format('External Clock     %d  Mhz',[LProcessorInfo.RAWProcessorInformation^.ExternalClock]));
        WriteLn(Format('Maximum processor speed %d  Mhz',[LProcessorInfo.RAWProcessorInformation^.MaxSpeed]));
        WriteLn(Format('Current processor speed %d  Mhz',[LProcessorInfo.RAWProcessorInformation^.CurrentSpeed]));
        WriteLn('Processor Upgrade   '+LProcessorInfo.ProcessorUpgradeStr);
        WriteLn(Format('External Clock     %d  Mhz',[LProcessorInfo.RAWProcessorInformation^.ExternalClock]));

        if SMBios.SmbiosVersion>='2.3' then
        begin
          WriteLn('Serial Number      '+LProcessorInfo.SerialNumberStr);
          WriteLn('Asset Tag          '+LProcessorInfo.AssetTagStr);
          WriteLn('Part Number        '+LProcessorInfo.PartNumberStr);
          if SMBios.SmbiosVersion>='2.5' then
          begin
            WriteLn(Format('Core Count         %d',[LProcessorInfo.RAWProcessorInformation^.CoreCount]));
            WriteLn(Format('Cores Enabled      %d',[LProcessorInfo.RAWProcessorInformation^.CoreEnabled]));
            WriteLn(Format('Threads Count      %d',[LProcessorInfo.RAWProcessorInformation^.ThreadCount]));
            WriteLn(Format('Processor Characteristics %.4x',[LProcessorInfo.RAWProcessorInformation^.ProcessorCharacteristics]));
          end;
        end;
        Writeln;

        if (LProcessorInfo.RAWProcessorInformation^.L1CacheHandle>0) and (LProcessorInfo.L2Chache<>nil)  then
        begin
          WriteLn('L1 Cache Handle Info');
          WriteLn('--------------------');
          WriteLn('  Socket Designation    '+LProcessorInfo.L1Chache.SocketDesignationStr);
          WriteLn(Format('  Cache Configuration   %.4x',[LProcessorInfo.L1Chache.RAWCacheInformation^.CacheConfiguration]));
          WriteLn(Format('  Maximum Cache Size    %d Kb',[LProcessorInfo.L1Chache.GetMaximumCacheSize]));
          WriteLn(Format('  Installed Cache Size  %d Kb',[LProcessorInfo.L1Chache.GetInstalledCacheSize]));
          LSRAMTypes:=LProcessorInfo.L1Chache.GetSupportedSRAMType;
          WriteLn(Format('  Supported SRAM Type   %s',[SetToString(TypeInfo(TCacheSRAMTypes), LSRAMTypes, True)]));
          LSRAMTypes:=LProcessorInfo.L1Chache.GetCurrentSRAMType;
          WriteLn(Format('  Current SRAM Type     %s',[SetToString(TypeInfo(TCacheSRAMTypes), LSRAMTypes, True)]));

          WriteLn(Format('  Error Correction Type %s',[ErrorCorrectionTypeStr[LProcessorInfo.L1Chache.GetErrorCorrectionType]]));
          WriteLn(Format('  System Cache Type     %s',[SystemCacheTypeStr[LProcessorInfo.L1Chache.GetSystemCacheType]]));
          WriteLn(Format('  Associativity         %s',[LProcessorInfo.L1Chache.AssociativityStr]));
        end;

        if (LProcessorInfo.RAWProcessorInformation^.L2CacheHandle>0)  and (LProcessorInfo.L2Chache<>nil)  then
        begin
          WriteLn('L2 Cache Handle Info');
          WriteLn('--------------------');
          WriteLn('  Socket Designation    '+LProcessorInfo.L2Chache.SocketDesignationStr);
          WriteLn(Format('  Cache Configuration   %.4x',[LProcessorInfo.L2Chache.RAWCacheInformation^.CacheConfiguration]));
          WriteLn(Format('  Maximum Cache Size    %d Kb',[LProcessorInfo.L2Chache.GetMaximumCacheSize]));
          WriteLn(Format('  Installed Cache Size  %d Kb',[LProcessorInfo.L2Chache.GetInstalledCacheSize]));
          LSRAMTypes:=LProcessorInfo.L2Chache.GetSupportedSRAMType;
          WriteLn(Format('  Supported SRAM Type   %s',[SetToString(TypeInfo(TCacheSRAMTypes), LSRAMTypes, True)]));
          LSRAMTypes:=LProcessorInfo.L2Chache.GetCurrentSRAMType;
          WriteLn(Format('  Current SRAM Type     %s',[SetToString(TypeInfo(TCacheSRAMTypes), LSRAMTypes, True)]));

          WriteLn(Format('  Error Correction Type %s',[ErrorCorrectionTypeStr[LProcessorInfo.L2Chache.GetErrorCorrectionType]]));
          WriteLn(Format('  System Cache Type     %s',[SystemCacheTypeStr[LProcessorInfo.L2Chache.GetSystemCacheType]]));
          WriteLn(Format('  Associativity         %s',[LProcessorInfo.L2Chache.AssociativityStr]));
        end;

        if (LProcessorInfo.RAWProcessorInformation^.L3CacheHandle>0) and (LProcessorInfo.L3Chache<>nil) then
        begin
          WriteLn('L3 Cache Handle Info');
          WriteLn('--------------------');
          WriteLn('  Socket Designation    '+LProcessorInfo.L3Chache.SocketDesignationStr);
          WriteLn(Format('  Cache Configuration   %.4x',[LProcessorInfo.L3Chache.RAWCacheInformation^.CacheConfiguration]));
          WriteLn(Format('  Maximum Cache Size    %d Kb',[LProcessorInfo.L3Chache.GetMaximumCacheSize]));
          WriteLn(Format('  Installed Cache Size  %d Kb',[LProcessorInfo.L3Chache.GetInstalledCacheSize]));
          LSRAMTypes:=LProcessorInfo.L3Chache.GetSupportedSRAMType;
          WriteLn(Format('  Supported SRAM Type   %s',[SetToString(TypeInfo(TCacheSRAMTypes), LSRAMTypes, True)]));
          LSRAMTypes:=LProcessorInfo.L3Chache.GetCurrentSRAMType;
          WriteLn(Format('  Current SRAM Type     %s',[SetToString(TypeInfo(TCacheSRAMTypes), LSRAMTypes, True)]));

          WriteLn(Format('  Error Correction Type %s',[ErrorCorrectionTypeStr[LProcessorInfo.L3Chache.GetErrorCorrectionType]]));
          WriteLn(Format('  System Cache Type     %s',[SystemCacheTypeStr[LProcessorInfo.L3Chache.GetSystemCacheType]]));
          WriteLn(Format('  Associativity         %s',[LProcessorInfo.L3Chache.AssociativityStr]));
        end;

        //Readln;
      end
      else
      Writeln('No Processor Info was found');
  finally
   SMBios.Free;
  end;
end;



procedure GetEnclosureInfo;
Var
  SMBios : TSMBios;
  LEnclosure  : TEnclosureInformation;
begin
  SMBios:=TSMBios.Create;
  try
      //WriteLn('Enclosure Information');
      if SMBios.HasEnclosureInfo then
      for LEnclosure in SMBios.EnclosureInfo do
      begin
        outlist.Append('enclosure.Manufacter='+LEnclosure.ManufacturerStr);
        outlist.Append('enclosure.Version='+LEnclosure.VersionStr);
        outlist.Append('enclosure.Serial Number='+LEnclosure.SerialNumberStr);
        outlist.Append('enclosure.Asset Tag Number='+LEnclosure.AssetTagNumberStr);
        outlist.Append('enclosure.Type='+LEnclosure.TypeStr);
        outlist.Append('enclosure.Power Supply State='+LEnclosure.PowerSupplyStateStr);
        outlist.Append('enclosure.BootUp State='+LEnclosure.BootUpStateStr);
        //WriteLn;
      end;
      //else
      //Writeln('No Enclosure Info was found');
  finally
   SMBios.Free;
  end;
end;


procedure GetBaseBoardInfo;
Var
  SMBios : TSMBios;
  LBaseBoard : TBaseBoardInformation;
begin
  SMBios:=TSMBios.Create;
  try
      //WriteLn('Base Board Information');
      if SMBios.HasBaseBoardInfo then
      for LBaseBoard in SMBios.BaseBoardInfo do
      begin
        //WriteLn('Manufacter          '+SMBios.GetSMBiosString(BBI.LocalIndex  + BBI.Header.Length, BBI.Manufacturer));
        outlist.Append('board.Manufacter='+LBaseBoard.ManufacturerStr);
        outlist.Append('board.Product='+LBaseBoard.ProductStr);
        outlist.Append('board.Version='+LBaseBoard.VersionStr);
        outlist.Append('board.Serial Number='+LBaseBoard.SerialNumberStr);
        outlist.Append('board.Asset Tag='+LBaseBoard.AssetTagStr);
        outlist.Append('board.Feature Flags='+ByteToBinStr(LBaseBoard.RAWBaseBoardInformation^.FeatureFlags));
        outlist.Append('board.Location in Chassis='+LBaseBoard.LocationinChassisStr);
        outlist.Append(Format('board.Chassis Handle=%0.4x',[LBaseBoard.RAWBaseBoardInformation^.ChassisHandle]));
        outlist.Append(Format('board.Board Type=%0.2x %s',[LBaseBoard.RAWBaseBoardInformation^.BoardType, LBaseBoard.BoardTypeStr]));
        outlist.Append('board.Number of Contained Object Handles='+IntToStr(LBaseBoard.RAWBaseBoardInformation^.NumberofContainedObjectHandles));
        //WriteLn;
      end;
      //else
      //Writeln('No Base Board Info was found');
  finally
   SMBios.Free;
  end;
end;



procedure GetSystemInfo;
Var
  SMBios : TSMBios;
  LSystem: TSystemInformation;
  UUID   : Array[0..31] of AnsiChar;
begin
  SMBios:=TSMBios.Create;
  try
    LSystem:=SMBios.SysInfo;
    //WriteLn('System Information');
    outlist.Append('sysinfo.Manufacter='+LSystem.ManufacturerStr);
    outlist.Append('sysinfo.Product Name='+LSystem.ProductNameStr);
    outlist.Append('sysinfo.Version='+LSystem.VersionStr);
    outlist.Append('sysinfo.Serial Number='+LSystem.SerialNumberStr);
    BinToHex(@LSystem.RAWSystemInformation^.UUID,UUID,SizeOf(LSystem.RAWSystemInformation^.UUID));
    outlist.Append('sysinfo.UUID='+UUID);
    if SMBios.SmbiosVersion>='2.4' then
    begin
      outlist.Append('sysinfo.SKU Number='+LSystem.SKUNumberStr);
      outlist.Append('sysinfo.Family='+LSystem.FamilyStr);
    end;
    //WriteLn;
  finally
   SMBios.Free;
  end;
end;


procedure GetBIOSInfo;
Var
  SMBios  : TSMBios;
  LBIOS   : TBiosInformation;
  OEMStr  : TOEMStringsInformation;
  LSystemConf : TSystemConfInformation;
  i : Integer;
begin
  SMBios:=TSMBios.Create;
  try
    LBIOS:=SMBios.BiosInfo;

    outlist.Append('bios.Vendor='+LBIOS.VendorStr);
    outlist.Append('bios.Version='+LBIOS.VersionStr);
    outlist.Append('bios.Start Segment='+IntToHex(LBIOS.RAWBiosInformation^.StartingSegment,4));
    outlist.Append('bios.ReleaseDate='+LBIOS.ReleaseDateStr);
    outlist.Append(Format('bios.RomSize=%d k',[64*(LBIOS.RAWBiosInformation^.BiosRomSize+1)]));

    if LBIOS.RAWBiosInformation^.SystemBIOSMajorRelease<>$ff then
    outlist.Append(Format('bios.SystemBIOS Major Release=%d',[LBIOS.RAWBiosInformation^.SystemBIOSMajorRelease]));
    if LBIOS.RAWBiosInformation^.SystemBIOSMinorRelease<>$ff then
    outlist.Append(Format('bios.System BIOS Minor Release=%d',[LBIOS.RAWBiosInformation^.SystemBIOSMinorRelease]));

    //If the system does not have field upgradeable embedded controller firmware, the value is 0FFh.
    if LBIOS.RAWBiosInformation^.EmbeddedControllerFirmwareMajorRelease<>$ff then
    outlist.Append(Format('bios.Embedded Controller Firmware Major Release=%d',[LBIOS.RAWBiosInformation^.EmbeddedControllerFirmwareMajorRelease]));
    if LBIOS.RAWBiosInformation^.EmbeddedControllerFirmwareMinorRelease<>$ff then
    outlist.Append(Format('bios.Embedded Controller Firmware Minor Releasee=%d',[LBIOS.RAWBiosInformation^.EmbeddedControllerFirmwareMinorRelease]));

    (*
    WriteLn('Bios Information');
    WriteLn('Vendor        '+LBIOS.VendorStr);
    WriteLn('Version       '+LBIOS.VersionStr);
    WriteLn('Start Segment '+IntToHex(LBIOS.RAWBiosInformation^.StartingSegment,4));
    WriteLn('ReleaseDate   '+LBIOS.ReleaseDateStr);
    WriteLn(Format('Bios Rom Size %d k',[64*(LBIOS.RAWBiosInformation^.BiosRomSize+1)]));

    if LBIOS.RAWBiosInformation^.SystemBIOSMajorRelease<>$ff then
    WriteLn(Format('System BIOS Major Release %d',[LBIOS.RAWBiosInformation^.SystemBIOSMajorRelease]));
    if LBIOS.RAWBiosInformation^.SystemBIOSMinorRelease<>$ff then
    WriteLn(Format('System BIOS Minor Release %d',[LBIOS.RAWBiosInformation^.SystemBIOSMinorRelease]));

    //If the system does not have field upgradeable embedded controller firmware, the value is 0FFh.
    if LBIOS.RAWBiosInformation^.EmbeddedControllerFirmwareMajorRelease<>$ff then
    WriteLn(Format('Embedded Controller Firmware Major Release %d',[LBIOS.RAWBiosInformation^.EmbeddedControllerFirmwareMajorRelease]));
    if LBIOS.RAWBiosInformation^.EmbeddedControllerFirmwareMinorRelease<>$ff then
    WriteLn(Format('Embedded Controller Firmware Minor Releasee %d',[LBIOS.RAWBiosInformation^.EmbeddedControllerFirmwareMinorRelease]));
    WriteLn;
          *)

    if SMBios.HasOEMStringsInfo then
    begin
     //Writeln('OEM Strings');
     //Writeln('-----------');
     for OEMStr in SMBios.OEMStringsInfo do
      for i:=1 to OEMStr.RAWOEMStringsInformation^.Count do
       outlist.Append('bios.OEM String['+inttostr(i)+']='+OEMStr.GetOEMString(i));
    end;

    if SMBios.HasSystemConfInfo then
    begin
     //Writeln('System Config Strings');
     //Writeln('---------------------');
     for LSystemConf in SMBios.SystemConfInfo do
      for i:=1 to LSystemConf.RAWSystemConfInformation^.Count do
       outlist.Append('bios.System Config String['+inttostr(i)+']='+LSystemConf.GetConfString(i));
    end;


  finally
   SMBios.Free;
  end;
end;


procedure writeall;
begin
 try
   GetBIOSInfo;
   GetSystemInfo;
   GetBaseBoardInfo;
   GetEnclosureInfo;
   GetProcessorInfo;
   GetPortConnectorInfo;
   GetSystemSlotInfo;
   GetBIOSLanguageInfo;
   GetPhysicalMemArrayInfo;
   GetMemoryDeviceInfo;
   GetMemArrayMappedInfo;
   GetMemDeviceMappedInfo;
   GetPointingDeviceInfo;
   GetBatteryInfo;
   GetVoltageProbeInfo;
   GetCoolingDeviceInfo;
   GetTempProbeInfo;
   GetElectricalCurrProbeInfo;
 except
    on E:Exception do
        Writeln(E.Classname, ':', E.Message);
 end;
 Writeln;
 Writeln('Press Enter to exit');
 Readln;
end;

function getHwBiosShortlist : TStringlist;
var
  i : integer;

begin
 try
   outlist := TStringList.Create;
   Result := TStringList.Create;
   GetBIOSInfo;
   GetSystemInfo;
   GetBaseBoardInfo;
   GetEnclosureInfo;
   result.AddStrings(outlist);
   outlist.free;
 except
    on E:Exception do
        //Writeln(E.Classname, ':', E.Message);
 end;
end;

end.




