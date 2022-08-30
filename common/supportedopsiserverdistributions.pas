unit SupportedOpsiServerDistributions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

type
  TSupportedDistribution = (
    AlmaLinux_8,
    AlmaLinux_9,
    Debian_9,
    Debian_10,
    Debian_11,
    openSUSE_Leap_15_3,
    openSUSE_Leap_15_4,
    RHEL_8,
    RHEL_9,
    RockyLinux_8,
    RockyLinux_9,
    SLE15_SP1,
    SLE15_SP2,
    SLE15_SP3,
    SLE15_SP4,
    Univention_4_4,
    Univention_5_0,
    xUbuntu_18_04,
    xUbuntu_20_04,
    xUbuntu_22_04,
    other
    );


function GetSupportedDistribution(DistroName: string; DistroRelease: string): TSupportedDistribution;

const
  SupportedDistributionsInfoString = 'AlmaLinux 8,' + #10 + 'Debian 9, Debian 10, Debian 11,' +
    #10 + 'openSUSE 15.3, openSUSE 15.4,' + #10 +
    'RedHatEnterprise 8, Rocky 8,' + #10 + 'SUSE 15.1, SUSE 15.2,' + #10 +
    'Ubuntu 18.04, Ubuntu 20.04, Ubuntu 22.04,' + #10 + 'Univention 4.4, Univention 5.0';

implementation

function GetSupportedDistribution(DistroName: string; DistroRelease: string): TSupportedDistribution;
begin
  Result := other;

  // AlmaLinux has releases with names like 8.x
  if lowerCase(DistroName) = 'almalinux' then
  begin
    if Pos('8', DistroRelease) = 1 then
      Result := AlmaLinux_8
    else
    if Pos('9', DistroRelease) = 1 then
      Result := AlmaLinux_9;
  end
  else
  if lowerCase(DistroName) = 'debian' then
  begin
    if Pos('9', DistroRelease) = 1 then
      Result := Debian_9
    else
    if Pos('10', DistroRelease) = 1 then
      Result := Debian_10
    else
    if Pos('11', DistroRelease) = 1 then
      Result := Debian_11;
  end
  else
  if lowerCase(DistroName) = 'opensuse' then
  begin
    if DistroRelease = '15.3' then
      Result := openSUSE_Leap_15_3
    else if DistroRelease = '15.4' then
      Result := openSUSE_Leap_15_4;
  end
  else
  // RHEL has releases like 7.x
  if MatchStr(lowerCase(DistroName), ['redhatenterprise', 'rhel']) then
  begin
    if Pos('8', DistroRelease) = 1 then
      Result := RHEL_8
    else
    if Pos('9', DistroRelease) = 1 then
      Result := RHEL_9;
  end
  else
  if lowerCase(DistroName) = 'rocky' then
  begin
    if Pos('8', DistroRelease) = 1 then
      Result := RockyLinux_8
    else
    if Pos('9', DistroRelease) = 1 then
      Result := RockyLinux_9;
  end
  else
  if lowerCase(DistroName) = 'suse' then
  begin
    if DistroRelease = '15.1' then
      Result := SLE15_SP1
    else
    if DistroRelease = '15.2' then
      Result := SLE15_SP2
    else
    if DistroRelease = '15.3' then
      Result := SLE15_SP3
    else
    if DistroRelease = '15.4' then
      Result := SLE15_SP4;
  end
  else
  if lowerCase(DistroName) = 'ubuntu' then
  begin
    if DistroRelease = '18.04' then
      Result := xUbuntu_18_04
    else
    if DistroRelease = '20.04' then
      Result := xUbuntu_20_04
    else
    if DistroRelease = '22.04' then
      Result := xUbuntu_22_04;
  end
  else
  if lowerCase(DistroName) = 'univention' then
  begin
    if Pos('4.4', DistroRelease) = 1 then
      Result := Univention_4_4
    else
    if Pos('10', DistroRelease) = 1 then //Univention 5.0 Distro Release is 10
      Result := Univention_5_0;
  end;
end;


end.
