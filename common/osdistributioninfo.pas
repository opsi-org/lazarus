unit osDistributionInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osLinuxRepository;

type

  {DistributionInfo}

  TDistributionInfo = class(TObject)
  private
    FMyDistr: TDistribution;
    FDistrUrlPart: string;
  public
  const
    { old
    Distribs = 'CentOS 7,' + #10 + 'Debian 8, Debian 9, Debian 10,' +
      #10 + 'openSUSE 15.1, openSUSE 42.3,' + #10 +
      'RedHatEnterprise 7,' + 'RedHatEnterprise 8,' + #10 + 'Univention 4.3, Univention 4.4,' +
      #10 + 'Ubuntu 16.04, Ubuntu 18.04, Ubuntu 20.04';}
    Distribs = 'Debian 9, Debian 10,' + #10 +
      'openSUSE 15.1, openSUSE 15.2,' + #10 +
      'SLES 15 SP1, SLES 15 SP1,' + #10 + 'Ubuntu 18.04, Ubuntu 20.04,' +
      #10 + 'Univention 4.4';
    property MyDistr: TDistribution read FMyDistr;
    property DistrUrlPart: string read FDistrUrlPart;

    procedure SetInfo(distroName: string; distroRelease: string);
  end;

function GetPackageManagementShellCommand(distroName: string): string;

implementation

procedure TDistributionInfo.SetInfo(distroName: string; distroRelease: string);
begin
  // Change from distroName and -Release to TDistribution and respective URL part
  // Pos('7', distroRelease) = 1 checks whether string '7' occurs in string...
  // ...distroRelease at first position (1-based) for the fist time...
  // ...(meaning 0th position in array thinking).

  // Set default MyDistr so that for an unknown distribution the result is...
  // ...not the enum default CentOS_7
  FMyDistr := other;
  FDistrUrlPart := '';
  // CentOS has releases with names like 7.x-xxxx
  {if distroName = 'CentOS' then
  begin
    if Pos('7', distroRelease) = 1 then
    begin
      FMyDistr := CentOS_7;
      FDistrUrlPart := 'CentOS_7/';
    end
    else
    if Pos('8', distroRelease) = 1 then
    begin
      FMyDistr := CentOS_8;
      FDistrUrlPart := 'CentOS_8/';
    end;
  end
  else}
  if distroName = 'Debian' then
  begin
    if Pos('9', distroRelease) = 1 then
    begin
      FMyDistr := Debian_9;
      FDistrUrlPart := 'Debian_9/';
    end
    else
    if Pos('10', distroRelease) = 1 then
    begin
      FMyDistr := Debian_10;
      FDistrUrlPart := 'Debian_10/';
    end;
  end
  else
  if distroName = 'openSUSE' then
  begin
    if distroRelease = '15.1' then
    begin
      FMyDistr := openSUSE_Leap_15_1;
      FDistrUrlPart := 'openSUSE_Leap_15.1/';
    end
    else if distroRelease = '15.2' then
    begin
      FMyDistr := openSUSE_Leap_15_2;
      FDistrUrlPart := 'openSUSE_Leap_15.2/';
    {end
    else if distroRelease = '42.3' then
    begin
      FMyDistr := openSUSE_Leap_42_3;
      FDistrUrlPart := 'openSUSE_Leap_42.3/';}
    end;
  end
  else
  // RHEL has releases like 7.x
  {if distroName = 'RedHatEnterprise' then
  begin
    if Pos('7', distroRelease) = 1 then
    begin
      FMyDistr := RHEL_7;
      FDistrUrlPart := 'RHEL_7/';
    end
    else
    if Pos('8', distroRelease) = 1 then
    begin
      FMyDistr := RHEL_8;
      FDistrUrlPart := 'RHEL_8/';
    end;
  end
  else}
  if distroName = 'Univention' then
  begin
    {if Pos('4.3', distroRelease) = 1 then
    begin
      FMyDistr := Univention_4_3;
      FDistrUrlPart := 'Univention_4_3/';
    end
    else}
    if Pos('4.4', distroRelease) = 1 then
    begin
      FMyDistr := Univention_4_4;
      FDistrUrlPart := 'Univention_4.4/';
    end;
  end
  else
  if distroName = 'SUSE' then
  begin
    if distroRelease = '15.1' then
    begin
      FMyDistr := SLE15_SP1;
      FDistrUrlPart := 'SLE_15_SP1/';
    end
    else
    if distroRelease = '15.2' then
    begin
      FMyDistr := SLE15_SP2;
      FDistrUrlPart := 'SLE_15_SP1/';
    end;
  end
  else
  if distroName = 'Ubuntu' then
  begin
    {if distroRelease = '16.04' then
    begin
      FMyDistr := xUbuntu_16_04;
      FDistrUrlPart := 'xUbuntu_16.04/';
    end
    else}
    if distroRelease = '18.04' then
    begin
      FMyDistr := xUbuntu_18_04;
      FDistrUrlPart := 'xUbuntu_18.04/';
    end
    else
    if distroRelease = '20.04' then
    begin
      FMyDistr := xUbuntu_20_04;
      FDistrUrlPart := 'xUbuntu_20.04/';
    end;
  end;
end;

// get right shell command for package management depending on the distribution
function GetPackageManagementShellCommand(distroName: string): string;
begin
  {CentOS and RedHat}
  if (distroName = 'CentOS') or (distroName = 'RedHatEnterprise') then
    Result := 'yum '
  {Debian, Ubuntu, Univention}
  // univention is based on debian
  else if (distroName = 'Debian') or (distroName = 'Ubuntu') or
    (distroName = 'Univention') then
    Result := 'apt '
  {OpenSuse and SLES}
  else if (distroName = 'openSUSE') or (distroName = 'SUSE') then
    Result := 'zypper --non-interactive ';
end;

end.


