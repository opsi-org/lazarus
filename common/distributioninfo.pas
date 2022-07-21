unit DistributionInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osLinuxRepository;

type

  {DistributionInfo}
  TDistributionInfo = class(TObject)
  private
  var
    FDistroName: string;
    FDistroRelease: string;
    FDistr: TDistribution;
    FDistrRepoUrlPart: string;
    FPackageManagementShellCommand: string;
  public
  const
    Distribs = 'AlmaLinux 8,' + #10 + 'Debian 9, Debian 10, Debian 11,' +
      #10 + 'openSUSE 15.3, openSUSE 15.4,' + #10 +
      'RHEL 8, RockyLinux 8,' + #10 + 'SLES 15 SP1, SLES 15 SP2,' + #10 +
      'Ubuntu 18.04, Ubuntu 20.04, Ubuntu 22.04,' + #10 + 'UCS 4.4, UCS 5.0';

    constructor Create(DistroName: string; DistroRelease: string);overload;
    procedure SetNameAndRelease(DistroName: string; DistroRelease: string);
    procedure CorrectDistributionNameAndRelease(DistroName: string; DistroRelease: string);
    procedure SetDistrAndUrlPart;
    procedure SetPackageManagementShellCommand;

    property DistroName: string read FDistroName;
    property DistroRelease: string read FDistroRelease;
    property Distr: TDistribution read FDistr;
    property DistrRepoUrlPart: string read FDistrRepoUrlPart;
    property PackageManagementShellCommand: string read FPackageManagementShellCommand;
  end;



implementation

constructor TDistributionInfo.Create(DistroName: string; DistroRelease: string);overload;
begin
  inherited Create;
  FDistroName := DistroName;
  FDistroRelease := DistroRelease;
end;

procedure TDistributionInfo.SetNameAndRelease(DistroName: string; DistroRelease: string);
begin
  FDistroName := DistroName;
  FDistroRelease := DistroRelease;
end;

procedure TDistributionInfo.CorrectDistributionNameAndRelease(DistroName: string; DistroRelease: string);
begin
  FDistroName := DistroName;
  FDistroRelease := DistroRelease;
end;

procedure TDistributionInfo.SetDistrAndUrlPart;
begin
  FDistr := other;
  FDistrRepoUrlPart := '';

  // AlmaLinux has releases with names like 8.x
  if FDistroName = 'AlmaLinux' then
  begin
    if Pos('8', FDistroRelease) = 1 then
    begin
      FDistr := AlmaLinux_8;
      FDistrRepoUrlPart := 'AlmaLinux_8/';
    end
    else
    if Pos('9', FDistroRelease) = 1 then
    begin
      FDistr := AlmaLinux_9;
      FDistrRepoUrlPart := 'AlmaLinux_9/';
    end;
  end
  else
  if FDistroName = 'Debian' then
  begin
    if Pos('9', FDistroRelease) = 1 then
    begin
      FDistr := Debian_9;
      FDistrRepoUrlPart := 'Debian_9/';
    end
    else
    if Pos('10', FDistroRelease) = 1 then
    begin
      FDistr := Debian_10;
      FDistrRepoUrlPart := 'Debian_10/';
    end
    else
    if Pos('11', FDistroRelease) = 1 then
    begin
      FDistr := Debian_11;
      FDistrRepoUrlPart := 'Debian_11/';
    end;
  end
  else
  if FDistroName = 'openSUSE' then
  begin
    if FDistroRelease = '15.3' then
    begin
      FDistr := openSUSE_Leap_15_3;
      FDistrRepoUrlPart := 'openSUSE_Leap_15.3/';
    end
    else if FDistroRelease = '15.4' then
    begin
      FDistr := openSUSE_Leap_15_4;
      FDistrRepoUrlPart := 'openSUSE_Leap_15.4/';
    end;
  end
  else
  // RHEL has releases like 7.x
  if FDistroName = 'RedHatEnterprise' then
  begin
    if Pos('8', FDistroRelease) = 1 then
    begin
      FDistr := RHEL_8;
      FDistrRepoUrlPart := 'RHEL_8/';
    end
    else
    if Pos('9', FDistroRelease) = 1 then
    begin
      FDistr := RHEL_9;
      FDistrRepoUrlPart := 'RHEL_9/';
    end;
  end
  else
  if FDistroName = 'Rocky' then
  begin
    if Pos('8', FDistroRelease) = 1 then
    begin
      FDistr := RockyLinux_8;
      FDistrRepoUrlPart := 'RockyLinux_8/';
    end
    else
    if Pos('9', FDistroRelease) = 1 then
    begin
      FDistr := RockyLinux_9;
      FDistrRepoUrlPart := 'RockyLinux_9/';
    end;
  end
  else
  if FDistroName = 'SUSE' then
  begin
    if FDistroRelease = '15.1' then
    begin
      FDistr := SLE15_SP1;
      FDistrRepoUrlPart := 'SLE_15_SP1/';
    end
    else
    if FDistroRelease = '15.2' then
    begin
      FDistr := SLE15_SP2;
      FDistrRepoUrlPart := 'SLE_15_SP1/';
    end
    else
    if FDistroRelease = '15.3' then
    begin
      FDistr := SLE15_SP3;
      FDistrRepoUrlPart := 'SLE_15_SP3/';
    end
    else
    if FDistroRelease = '15.4' then
    begin
      FDistr := SLE15_SP4;
      FDistrRepoUrlPart := 'SLE_15_SP4/';
    end;
  end
  else
  if FDistroName = 'Ubuntu' then
  begin
    if FDistroRelease = '18.04' then
    begin
      FDistr := xUbuntu_18_04;
      FDistrRepoUrlPart := 'xUbuntu_18.04/';
    end
    else
    if FDistroRelease = '20.04' then
    begin
      FDistr := xUbuntu_20_04;
      FDistrRepoUrlPart := 'xUbuntu_20.04/';
    end
    else
    if FDistroRelease = '22.04' then
    begin
      FDistr := xUbuntu_22_04;
      FDistrRepoUrlPart := 'xUbuntu_22.04/';
    end;
  end
  else
  if FDistroName = 'Univention' then
  begin
    if Pos('4.4', FDistroRelease) = 1 then
    begin
      FDistr := Univention_4_4;
      FDistrRepoUrlPart := 'Univention_4.4/';
    end
    else
    if Pos('10', FDistroRelease) = 1 then //Univention 5.0 Distro Release is 10
    begin
      FDistr := Univention_5_0;
      FDistrRepoUrlPart := 'Univention_5.0/';
    end;
  end;
end;

procedure TDistributionInfo.SetPackageManagementShellCommand;
begin
  {RedHat like}
  if (FDistroName = 'RedHatEnterprise') or
    (FDistroName = 'AlmaLinux') or (FDistroName = 'Rocky') then
    FPackageManagementShellCommand := 'yum -y '
  {Debian like}
  else if (FDistroName = 'Debian') or (FDistroName = 'Ubuntu') or
    (FDistroName = 'Univention') then
    FPackageManagementShellCommand := 'apt --assume-yes '
  {Suse}
  else if (FDistroName = 'openSUSE') or (FDistroName = 'SUSE') then
    FPackageManagementShellCommand := 'zypper --non-interactive ';
end;

end.
