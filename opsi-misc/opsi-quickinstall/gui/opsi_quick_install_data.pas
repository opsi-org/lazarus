unit opsi_quick_install_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osDistributionInfo;

type

  {TSplitData}

  // For data that appears in the overview and the file properties.conf in different ways,
  // e.g. OverviewEntry = 'Yes' or 'Ja' or 'Oui' or ... and PropertyEntry = 'true'
  TSplitData = class(TObject)
  public
    OverviewEntry: string;
    PropertyEntry: string; // mostly gets boolean value
    procedure SetEntries(SetOverviewEntry: string; SetPropertyEntry: string);
  end;

type

  {TQuickInstallData}

  TQuickInstallData = class(TObject)
  public
    custom: boolean;
    distroName, distroRelease: string;
    DistrInfo: TDistributionInfo;

    opsiVersion, repo: string;
    proxy: TSplitData;
    repoNoCache: string;

    backend: string;
    copyMod: TSplitData;
    repoKind: string;

    ucsPassword: string;
    reboot, dhcp: TSplitData;
    symlink: string;

    netmask, networkAddress, domain, nameserver, gateway: string;
    adminName, adminPassword, ipName, ipNumber: string;

    constructor Create;
  end;

var
  Data: TQuickInstallData;

implementation

{TSplitData}

procedure TSplitData.SetEntries(SetOverviewEntry: string; SetPropertyEntry: string);
begin
  OverviewEntry := SetOverviewEntry;
  PropertyEntry := SetPropertyEntry;
end;

{TQuickInstallData}

constructor TQuickInstallData.Create;
begin
  // default values
  DistrInfo := TDistributionInfo.Create;
  opsiVersion := 'Opsi 4.2';
  repo := 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
  proxy := TSplitData.Create;
  repoNoCache := 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
  backend := 'file';
  copyMod := TSplitData.Create;
  repoKind := 'experimental';
  ucsPassword := '';
  reboot := TSplitData.Create;
  dhcp := TSplitData.Create;
  symlink := 'default.nomenu';
  netmask := '255.255.0.0';
  networkAddress := '192.168.0.0';
  domain := 'uib.local';
  nameserver := '192.168.1.245';
  gateway := '192.168.1.245';
  adminName := 'admin';
  adminPassword := 'linux123';
  ipName := 'auto';
  ipNumber := 'auto';
end;

end.

