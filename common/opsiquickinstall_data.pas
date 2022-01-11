unit opsiquickinstall_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DistributionInfo;

type

  {TSplitData}

  // For data that appears in the overview and the file properties.conf in different ways,
  // e.g. OverviewEntry = 'Yes' or 'Ja' or 'Oui' or ... and PropertyEntry = 'true'
  TSplitData = class(TObject)
  private
    FOverviewEntry: string;
    FPropertyEntry: string; // mostly gets boolean value
  public
    procedure SetEntries(SetOverviewEntry: string; SetPropertyEntry: string);
    property OverviewEntry: string read FOverviewEntry;
    property PropertyEntry: string read FPropertyEntry;
  end;

type

  {TQuickInstallData}

  TQuickInstallData = class(TObject)
  public
  var
    CustomSetup: boolean;
    DistrInfo: TDistributionInfo;

    opsiVersion, repo, proxy, repoNoCache: string;

    backend: string;
    copyMod: TSplitData;
    repoKind: string;

    ucsPassword: string;
    reboot: TSplitData;
    dhcp: TSplitData;
    symlink: string;

    netmask, networkAddress, domain, nameserver, gateway: string;
    adminName, adminPassword, ipName, ipNumber: string;

  const
    baseRepoUrlOpsi41 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    baseRepoUrlOpsi42 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';

    constructor Create;
  end;

var
  Data: TQuickInstallData;

implementation

{TSplitData}

procedure TSplitData.SetEntries(SetOverviewEntry: string; SetPropertyEntry: string);
begin
  FOverviewEntry := SetOverviewEntry;
  FPropertyEntry := SetPropertyEntry;
end;

{TQuickInstallData}

constructor TQuickInstallData.Create;
begin
  DistrInfo := TDistributionInfo.Create;

  // set default values:
  opsiVersion := 'Opsi 4.2';
  // automatically adjust repo to opsiVersion
  if opsiVersion = 'Opsi 4.2' then
    repo := baseRepoUrlOpsi42
  else
    repo := baseRepoUrlOpsi41;
  proxy := '';
  repoNoCache := repo;

  backend := 'file';
  copyMod := TSplitData.Create;
  copyMod.SetEntries('', 'false');
  repoKind := 'stable';

  ucsPassword := '';
  reboot := TSplitData.Create;
  reboot.SetEntries('', 'false');
  dhcp := TSplitData.Create;
  dhcp.SetEntries('', 'false');
  symlink := 'default.nomenu';

  netmask := '255.255.0.0';
  networkAddress := '192.168.0.0';
  domain := 'uib.local';
  nameserver := '192.168.1.245';

  gateway := '192.168.1.245';
  adminName := 'adminuser';
  adminPassword := 'linux123';
  ipName := 'auto';
  ipNumber := 'auto';
end;

end.
