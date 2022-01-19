unit opsi_configed_installer_data;

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

  {TConfigedInstallerData}

  TConfigedInstallerData = class(TObject)
  public
    distroName: string;
    distroRelease: string;
    DistrInfo: TDistributionInfo;

    opsiVersion: string;
    repo: string;
    repoNoCache: string;

    constructor Create;
  end;

var
  Data: TConfigedInstallerData;

implementation

{TSplitData}

procedure TSplitData.SetEntries(SetOverviewEntry: string; SetPropertyEntry: string);
begin
  OverviewEntry := SetOverviewEntry;
  PropertyEntry := SetPropertyEntry;
end;

{TQuickInstallData}

constructor TConfigedInstallerData.Create;
begin
  DistrInfo := TDistributionInfo.Create;
  opsiVersion := 'Opsi 4.2';
  repo := 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
  repoNoCache := repo;
end;

end.

