unit opsi_configed_installer_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, osDistributionInfo;


type

  {TConfigedInstallerData}

  TConfigedInstallerData = class(TObject)
  private
    FDistroName: string;
    FDistroRelease: string;
    FDistrInfo: TDistributionInfo;
    FMemoryRequirement: string;
    FScalingFactor: string;
  public
  const
    OpsiVersion = 'Opsi 4.2';
    Repo = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';

    constructor Create;
    property DistroName: string read FDistroName write FDistroName;
    property DistroRelease: string read FDistroRelease write FDistroRelease;
    property DistrInfo: TDistributionInfo read FDistrInfo write FDistrInfo;
    property MemoryRequirement: string read FMemoryRequirement write FMemoryRequirement;
    property ScalingFactor: string read FScalingFactor write FScalingFactor;
  end;

var
  Data: TConfigedInstallerData;

implementation

{TQuickInstallData}

constructor TConfigedInstallerData.Create;
begin
  FDistrInfo := TDistributionInfo.Create;
  FMemoryRequirement := '1024';
  FScalingFactor := '1.0';
end;

end.
