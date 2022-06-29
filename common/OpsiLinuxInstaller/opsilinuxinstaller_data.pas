unit OpsiLinuxInstaller_Data;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DistributionInfo;


type

  TOpsiLinuxInstallerData = class(TObject)
  private
    FDistrInfo: TDistributionInfo;
    // TODO: Add fields of query properties
  public
  const
    OpsiVersion = 'Opsi 4.2';
    Repo = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';

    constructor Create;
    property DistrInfo: TDistributionInfo read FDistrInfo write FDistrInfo;
    // TODO: Add query properties
  end;


implementation

constructor TOpsiLinuxInstallerData.Create;
begin
  FDistrInfo := TDistributionInfo.Create;
  // TODO: Set initial values of query properties
end;

end.

