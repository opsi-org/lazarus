unit OpsiLinuxInstaller_QueryData;

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
    constructor Create;
    property DistrInfo: TDistributionInfo read FDistrInfo write FDistrInfo;
    // TODO: Add query properties
    destructor Destroy; override;
  end;


implementation

constructor TOpsiLinuxInstallerData.Create;
begin
  FDistrInfo := TDistributionInfo.Create;
  // TODO: Set initial values of query properties
end;

destructor TOpsiLinuxInstallerData.Destroy;
begin
  if Assigned(FDistrInfo) then FreeAndNil(FDistrInfo);
  inherited Destroy;
end;

end.

