unit OpsiLinuxInstaller_QueryData;

// Object to store the data asked in the queries.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  osfunclin,
  DistributionInfo;


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
  // Following line takes time and is therefore executed only once at the
  // beginning of oqi when Data is created.
  FDistrInfo := TDistributionInfo.Create(getLinuxDistroName, getLinuxDistroRelease);
  // TODO: Set initial values of query properties
end;

destructor TOpsiLinuxInstallerData.Destroy;
begin
  if Assigned(FDistrInfo) then FreeAndNil(FDistrInfo);
  inherited Destroy;
end;

end.

