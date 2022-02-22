unit OckPathsUtils;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type
  TPaths = class
    FKioskApp: string;
    FCustomIcons: string;
    FCustomScreenShots: string;
    FCustomSettings: string;
    FCustomSkin: string;
  end;

  { TPathsOnDepot }

  TPathsOnDepot = class(TPaths)
  private
    FShare: string;
    procedure SetShare(theShare: string);
  public
    procedure SetDepotPaths; virtual; abstract;
    property Share: string read FShare write SetShare;
  end;

  TPathsOnClient = class(TPaths)
  private
    procedure SetAdminMode(theAdminMode: boolean); virtual;
  public
    FDefaultSettings: string;
    FDefaultIcons: string;
    FDefaultSkin: string;
    FAdminMode: boolean;
    procedure SetUserModePaths; virtual; abstract;
    procedure SetAdminModePaths; virtual; abstract;
    procedure InitPaths;
    constructor Create; virtual;
    destructor Destroy; override;
    property AdminMode: boolean read FAdminMode write SetAdminMode;
  end;


implementation

{ TPathsOnDepot }

procedure TPathsOnDepot.SetShare(theShare: string);
begin
  FShare := theShare;
  SetDepotPaths;
end;

procedure TPathsOnClient.SetAdminMode(theAdminMode: boolean);
begin
  FAdminMode := theAdminMode;
  InitPaths;
end;

procedure TPathsOnClient.InitPaths;
begin
  if FAdminMode then
  begin
    SetAdminModePaths;
  end
  else
  begin
    SetUserModePaths;
  end;
end;

constructor TPathsOnClient.Create;
begin
  inherited Create;
  InitPaths;
end;

destructor TPathsOnClient.Destroy;
begin
  inherited Destroy;
end;


end.

