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
  end;

  TPathsOnDepot = class(TPaths)

  end;

  TPathsOnClient = class(TPaths)
    FDefaultIcons: string;
  end;

  { TOckPaths }

  TOckPaths = class
  private
    procedure SetAdminMode(theAdminMode: boolean);
  public
   //Paths On client
    FOnClient: TPathsOnClient;
    //Paths on depot
    FOnDepot: TPathsOnDepot;
    FAdminMode: boolean;
    procedure SetUserModePaths; virtual; abstract;
    procedure SetAdminModePaths; virtual; abstract;
    procedure InitPaths;
    constructor Create; virtual;
    destructor destroy; override;
    property AdminMode: boolean read FAdminMode write SetAdminMode;
  end;

implementation

{ TOckPaths }

procedure TOckPaths.InitPaths;
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

procedure TOckPaths.SetAdminMode(theAdminMode: boolean);
begin
  FAdminMode := theAdminMode;
  InitPaths;
end;


constructor TOckPaths.Create;
begin
  inherited Create;
  FOnClient := TPathsOnClient.Create;
  FOnDepot := TPathsOnDepot.Create;
end;

destructor TOckPaths.destroy;
begin
  inherited destroy;
  FreeAndNil(FOnClient);
  FreeAndNil(FOnDepot);
end;

end.

