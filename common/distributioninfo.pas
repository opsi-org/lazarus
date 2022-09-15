unit DistributionInfo;

{$mode objfpc}{$H+}

interface

uses
  // Use e.g. the following commented code instead of the line below it if you
  // have client specific repos and you wrote a unit SupportedOpsiClientDistributions
 (*
  {$IF defined(TARGETOPSISERVER)}
  SupportedOpsiServerDistributions,
  {$ELSEIF defined(TARGETOPSICLIENT)}
  SupportedOpsiClientDistributions,
  {$ENDIF}
  *)
  SupportedOpsiServerDistributions,
  Classes, SysUtils, StrUtils,
  oslog;

type

  TDistributionInfo = class(TObject)
  private
  var
    FDistroName: string;
    FDistroRelease: string;
    FDistribution: TSupportedDistribution;
    FPackageManagementShellCommand: string;

    procedure SetDistribution;
    procedure SetPackageManagementShellCommand;
    procedure FillDistributionInfo(DistroName: string; DistroRelease: string);
  public
    constructor Create(DistroName: string; DistroRelease: string); overload;
    procedure CorrectDistribution(DistroName: string; DistroRelease: string);

    property DistroName: string read FDistroName;
    property DistroRelease: string read FDistroRelease;
    property Distr: TSupportedDistribution read FDistribution;
    property PackageManagementShellCommand: string read FPackageManagementShellCommand;
  end;

function GetPackageManagementShellCommand(DistroName: string): string;

implementation

// A general public function for getting only the package management shell command for a linux distribution name.
// Use TDistributionInfo instead if you also like to check and/or edit a distribution.
function GetPackageManagementShellCommand(DistroName: string): string;
begin
  Result := '';
  {RedHat like}
  if MatchStr(lowerCase(DistroName), ['almalinux', 'redhatenterprise', 'rhel', 'rocky']) then
    Result := 'yum -y '
  {Debian like}
  else if MatchStr(lowerCase(DistroName), ['debian', 'ubuntu', 'univention', 'linuxmint']) then
    Result := 'apt --assume-yes '
  {Suse}
  else if MatchStr(lowerCase(DistroName), ['opensuse', 'suse']) then
    Result := 'zypper --non-interactive ';
end;

{private}
procedure TDistributionInfo.SetDistribution;
begin
  FDistribution := GetSupportedDistribution(DistroName, DistroRelease);
end;

procedure TDistributionInfo.SetPackageManagementShellCommand;
begin
  FPackageManagementShellCommand := GetPackageManagementShellCommand(FDistroName);
end;

procedure TDistributionInfo.FillDistributionInfo(DistroName: string;
  DistroRelease: string);
begin
  FDistroName := DistroName;
  FDistroRelease := DistroRelease;
  LogDatei.log(FDistroName + ' ' + FDistroRelease,
    LLessential);
  SetDistribution;
  if (FDistribution <> other) then SetPackageManagementShellCommand;
end;

{public}
constructor TDistributionInfo.Create(DistroName: string; DistroRelease: string);
  overload;
begin
  inherited Create;
  FillDistributionInfo(DistroName, DistroRelease);
end;

procedure TDistributionInfo.CorrectDistribution(DistroName: string;
  DistroRelease: string);
begin
  FillDistributionInfo(DistroName, DistroRelease);
end;


end.
