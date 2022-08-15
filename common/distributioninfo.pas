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
    constructor Create(DistroName: string; DistroRelease: string);overload;
    procedure CorrectDistribution(DistroName: string; DistroRelease: string);

    property DistroName: string read FDistroName;
    property DistroRelease: string read FDistroRelease;
    property Distr: TSupportedDistribution read FDistribution;
    property PackageManagementShellCommand: string read FPackageManagementShellCommand;
  end;


implementation

{private}
procedure TDistributionInfo.SetDistribution;
begin
  FDistribution := GetSupportedDistribution(DistroName, DistroRelease);
end;

procedure TDistributionInfo.SetPackageManagementShellCommand;
begin
  {RedHat like}
  if MatchStr(lowerCase(FDistroName), ['almalinux', 'redhatenterprise', 'rocky']) then
    FPackageManagementShellCommand := 'yum -y '
  {Debian like}
  else if MatchStr(lowerCase(FDistroName), ['debian', 'ubuntu', 'univention']) then
    FPackageManagementShellCommand := 'apt --assume-yes '
  {Suse}
  else if MatchStr(lowerCase(FDistroName), ['opensuse', 'suse']) then
    FPackageManagementShellCommand := 'zypper --non-interactive ';
end;

procedure TDistributionInfo.FillDistributionInfo(DistroName: string; DistroRelease: string);
begin
  FDistroName := DistroName;
  FDistroRelease := DistroRelease;
  LogDatei.log(FDistroName + ' ' + FDistroRelease,
      LLessential);
  SetDistribution;
  if (FDistribution <> other) then SetPackageManagementShellCommand;
end;

{public}
constructor TDistributionInfo.Create(DistroName: string; DistroRelease: string);overload;
begin
  inherited Create;
  FillDistributionInfo(DistroName, DistroRelease)
end;

procedure TDistributionInfo.CorrectDistribution(DistroName: string; DistroRelease: string);
begin
  FillDistributionInfo(DistroName, DistroRelease)
end;


end.
