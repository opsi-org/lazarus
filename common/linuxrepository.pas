unit LinuxRepository;

(*
Here is an example of how to use this unit in one of the easiest ways:

MyRepo := TLinuxRepository.Create(MyPassword,False,'MyKeyRingName.gpg','myrepos.list');
MyRepo.Add('Ubuntu','https://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/stable/xUbuntu22.04/')

Done :)
*)

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

  Classes, SysUtils, Process, StrUtils,
  osTextFileUtils,
  osLog,
  osRunCommandElevated;

type

  // Add any new opsi Version here
  TOpsiVersion = (Opsi41, Opsi42);

  (* The branches of Opsi. Are you adventurous and interested in new functionalities use the experimental branch,
  if you are not sure what to choose or you make your first steps in Opsi then use the stable branch.*)
  TOpsiBranch = (experimental, testing, stable);

  TLinuxRepository = class(TObject)
  private
  const
    // Maybe BaseURLOpsi42 has to be adapted in the future
    FDefaultBaseURLOpsi41 =
      'https://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    FDefaultBaseURLOpsi42 =
      'https://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
    // Required for adding repos to Debian like distributions
    FSourcesListDirectory = '/etc/apt/sources.list.d/';
    FKeyRingsPath = '/usr/local/share/keyrings/';
  var
    FURL: string;
    FRunCommandElevated: TRunCommandElevated;
    FSourcesListFilePath: string;
    FOwnerOfSourcesList: string;
    FKeyPath: string;

    procedure CreateKeyRingAndAddKey;
    procedure ReadOwnerOfExistingSourcesList;
    procedure CreateSourcesListAsRoot;
    procedure AddRepoToSourcesListByKey;
    procedure AddDebianRepo;
    procedure AddRedhatRepo;
  public
    constructor Create(Password: string; Sudo: boolean = False;
      KeyRingName: string = 'OpsiQuickInstall.gpg'; RepoListFileName: string = 'opsi.list');
    // Constructs the repository URL based on distribution, opsi version and opsi branch and gives it back as result
    function GetOpsiServerRepoDefaultURL(OpsiVersion: TOpsiVersion; OpsiBranch: TOpsiBranch;
      Distribution: TSupportedDistribution): string;
    // Add repository URL to the package system of the OS
    procedure Add(DistroName: string; URL: string);
    procedure AddSuseRepo(URL: string; RepoName: string);
  end;

function stringToOpsiBranch(branch: string): TOpsiBranch;

implementation

function stringToOpsiBranch(branch: string): TOpsiBranch;
begin
  if branch = 'experimental' then Result := experimental
  else if branch = 'testing' then Result := testing
  else if branch = 'stable' then Result := stable;
end;

{private}
procedure TLinuxRepository.CreateKeyRingAndAddKey;
var
  Output: string;
begin
  FRunCommandElevated.Run('mkdir -p ' + FKeyRingsPath, Output);
  FRunCommandElevated.Run(
    'apt install -y apt-transport-https software-properties-common curl gpg', Output);

  FRunCommandElevated.Run('curl -fsSL ' + FURL +
    'Release.key | gpg --dearmor | sudo tee ' + FKeyPath + ' > /dev/null', Output);
  //FRunCommandElevated.Run('gpg ' + FKeyPath + ' 2>/dev/null', Output); //check if key import worked
end;

procedure TLinuxRepository.ReadOwnerOfExistingSourcesList;
begin
  FRunCommandElevated.Run('stat -c "%U" ' + FSourcesListFilePath, FOwnerOfSourcesList);
  FOwnerOfSourcesList := StringReplace(FOwnerOfSourcesList, LineEnding, '',
    [rfReplaceAll]);
  LogDatei.log('Owner: ' + FOwnerOfSourcesList, LLInfo);
end;

procedure TLinuxRepository.CreateSourcesListAsRoot;
var
  Output: string;
begin
  FOwnerOfSourcesList := 'root';
  FRunCommandElevated.Run('touch ' + FSourcesListFilePath, Output);
end;

procedure TLinuxRepository.AddRepoToSourcesListByKey;
var
  Output: string;
begin
  if FileExists(FSourcesListFilePath) then
    ReadOwnerOfExistingSourcesList
  else
    CreateSourcesListAsRoot;

  // change owner of file FSourcesListFilePath from root to user
  FRunCommandElevated.Run('chown -c $USER ' + FSourcesListFilePath, Output);
  // add repo
  AddLineToTextFile('deb [signed-by=' + FKeyPath + '] ' + FURL + ' /',
    FSourcesListFilePath);
  // change owner of file FSourcesListFilePath from user to root
  FRunCommandElevated.Run('chown -c ' + FOwnerOfSourcesList + ' ' +
    FSourcesListFilePath, Output);
end;

procedure TLinuxRepository.AddDebianRepo;
begin
  try
    CreateKeyRingAndAddKey;
    AddRepoToSourcesListByKey;
  except
    LogDatei.log('Exception while adding repository.', LLDebug);
  end;
end;

procedure TLinuxRepository.AddRedhatRepo;
var
  Output: string;
begin
  if SetCurrentDir('/etc/yum.repos.d/') then
  begin
    FRunCommandElevated.Run('yum install -y yum-utils', Output);
    FRunCommandElevated.Run('yum-config-manager --add-repo ' + FURL, Output);
  end
  else
    LogDatei.log('Could not set directory to /etc/yum.repos.d/', LLInfo);
end;

{public}
constructor TLinuxRepository.Create(Password: string; Sudo: boolean = False;
  KeyRingName: string = 'OpsiQuickInstall.gpg'; RepoListFileName: string = 'opsi.list');
begin
  FRunCommandElevated := TRunCommandElevated.Create(Password, Sudo);
  FKeyPath := FKeyRingsPath + KeyRingName;
  FSourcesListFilePath := FSourcesListDirectory + RepoListFileName;
end;

// GetOpsiServerRepoDefaultURL can be helpful but is not necessary
function TLinuxRepository.GetOpsiServerRepoDefaultURL(OpsiVersion: TOpsiVersion;
  OpsiBranch: TOpsiBranch; Distribution: TSupportedDistribution): string;
var
  RedHatRepoName: string = 'home:uibmz:opsi:';
begin
  Result := '';

  case OpsiVersion of
    Opsi41: Result := FDefaultBaseURLOpsi41;
    Opsi42: Result := FDefaultBaseURLOpsi42;
  end;
  case OpsiBranch of
    experimental: Result := Result + 'experimental/';
    testing: Result := Result + 'testing/';
    stable: Result := Result + 'stable/';
  end;

  // For redhat compose the specific repo name of the default repo
  case OpsiVersion of
    Opsi41: RedHatRepoName := RedHatRepoName + '4.1:';
    Opsi42: RedHatRepoName := RedHatRepoName + '4.2:';
  end;
  case OpsiBranch of
    experimental: RedHatRepoName := RedHatRepoName + 'experimental.repo';
    testing: RedHatRepoName := RedHatRepoName + 'testing.repo';
    stable: RedHatRepoName := RedHatRepoName + 'stable.repo';
  end;

  case Distribution of
    AlmaLinux_8: Result := Result + 'AlmaLinux_8/';
    AlmaLinux_9: Result := Result + 'AlmaLinux_9/';
    Debian_9: Result := Result + 'Debian_9/';
    Debian_10: Result := Result + 'Debian_10/';
    Debian_11: Result := Result + 'Debian_11/';
    openSUSE_Leap_15_3: Result := Result + 'openSUSE_Leap_15.3/';
    openSUSE_Leap_15_4: Result := Result + 'openSUSE_Leap_15.4/';
    RHEL_8: Result := Result + 'RHEL_8/' + RedHatRepoName;
    RHEL_9: Result := Result + 'RHEL_9/' + RedHatRepoName;
    RockyLinux_8: Result := Result + 'RockyLinux_8/';
    RockyLinux_9: Result := Result + 'RockyLinux_9/';
    SLE15_SP1: Result := Result + 'SLE_15_SP1/';
    SLE15_SP2: Result := Result + 'SLE_15_SP2/';
    SLE15_SP3: Result := Result + 'SLE_15_SP3/';
    SLE15_SP4: Result := Result + 'SLE_15_SP4/';
    Univention_4_4: Result := Result + 'Univention_4.4/';
    Univention_5_0: Result := Result + 'Univention_5.0/';
    xUbuntu_18_04: Result := Result + 'xUbuntu_18.04/';
    xUbuntu_20_04: Result := Result + 'xUbuntu_20.04/';
    xUbuntu_22_04: Result := Result + 'xUbuntu_22.04/';
    other: Result := '';
  end;
end;

// For suse call AddSuseRepo directly with your desired repo name
// or call below function Add and the repo will get a default name.
procedure TLinuxRepository.AddSuseRepo(URL: string; RepoName: string);
var
  Output: string;
begin
  FURL := URL;
  // zypper addrepo <options> <URI> <alias>
  FRunCommandElevated.Run('zypper addrepo ' + FURL + ' ' + RepoName, Output);
  FRunCommandElevated.Run(
    'zypper --no-gpg-checks --non-interactive --gpg-auto-import-keys refresh',
    Output);
end;

procedure TLinuxRepository.Add(DistroName: string; URL: string);
begin
  FURL := URL;
  if MatchStr(lowerCase(DistroName), ['debian', 'ubuntu', 'univention']) then
    AddDebianRepo
  else if MatchStr(lowerCase(DistroName), ['almalinux', 'redhatenterprise', 'rocky']) then
    AddRedhatRepo
  else if MatchStr(lowerCase(DistroName), ['opensuse', 'suse']) then
    AddSuseRepo(URL, 'DefaultSuseRepoName');
end;


end.
