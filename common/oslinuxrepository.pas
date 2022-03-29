unit osLinuxRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, osTextFileUtils, osLog,
  osRunCommandElevated, BaseUnix;

type

  {TDistribution: distributions which opsi-server supports, add any new supported distribution here }
  TDistribution = (AlmaLinux_8,
    CentOS_7,
    CentOS_8,
    Debian_8,
    Debian_9,
    Debian_10,
    Debian_11,
    openSUSE_Leap_15_1,
    openSUSE_Leap_15_2,
    openSUSE_Leap_15_3,
    openSUSE_Leap_42_3,
    RHEL_7,
    RHEL_8,
    RockyLinux_8,
    SLE_12,
    SLE12_SP1,
    SLE12_SP2,
    SLE12_SP3,
    SLE12_SP4,
    SLE15_SP1,
    SLE15_SP2,
    Univention_4_3,
    Univention_4_4,
    xUbuntu_16_04,
    xUbuntu_18_04,
    xUbuntu_20_04,
    other
    );

  {TOpsiVersion: add any new opsi Version here}
  TOpsiVersion = (Opsi41, Opsi42);

  {TOpsiBranch: The branches of Opsi. Are you adventurous and interested in new functionalities use the experimental branch,
  if you are not sure what to choose or you make your first steps in Opsi then use the stable branch}
  TOpsiBranch = (experimental, testing, stable);

  { TLinuxRepository }

  TLinuxRepository = class(TObject)
  private
  const
    { Maybe BaseURLOpsi42 has to be adapted in the future }
    FBaseURLOpsi41 =
      'https://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    FBaseURLOpsi42 =
      'https://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
    FSourcesListDirectory = '/etc/apt/sources.list.d'; //Debian/Ubuntu
    FSourcesListFilePath = FSourcesListDirectory + '/opsi.list'; //Debian/Ubuntu
    FKeyRingPath = '/usr/local/share/keyrings'; //Debian/Ubuntu
    FKeyPath = FKeyRingPath + '/opensuseOpsi.gpg'; //Debian/Ubuntu
  var
    FDistribution: TDistribution; //Linux distribution
    FOpsiVersion: TOpsiVersion;
    FOpsiBranch: TOpsiBranch;
    FURL: string;
    FRunCommandElevated: TRunCommandElevated;
    FOwnerOfSourcesList: string;

    procedure CreateKeyRingAndAddKey;
    procedure CreateSourcesListIfNotExistentAndDetermineOwner;
    procedure AddRepoToSourcesListByKey;
    procedure AddDebianUbuntu;
    procedure AddOpenSuseSLES(RepoName: string);
    procedure AddCentOSRedHat;
  public
    constructor Create(Distribution: TDistribution; Password: string;
      Sudo: boolean = False);

    destructor Destroy; override;
    function GetDefaultURL(OpsiVersion: TOpsiVersion; OpsiBranch: TOpsiBranch): string;
    { Constructs the repository URL based on distribution, opsi version and opsi branch and gives it back as result}
    procedure Add(URL: string);
    procedure Add(URL: string; RepoName: string);
    { Add repository URL to the package system of the OS }
    property URL: string read FURL;
  end;

function stringToOpsiBranch(branch: string): TOpsiBranch;

implementation

function stringToOpsiBranch(branch: string): TOpsiBranch;
begin
   if branch = 'experimental' then Result := experimental
   else if branch = 'testing' then Result := testing
   else if branch = 'stable' then Result := stable
end;

constructor TLinuxRepository.Create(Distribution: TDistribution;
  Password: string; Sudo: boolean = False);
begin
  //LogDatei := TLogInfo.Create;
  //LogDatei.CreateTheLogfile('opsiLinuxRepository.log');
  //LogDatei.CreateTheLogFile(ProgramInfo.InternalName +'.log');
  FDistribution := Distribution;
  FOpsiVersion := Opsi41;
  FOpsiBranch := stable;
  FURL := GetDefaultURL(Opsi41, stable);
  FRunCommandElevated := TRunCommandElevated.Create(Password, Sudo);
end;

destructor TLinuxRepository.Destroy;
begin
  FRunCommandElevated.Free;
  //if assigned(LogDatei) then FreeAndNil(LogDatei);
  inherited Destroy;
end;

function TLinuxRepository.GetDefaultURL(OpsiVersion: TOpsiVersion;
  OpsiBranch: TOpsiBranch): string;
begin
  Result := '';
  FOpsiVersion := OpsiVersion;
  FOpsiBranch := OpsiBranch;
  case FOpsiVersion of
    Opsi41: Result := FBaseURLOpsi41;
    Opsi42: Result := FBaseURLOpsi42;
  end;
  case FOpsiBranch of
    experimental: Result := Result + 'experimental/';
    testing: Result := Result + 'testing/';
    stable: Result := Result + 'stable/';
  end;
  case FDistribution of
    AlmaLinux_8: Result := Result + 'AlmaLinux_8/';
    CentOS_7: Result := Result + 'CentOS_7/';
    CentOS_8: Result := Result + 'CentOS_8/';
    Debian_8: Result := Result + 'Debian_8/';
    Debian_9: Result := Result + 'Debian_9/';
    Debian_10: Result := Result + 'Debian_10/';
    Debian_11: Result := Result + 'Debian_11/';
    openSUSE_Leap_15_1: Result := Result + 'openSUSE_Leap_15.1/';
    openSUSE_Leap_15_2: Result := Result + 'openSUSE_Leap_15.2/';
    openSUSE_Leap_15_3: Result := Result + 'openSUSE_Leap_15.3/';
    openSUSE_Leap_42_3: Result := Result + 'openSUSE_Leap_42.3/';
    RHEL_7: Result := Result + 'RHEL_7/';
    RHEL_8: Result := Result + 'RHEL_8/';
    RockyLinux_8: Result := Result + 'RockyLinux_8/';
    SLE_12: Result := Result + 'SLE_12/';
    SLE12_SP1: Result := Result + 'SLE12_SP1/';
    SLE12_SP2: Result := Result + 'SLE12_SP2/';
    SLE12_SP3: Result := Result + 'SLE12_SP3/';
    SLE12_SP4: Result := Result + 'SLE12_SP4/';
    SLE15_SP1: Result := Result + 'SLE_15_SP1/';
    SLE15_SP2: Result := Result + 'SLE_15_SP2/';
    Univention_4_3: Result := Result + 'Univention_4.3/';
    Univention_4_4: Result := Result + 'Univention_4.4/';
    xUbuntu_16_04: Result := Result + 'xUbuntu_16.04/';
    xUbuntu_18_04: Result := Result + 'xUbuntu_18.04/';
    xUbuntu_20_04: Result := Result + 'xUbuntu_20.04/';
    other: Result := '';
  end;
end;

procedure TLinuxRepository.CreateKeyRingAndAddKey;
var
  Output: string;
begin
  FRunCommandElevated.Run('mkdir -p ' + FKeyRingPath, Output);
  FRunCommandElevated.Run(
    'apt install -y apt-transport-https software-properties-common curl gpg', Output);

  FRunCommandElevated.Run('curl -fsSL ' + FURL +
    'Release.key | gpg --dearmor | sudo tee ' + FKeyPath + ' > /dev/null', Output);
  //FRunCommandElevated.Run('gpg ' + FKeyPath + ' 2>/dev/null', Output); //check if key import worked
end;

procedure TLinuxRepository.CreateSourcesListIfNotExistentAndDetermineOwner;
var
  Output: string;
  //Buffer:stat;
  //ErrorNr:integer;
begin
  if FileExists(FSourcesListFilePath) then
    begin
      //fpStat(FSourcesListFilePath,Buffer);
      //LogDatei.log('uid: ' + IntToStr(Buffer.st_uid),LLInfo);
      //ErrorNr := fpChown(FSourcesListFilePath,fpGetUid,Buffer.st_gid);
      //LogDatei.Log('ErrorNr: ' + IntToStr(ErrorNr),LLInfo);
      FRunCommandElevated.Run('stat -c "%U" ' + FSourcesListFilePath, FOwnerOfSourcesList);
      FOwnerOfSourcesList := StringReplace(FOwnerOfSourcesList, LineEnding, '', [rfReplaceAll]);
      LogDatei.log('Owner: ' + FOwnerOfSourcesList, LLInfo);
    end
    else
    begin
      FOwnerOfSourcesList := 'root';
      FRunCommandElevated.Run('touch ' + FSourcesListFilePath, Output);
    end;
end;

procedure TLinuxRepository.AddRepoToSourcesListByKey;
var
  Output: string;
begin
  CreateSourcesListIfNotExistentAndDetermineOwner;
  // change owner of file FSourcesListFilePath from root to user
  FRunCommandElevated.Run('chown -c $USER ' + FSourcesListFilePath, Output);
  // add repo
  AddLineToTextFile('deb [signed-by=' + FKeyPath + '] ' + FURL + ' /',
    FSourcesListFilePath);
  // change owner of file FSourcesListFilePath from user to root
  FRunCommandElevated.Run('chown -c ' + FOwnerOfSourcesList + ' ' +
    FSourcesListFilePath, Output);
end;

procedure TLinuxRepository.AddDebianUbuntu;
begin
  try
    CreateKeyRingAndAddKey;
    AddRepoToSourcesListByKey;
  except
    LogDatei.log('Exception while adding repository.', LLDebug);
  end;

end;

procedure TLinuxRepository.AddOpenSuseSLES(RepoName: string);
var
  Output: string;
begin
  // zypper addrepo <options> <URI> <alias>
  //writeln('zypper addrepo ' + FURL + ' ' + RepoName);
  LogDatei.log('zypper addrepo ' + FURL + ' ' + RepoName, LLInfo);
  FRunCommandElevated.Run('zypper addrepo ' + FURL + ' ' + RepoName, Output);
  //writeln('zypper --no-gpg-checks --non-interactive --gpg-auto-import-keys refresh');
  LogDatei.log('zypper --no-gpg-checks --non-interactive --gpg-auto-import-keys refresh', LLInfo);
  FRunCommandElevated.Run('zypper --no-gpg-checks --non-interactive --gpg-auto-import-keys refresh', Output);
end;

procedure TLinuxRepository.AddCentOSRedHat;
var
  Output: string;
begin
  if SetCurrentDir('/etc/yum.repos.d/') then
  begin
    FRunCommandElevated.Run('wget ' + FURL, Output);
    FRunCommandElevated.Run('yum makecache', Output);
  end
  else
    LogDatei.log('Could not set directory to /etc/yum.repos.d/', LLInfo);
end;

procedure TLinuxRepository.Add(URL: string);
begin
  FURL := URL;
  case FDistribution of
    {Debian and Ubuntu}
    Debian_8, Debian_9, Debian_10, Debian_11,
    Univention_4_3, Univention_4_4,
    xUbuntu_16_04, xUbuntu_18_04, xUbuntu_20_04:
    begin
      AddDebianUbuntu;
    end;
    {CentOS and RedHat}
    AlmaLinux_8, CentOS_7, CentOS_8, RHEL_7, RHEL_8, RockyLinux_8:
    begin
      AddCentOSRedHat;
    end;
  end;
end;

procedure TLinuxRepository.Add(URL: string; RepoName: string);
begin
  FURL := URL;
  case FDistribution of
    {OpenSuse and SLES}
    openSUSE_Leap_15_1, openSUSE_Leap_15_2, openSUSE_Leap_15_3, openSUSE_Leap_42_3,
    SLE_12, SLE12_SP1, SLE12_SP2, SLE12_SP3, SLE12_SP4, SLE15_SP1, SLE15_SP2:
    begin
      AddOpenSuseSLES(RepoName);
    end;
  end;
end;

end.
