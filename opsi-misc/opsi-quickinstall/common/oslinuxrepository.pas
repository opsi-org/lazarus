unit osLinuxRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, osTextFileUtils, osLog, Dialogs,
  osRunCommandElevated, BaseUnix;

type

  {TDistribution: distributions which opsi-server supports, add any new supported distribution here }
  TDistribution = (CentOS_7,
    CentOS_8,
    Debian_8,
    Debian_9,
    Debian_10,
    openSUSE_Leap_15_1,
    openSUSE_Leap_15_2,
    openSUSE_Leap_42_3,
    RHEL_7,
    RHEL_8,
    SLE_12,
    SLE12_SP1,
    SLE12_SP2,
    SLE12_SP3,
    SLE12_SP4,
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
  var
    FDistribution: TDistribution; //Linux distribution
    FOpsiVersion: TOpsiVersion;
    FOpsiBranch: TOPsiBranch;
    FURL: string;
    FRunCommandElevated: TRunCommandElevated;

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
    CentOS_7: Result := Result + 'CentOS_7/';
    CentOS_8: Result := Result + 'CentOS_8/';
    Debian_8: Result := Result + 'Debian_8/';
    Debian_9: Result := Result + 'Debian_9/';
    Debian_10: Result := Result + 'Debian_10/';
    openSUSE_Leap_15_1: Result := Result + 'openSUSE_Leap_15.1/';
    openSUSE_Leap_15_2: Result := Result + 'openSUSE_Leap_15.2/';
    openSUSE_Leap_42_3: Result := Result + 'openSUSE_Leap_42.3/';
    RHEL_7: Result := Result + 'RHEL_7/';
    RHEL_8: Result := Result + 'RHEL_8/';
    SLE_12: Result := Result + 'SLE_12/';
    SLE12_SP1: Result := Result + 'SLE12_SP1/';
    SLE12_SP2: Result := Result + 'SLE12_SP2/';
    SLE12_SP3: Result := Result + 'SLE12_SP3/';
    SLE12_SP4: Result := Result + 'SLE12_SP4/';
    Univention_4_3: Result := Result + 'Univention_4.3/';
    Univention_4_4: Result := Result + 'Univention_4.4/';
    xUbuntu_16_04: Result := Result + 'xUbuntu_16.04/';
    xUbuntu_18_04: Result := Result + 'xUbuntu_18.04/';
    xUbuntu_20_04: Result := Result + 'xUbuntu_20.04/';
    other: Result := '';
  end;
  //ShowMessage(Result);
end;

procedure TLinuxRepository.AddDebianUbuntu;
var
  Owner, Output: string;
  //Buffer:stat;
  //ErrorNr:integer;
begin
  //ShowMessage(FSourcesListFilePath);
  try
    if FileExists(FSourcesListFilePath) then
    begin
      //fpStat(FSourcesListFilePath,Buffer);
      //LogDatei.log('uid: ' + IntToStr(Buffer.st_uid),LLInfo);
      //ErrorNr := fpChown(FSourcesListFilePath,fpGetUid,Buffer.st_gid);
      //LogDatei.Log('ErrorNr: ' + IntToStr(ErrorNr),LLInfo);
      Owner := FRunCommandElevated.Run('stat -c "%U" ' + FSourcesListFilePath);
      Owner := StringReplace(Owner, LineEnding, '', [rfReplaceAll]);
      //ShowMessage(Owner); // result in Anja-M.'s case with ubuntu 18.04: root
      LogDatei.log('Owner: ' + Owner, LLInfo);
      // following code line moved down after else part for not getting trouble with AddLineToTextFile
      //FRunCommandElevated.Run('chown -c $USER ' + FSourcesListFilePath);
    end
    else
    begin
      Owner := 'root';
      FRunCommandElevated.Run('touch ' + FSourcesListFilePath);
    end;
    // change owner of file FSourcesListFilePath from root to user
    Output := FRunCommandElevated.Run('chown -c $USER ' + FSourcesListFilePath);
    //ShowMessage(Output);

    AddLineToTextFile('deb ' + FURL + ' /', FSourcesListFilePath);

    // change owner of file FSourcesListFilePath from user to root
    Output := (FRunCommandElevated.Run('chown -c ' + Owner + ' ' +
      FSourcesListFilePath));
    //ShowMessage(Output);

    FRunCommandelevated.Run('wget -nv' + ' ' + FURL + 'Release.key -O' +
      ' ' + 'Release.key');
    // for beeing sure again:
    FRunCommandelevated.Run('wget -nv' + ' ' + FURL + 'Release.key');
    // apt-key add is deprecated (last available in Debian 11, Ubuntu 22.04) and needs gnupg
    FRunCommandelevated.Run('apt-get install gnupg2');
    FRunCommandElevated.Run('apt-key add - < Release.key');
    FRunCommandElevated.Run('rm Release.key');
  except
    LogDatei.log('Exception while adding repository.', LLDebug);
  end;

end;

procedure TLinuxRepository.AddOpenSuseSLES(RepoName: string);
begin
  // zypper addrepo <options> <URI> <alias>
  writeln('zypper addrepo ' + FURL + ' ' + RepoName);
  FRunCommandElevated.Run('zypper addrepo ' + FURL + ' ' + RepoName);
  writeln('zypper --no-gpg-checks --non-interactive --gpg-auto-import-keys refresh');
  FRunCommandElevated.Run('zypper --no-gpg-checks --non-interactive --gpg-auto-import-keys refresh');
end;

procedure TLinuxRepository.AddCentOSRedHat;
begin
  if SetCurrentDir('/etc/yum.repos.d/') then
  begin
    FRunCommandElevated.Run('wget ' + FURL);
    FRunCommandElevated.Run('yum makecache');
  end
  else
    LogDatei.log('Could not set directory to /etc/yum.repos.d/', LLInfo);
end;

procedure TLinuxRepository.Add(URL: string);
begin
  FURL := URL;
  case FDistribution of
    {Debian and Ubuntu}
    Debian_8, Debian_9, Debian_10,
    xUbuntu_16_04, xUbuntu_18_04, xUbuntu_20_04:
    begin
      AddDebianUbuntu;
    end;
    {CentOS and RedHat}
    CentOS_7, CentOS_8, RHEL_7, RHEL_8:
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
    openSUSE_Leap_15_1, openSUSE_Leap_15_2, openSUSE_Leap_42_3,
    SLE_12, SLE12_SP1, SLE12_SP2, SLE12_SP3, SLE12_SP4:
    begin
      AddOpenSuseSLES(RepoName);
    end;
  end;
end;

end.
