program opsi_quick_install_project;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Process,
  GetText,
  Translations,
  osDistributionInfo,
  osRunCommandElevated,
  osfunclin,
  osLinuxRepository,
  opsi_quick_install_resourcestrings,
  oslog;

type

  { TQuickInstall }

  TQuickInstall = class(TCustomApplication)
  private
  var
    input, setupType, distroName, distroRelease: string;
    DistrInfo: TDistributionInfo;
    opsiVersion, repo, proxy, repoNoCache: string;
    backend, copyMod, repoKind: string;
    ucsPassword, reboot, dhcp, link: string;
    netmask, networkAddress, domain, nameserver, gateway: string;
    adminName, adminPassword, ipName, ipNumber: string;
    FileText, PropsFile: TStringList;
    MyRepo: TLinuxRepository;
    InstallOpsiCommand: TRunCommandElevated;
    DirClientData, url, shellCommand, Output: string;
  const
    baseUrlOpsi41 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    baseUrlOpsi42 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';

    // set default values for all required variables
    procedure SetDefaultValues;
    // write properties in l-opsi-server.conf and properties.conf file
    procedure WritePropsToFile;
    // install opsi-server
    procedure InstallOpsi;

    procedure NoGuiQuery;
    procedure QueryDistribution;
    procedure QueryOpsiVersion;
    procedure QueryRepo;
    procedure QueryProxy;
    procedure QueryRepoNoCache;
    procedure QueryBackend;
    procedure QueryModules;
    procedure QueryRepoKind;
    procedure QueryUCS;
    procedure QueryReboot;
    procedure QueryDhcp;
    procedure QueryLink;
    procedure QueryNetmask;
    procedure QueryNetworkAddress;
    procedure QueryDomain;
    procedure QueryNameserver;
    procedure QueryGateway;
    procedure QueryAdminName;
    procedure QueryAdminPass;
    procedure QueryIPName;
    procedure QueryIPNumber;
    procedure QueryOverview;

    procedure ExecuteWithDefaultValues;
    procedure ReadProps;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


  { TQuickInstall }

  procedure TQuickInstall.DoRun;
  var
    ErrorMsg, project: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('htgndf:', 'help test gui nogui default file:');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      //Terminate;
      //Exit;
    end;

    if HasOption('t', 'test') then
    begin
      writeln(rsFinish);
      Terminate;
      Exit;
    end;

    if HasOption('g', 'gui') then
    begin
      // don't call gui version
      {project := ExtractFilePath(ParamStr(0));
      Delete(project, Length(project), 1);
      project := ExtractFilePath(project) + 'gui/opsi_quick_install_project';
      writeln(project);
      ExecuteProcess(project, '', []);}
      Terminate;
      Exit;
    end;

    if HasOption('n', 'nogui') then
    begin
      NoGuiQuery;
      Terminate;
      Exit;
    end;

    if HasOption('d', 'default') then
    begin
      ExecuteWithDefaultValues;
      Terminate;
      Exit;
    end;

    if HasOption('f', 'file') then
    begin
      //writeln(getOptionValue('f', 'file'));
      // read properties from file
      PropsFile := TStringList.Create;
      try
        begin
          PropsFile.LoadFromFile(getOptionValue('f', 'file'));
          ReadProps;
        end;
      except
        writeln('Executing Opsi Quick-Install didn''t work!');
      end;
      PropsFile.Free;
      Terminate;
      Exit;
    end;

    { add your program here }
    // stop program loop
    Terminate;
    Exit;
  end;

  constructor TQuickInstall.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TQuickInstall.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TQuickInstall.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
    writeln('Exit');
  end;

  // set default values for all required variables
  procedure TQuickInstall.SetDefaultValues;
  begin
    // set default values:
    opsiVersion := 'Opsi 4.2';
    // repo depending on opsi version
    if opsiVersion = 'Opsi 4.1' then
      repo := baseUrlOpsi41
    else
      repo := baseUrlOpsi42;
    proxy := '';
    repoNoCache := repo;
    backend := 'file';
    copyMod := rsNo;
    repoKind := 'experimental';
    ucsPassword := '';
    reboot := rsNo;
    dhcp := rsNo;
    link := 'default.nomenu';
    netmask := '255.255.0.0';
    networkAddress := '192.168.0.0';
    domain := 'uib.local';
    nameserver := '192.168.1.245';
    gateway := '192.168.1.245';
    adminName := 'Antalya';
    adminPassword := 'linux123';
    ipName := 'auto';
    ipNumber := 'auto';
  end;

  // write properties in l-opsi-server.conf and properties.conf file
  procedure TQuickInstall.WritePropsToFile;
  begin
    // write file text
    FileText := TStringList.Create;
    if reboot = rsYes then
      FileText.Add('allow_reboot=true')
    else
      FileText.Add('allow_reboot=false');
    FileText.Add('backend=' + backend);
    FileText.Add('dnsdomain=' + domain);
    if copyMod = rsYes then
      FileText.Add('force_copy_modules=true')
    else
      FileText.Add('force_copy_modules=false');
    FileText.Add('gateway=' + gateway);
    if dhcp = rsYes then
      FileText.Add('install_and_configure_dhcp=true')
    else
      FileText.Add('install_and_configure_dhcp=false');
    FileText.Add('myipname=' + ipName);
    FileText.Add('myipnumber=' + ipNumber);
    FileText.Add('nameserver=' + nameserver);
    FileText.Add('netmask=' + netmask);
    FileText.Add('network=' + networkAddress);
    FileText.Add('opsi_admin_user_name=' + adminName);
    FileText.Add('opsi_admin_user_password=' + adminPassword);
    FileText.Add('opsi_online_repository=' + repo);
    FileText.Add('opsi_noproxy_online_repository=' + repoNoCache);
    FileText.Add('patch_default_link_for_bootimage=' + link);
    FileText.Add('proxy=' + proxy);
    FileText.Add('repo_kind=' + repoKind);
    FileText.Add('ucs_master_admin_password=' + ucsPassword);
    // update_test shall always be false
    FileText.Add('update_test=false');

    // write in l-opsi-server.conf file:
    FileText.SaveToFile(ExtractFilePath(ParamStr(0)) + 'l-opsi-server.conf');
    // write in properties.conf file:
    FileText.SaveToFile(DirClientData + 'properties.conf');

    FileText.Free;
  end;

  // install opsi-server
  // requires: opsiVersion, repoKind, distroName, DistrInfo, existing LogDatei
  procedure TQuickInstall.InstallOpsi;
  begin
    if (opsiVersion = 'Opsi 4.2') and (repoKind <> 'experimental') then
    begin
      writeln('The branch "' + repoKind + '" is not available for Opsi 4.2');
      Exit;
    end;

    // Set text of result.conf to 'failed' first (for safety)
    FileText := TStringList.Create;
    FileText.Add('failed');
    FileText.SaveToFile(DirClientData + 'result.conf');

    // create repository (no password, user is root)
    MyRepo := TLinuxRepository.Create(DistrInfo.MyDistr, '', False);
    // Set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
    if opsiVersion = 'Opsi 4.1' then
    begin
      if repoKind = 'experimental' then
        url := MyRepo.GetDefaultURL(Opsi41, experimental)
      else if repoKind = 'stable' then
        url := MyRepo.GetDefaultURL(Opsi41, stable)
      else if repoKind = 'testing' then
        url := MyRepo.GetDefaultURL(Opsi41, testing);
    end
    else
    begin
      if repoKind = 'experimental' then
        url := MyRepo.GetDefaultURL(Opsi42, experimental)
      else if repoKind = 'stable' then
        url := MyRepo.GetDefaultURL(Opsi42, stable)
      else if repoKind = 'testing' then
        url := MyRepo.GetDefaultURL(Opsi42, testing);
    end;
    //writeln(url);
    // !following two lines need an existing LogDatei
    MyRepo.Add(url);
    InstallOpsiCommand := TRunCommandElevated.Create('', False);
    shellCommand := DistrInfo.GetPackageManagementShellCommand(distroName);
    // !following lines need an existing LogDatei
    Output := InstallOpsiCommand.Run(shellCommand + 'update');
    //writeln(Output);
    Output := InstallOpsiCommand.Run(shellCommand + 'install opsi-script');
    //writeln(Output);
    Output := InstallOpsiCommand.Run('opsi-script -batch ' + DirClientData +
      'setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log');

    // get result from result file and print it
    FileText.LoadFromFile(DirClientData + 'result.conf');
    // print result of installation
    writeln(FileText.Text);

    InstallOpsiCommand.Free;
    MyRepo.Free;
    FileText.Free;
  end;

  /////////////////////////////////////////////////////////////////////////////
  procedure TQuickInstall.NoGuiQuery;
  begin
    SetDefaultValues;

    // input variables set by resourcestrings for usage in the overview and
    // for easier query handling, e.g. if dhcp = rsYes instead of
    // if (dhcp = 'Yes') or (dhcp = 'Ja') or (dhcp = 'Oui') or ...

    // setup type:
    writeln(rsSetup, rsSetupOp);
    readln(input);
    while not ((input = rsStandard) or (input = rsCustom)) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    setupType := input;
    writeln('');
    writeln(rsCarryOut);
    writeln('');
    QueryDistribution;
  end;

  procedure TQuickInstall.QueryDistribution;
  begin
    // distribution:
    writeln(rsDistr, ' ', distroName, ' ', distroRelease);
    writeln(rsIsCorrect, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo)) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    // if distribution isn't correct, read the correct one
    if input = rsNo then
    begin
      writeln(rsOtherDistr);
      readln(input);
      distroName := Copy(input, 1, Pos(' ', input) - 1);
      distroRelease := Copy(input, Pos(' ', input) + 1, Length(input) - Pos(' ', input));
    end;
    DistrInfo.SetInfo(distroName, distroRelease);
    if DistrInfo.MyDistr = other then
    begin
      writeln(rsNoSupport + #10 + DistrInfo.Distribs);
      Exit;
    end;

    if setupType = rsCustom then
      // following queries only for custom setup
      QueryOpsiVersion
    else
      QueryDhcp;
  end;

  procedure TQuickInstall.QueryOpsiVersion;
  begin
    // opsi version:
    writeln(rsOpsiVersion, rsOpsiVersionOp);
    readln(input);
    while not ((input = 'Opsi 4.1') or (input = 'Opsi 4.2')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    opsiVersion := input;
    QueryRepo;
  end;

  procedure TQuickInstall.QueryRepo;
  begin
    // repo:
    if opsiVersion = 'Opsi 4.1' then
      writeln(rsRepo, ' [Example: ', baseUrlOpsi41, ']')
    else if opsiVersion = 'Opsi 4.2' then
      writeln(rsRepo, ' [Example: ', baseUrlOpsi42, ']');
    readln(input);
    while (Pos('http', input) <> 1) or (input = '') do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    repo := input;
    QueryProxy;
  end;

  procedure TQuickInstall.QueryProxy;
  begin
    // proxy:
    writeln(rsUseProxy, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo)) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = rsYes then
    begin
      writeln('Which Proxy would you like to use? [Example: "http://myproxy.dom.org:8080"]');
      readln(input);
      proxy := input;
    end;
    QueryRepoNoCache;
  end;

  procedure TQuickInstall.QueryRepoNoCache;
  begin
    // repo wihout cache proxy:
    if opsiVersion = 'Opsi 4.1' then
      writeln(rsRepoNoCache, ' [Example: ', baseUrlOpsi41, ']')
    else if opsiVersion = 'Opsi 4.2' then
      writeln(rsRepoNoCache, ' [Example: ', baseUrlOpsi42, ']');
    readln(input);
    while (Pos('http', input) <> 1) or (input = '') do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    repoNoCache := input;
    QueryBackend;
  end;

  procedure TQuickInstall.QueryBackend;
  begin
    // backend:
    writeln(rsBackend, rsBackendOp);
    readln(input);
    while not ((input = 'file') or (input = 'mysql')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    backend := input;

    if input = 'mysql' then
      QueryModules
    else
      QueryRepoKind;
  end;

  procedure TQuickInstall.QueryModules;
  begin
    // copy modules:
    writeln(rsCopyModules, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo)) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    copyMod := input;
    QueryRepoKind;
  end;

  procedure TQuickInstall.QueryRepoKind;
  begin
    // repo kind:
    writeln(rsRepoKind, rsRepoKindOp);
    readln(input);
    while not ((input = 'experimental') or (input = 'stable') or
        (input = 'testing')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    repoKind := input;

    if distroName = 'Univention' then
      QueryUCS
    else
      QueryReboot;
  end;

  procedure TQuickInstall.QueryUCS;
  begin
    // ucs password:
    writeln(rsUCS);
    readln(input);
    ucsPassword := input;
    QueryReboot;
  end;

  procedure TQuickInstall.QueryReboot;
  begin
    // reboot:
    writeln(rsReboot, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo)) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    reboot := input;
    QueryDhcp;
  end;

  procedure TQuickInstall.QueryDhcp;
  begin
    // dhcp:
    writeln(rsDhcp, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo)) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    dhcp := input;

    if dhcp = rsYes then
      // following queries only for dhcp
      QueryLink
    else
      QueryAdminName;
  end;

  procedure TQuickInstall.QueryLink;
  begin
    // link:
    writeln(rsTFTPROOT, rsLinkOp);
    readln(input);
    while not ((input = 'default.menu') or (input = 'default.nomenu')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    link := input;
    QueryNetmask;
  end;

  procedure TQuickInstall.QueryNetmask;
  begin
    // netmask:
    writeln(rsNetmask, rsNetmaskEx);
    readln(input);
    netmask := input;
    QueryNetworkAddress;
  end;

  procedure TQuickInstall.QueryNetworkAddress;
  begin
    // network address:
    writeln(rsNetworkAddress, rsNetworkAddressEx);
    readln(input);
    networkAddress := input;
    QueryDomain;
  end;

  procedure TQuickInstall.QueryDomain;
  begin
    // domain:
    writeln(rsDomain, rsDomainEx);
    readln(input);
    domain := input;
    QueryNameserver;
  end;

  procedure TQuickInstall.QueryNameserver;
  begin
    // nameserver:
    writeln(rsNameserver, rsNameserverEx);
    readln(input);
    nameserver := input;
    QueryGateway;
  end;

  procedure TQuickInstall.QueryGateway;
  begin
    // gateway:
    writeln(rsGateway, rsGatewayEx);
    readln(input);
    gateway := input;
    QueryAdminName;
  end;

  procedure TQuickInstall.QueryAdminName;
  begin
    // admin name:
    writeln(rsAdminName);
    readln(input);
    adminName := input;
    QueryAdminPass;
  end;

  procedure TQuickInstall.QueryAdminPass;
  begin
    // admin password:
    writeln(rsAdminPassword);
    readln(input);
    adminPassword := input;
    QueryIPName;
  end;

  procedure TQuickInstall.QueryIPName;
  begin
    // IP name:
    writeln(rsIPName);
    readln(input);
    ipName := input;
    QueryIPNumber;
  end;

  procedure TQuickInstall.QueryIPNumber;
  begin
    // IP number:
    writeln(rsIPNumber);
    readln(input);
    ipNumber := input;
    QueryOverview;
  end;

  procedure TQuickInstall.QueryOverview;
  begin
    // Overview
    writeln('');
    writeln(rsOverview);
    writeln(rsOpsiVersionO, opsiVersion);
    {Custom installation}
    if setupType = rsCustom then
    begin
      writeln(rsRepoO, repo);
      writeln(rsProxyO, proxy);
      writeln(rsRepoNoCacheO, repoNoCache);
      writeln(rsBackendO, backend);
      if backend = 'mysql' then
        writeln(rsCopyModulesO, copyMod);
      writeln(rsRepoKindO, repoKind);
      if distroName = 'Univention' then
        writeln(rsUCSO, ucsPassword);
      writeln(rsRebootO, reboot);
    end;
    {Both}
    writeln(rsDhcpO, dhcp);
    if dhcp = lowerCase(rsYes) then
    begin
      writeln(rsTFTPROOTO, link);
      writeln(rsNetmaskO, netmask);
      writeln(rsNetworkO, networkAddress);
      writeln(rsDomainO, domain);
      writeln(rsNameserverO, nameserver);
      writeln(rsGatewayO, gateway);
    end;
    writeln(rsAdminNameO, adminName);
    writeln(rsAdminPasswordO, adminPassword);
    writeln(rsIPNameO, ipName);
    writeln(rsIPNumberO, ipNumber);

    writeln('');
    writeln('To continue, please press enter...');
    readln();

    WritePropsToFile;
    InstallOpsi;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // install opsi server with the default values from SetDefaultValues
  procedure TQuickInstall.ExecuteWithDefaultValues;
  begin
    SetDefaultValues;
    WritePropsToFile;
    InstallOpsi;
  end;

  procedure TQuickInstall.ReadProps;
  var
    i: integer;
  begin
    // Read from file what is required
    for i := 0 to PropsFile.Count - 1 do
    begin
      // Read repo_kind
      if Pos('repo_kind', PropsFile[i]) = 1 then
      begin
        // IndexOf(...) is 0-based
        repoKind := Copy(PropsFile[i], PropsFile[i].IndexOf('=') +
          2, PropsFile[i].Length - PropsFile[i].IndexOf('=') + 1);
      end;
      // Read opsi version from repo url
      if Pos('opsi_online_repository', PropsFile[i]) = 1 then
      begin
        if Pos('4.1', PropsFile[i]) > 0 then
          opsiVersion := 'Opsi 4.1'
        else
          opsiVersion := 'Opsi 4.2';
      end;
    end;

    // Take text of PropsFile as text for properties.conf
    PropsFile.SaveToFile(DirClientData + 'properties.conf');

    InstallOpsi;
  end;

var
  QuickInstall: TQuickInstall;
  user, userID, customLanguage, Lang, DefLang: string;
  //r: TTranslateUnitResult;

begin
  // only execute QuickInstall if user is root:
  if (RunCommand('/bin/sh', ['-c', 'echo | id -nu'], user) and
    RunCommand('/bin/sh', ['-c', 'echo | id -u'], userID)) then
  begin
    Delete(user, user.Length, 1);
    Delete(userID, userID.Length, 1);
  end;
  // exit if user is not root
  if not ((user = 'root') and (userID = '0')) then
  begin
    writeln('Please execute Opsi Quick-Install as root!');
    exit;
  end;
  //writeln(user, userID);

  QuickInstall := TQuickInstall.Create(nil);
  // .../lazarus/common/oslog.pas
  // log file in /tmp/opsi_quickinstall.log
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile('opsi_quickinstall_nogui.log');

  //writeln(LowerCase((user = 'sudo').ToString(TUseBoolStrs.True)));

  // Get directory of l-opsi-server/CLIENT_DATA
  QuickInstall.DirClientData := ExtractFilePath(ParamStr(0));
  Delete(QuickInstall.DirClientData, Length(QuickInstall.DirClientData), 1);
  QuickInstall.DirClientData :=
    ExtractFilePath(QuickInstall.DirClientData) + 'l-opsi-server/CLIENT_DATA/';

  // do language selection here only for nogui installation
  if QuickInstall.HasOption('n', 'nogui') then
  begin
    // get default language (system language)
    GetLanguageIDs(Lang, DefLang);
    // use default language for resourcestrings
    // use po-files of gui version (because LCL does not seem to be able...
    // ...to use po-files from other directories)
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      '../gui/locale/opsi_quick_install_project.%s.po', Lang, DefLang);

    writeln('');
    writeln(rsWelcome);
    // language:
    writeln(rsSelLanguage, rsLangOp);
    readln(customLanguage);
    // check for right input
    while not ((customLanguage = 'de') or (customLanguage = 'en')) do
    begin
      writeln('"', customLanguage, '"', rsNotValid);
      readln(customLanguage);
    end;
    // use po-files of gui version (because LCL does not seem to be able...
    // ...to use po-files from other directories)
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      '../gui/locale/opsi_quick_install_project.' + customLanguage + '.po');
  end;

  with QuickInstall do
  begin
    // distribution info
    distroName := getLinuxDistroName;
    distroRelease := getLinuxDistroRelease;
    //writeln(QuickInstall.distroName, QuickInstall.distroRelease);
    DistrInfo := TDistributionInfo.Create;
    DistrInfo.SetInfo(distroName, distroRelease);
    // In the nogui query the checking of the distribution will be done later.
    if not HasOption('n', 'nogui') then
    begin
      if DistrInfo.MyDistr = other then
      begin
        writeln(rsNoSupport + #10 + DistrInfo.Distribs);
        Exit;
      end;
    end;
    Run;
    DistrInfo.Free;
    Free;
  end;

  LogDatei.Free;
end.
