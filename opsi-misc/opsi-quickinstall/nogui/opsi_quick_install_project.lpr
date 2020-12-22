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
    procedure QuerySetupType;
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
    LogDatei.log('Entered SetDefaultValues', 0);
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
    adminName := 'Alexandra';
    adminPassword := 'linux123';
    ipName := 'auto';
    ipNumber := 'auto';
  end;

  // write properties in l-opsi-server.conf and properties.conf file
  procedure TQuickInstall.WritePropsToFile;
  begin
    LogDatei.log('Entered WritePropsToFile', 0);
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
    LogDatei.log('Entered InstallOpsi', 0);
    writeln(rsInstall + opsiVersion + ':');

    if (opsiVersion = 'Opsi 4.2') and (repoKind <> 'experimental') then
    begin
      writeln('The branch "' + repoKind + '" is not available for Opsi 4.2');
      Exit;
    end;

    // Set text of result.conf to 'failed' first (for safety)
    FileText := TStringList.Create;
    FileText.Add('failed');
    FileText.SaveToFile(DirClientData + 'result.conf');

    writeln(rsCreateRepo);
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
    // !following lines need an existing LogDatei
    if distroName = 'openSUSE' then
    begin
      writeln('OpenSUSE: Add Repo');
      MyRepo.Add(url, 'Opsi-Quick-Install-Repository');
    end
    else
      MyRepo.Add(url);
    InstallOpsiCommand := TRunCommandElevated.Create('', False);

    shellCommand := DistrInfo.GetPackageManagementShellCommand(distroName);
    // !following lines need an existing LogDatei
    Output := InstallOpsiCommand.Run(shellCommand + 'update');
    //writeln(Output);

    writeln(rsInstall + 'opsi-script...');
    Output := InstallOpsiCommand.Run(shellCommand + 'install opsi-script');
    //writeln(Output);

    writeln(rsInstall + 'l-opsi-server... ' + rsSomeMin);
    // "opsi-script -batch" for installation with gui window, ...
    // ..."opsi-script-nogui -batch" for without
    Output := InstallOpsiCommand.Run('opsi-script-nogui -batch ' +
      DirClientData + 'setup.opsiscript /var/log/opsi-quick-install-l-opsi-server.log');

    // get result from result file and print it
    FileText.LoadFromFile(DirClientData + 'result.conf');
    // adjust quick-install ExitCode
    if FileText[0] = 'failed' then
      ExitCode := 1;
    // print result of installation
    writeln(FileText.Text);
    writeln(rsLog);
    writeln(LogOpsiServer);

    InstallOpsiCommand.Free;
    MyRepo.Free;
    FileText.Free;
  end;

  /////////////////////////////////////////////////////////////////////////////
  procedure TQuickInstall.NoGuiQuery;
  begin
    SetDefaultValues;

    // input variables not set by resourcestrings but by characters for no...
    // ...need of mouse.
    // distribution:
    writeln(rsDistr, ' ', distroName, ' ', distroRelease);
    writeln(rsIsCorrect, rsYesNoOp);
    readln(input);
    while not ((input = 'y') or (input = 'n')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    // if distribution isn't correct, read the correct one
    if input = 'n' then
    begin
      writeln(rsOtherDistr);
      readln(input);
      distroName := Copy(input, 1, Pos(' ', input) - 1);
      distroRelease := Copy(input, Pos(' ', input) + 1, Length(input) -
        Pos(' ', input));
    end;
    DistrInfo.SetInfo(distroName, distroRelease);
    if DistrInfo.MyDistr = other then
    begin
      writeln('');
      writeln(rsNoSupport + #10 + DistrInfo.Distribs);
      Exit;
    end;
    QuerySetupType;
  end;

  procedure TQuickInstall.QuerySetupType;
  begin
    // setup type:
    writeln(rsSetup, rsSetupOp);
    readln(input);
    while not ((input = 's') or (input = 'c') or (input = '-b') or
        (input = '')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    // if input = -b then go back to the previous query
    if input = '-b' then
      NoGuiQuery
    else
    begin
      setupType := input;
      if input = '' then
        setupType := 's';
      writeln('');
      writeln(rsCarryOut);
      writeln('');
      if setupType = 'c' then
        // following queries only for custom setup
        QueryOpsiVersion
      else
      if distroName = 'Univention' then
        QueryUCS
      else
        QueryDhcp;
    end;
  end;

  procedure TQuickInstall.QueryOpsiVersion;
  begin
    // opsi version:
    writeln(rsOpsiVersion, rsOpsiVersionOp);
    readln(input);
    while not ((input = '1') or (input = '2') or (input = '-b') or
        (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoOpsiVersion)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QuerySetupType
    else
    begin
      opsiVersion := 'Opsi 4.' + input;
      if input = '' then
        opsiVersion := 'Opsi 4.1';
      QueryRepo;
    end;
  end;

  procedure TQuickInstall.QueryRepo;
  begin
    // repo:
    if opsiVersion = 'Opsi 4.1' then
      writeln(rsRepo, ' [Example: ', baseUrlOpsi41, ']')
    else if opsiVersion = 'Opsi 4.2' then
      writeln(rsRepo, ' [Example: ', baseUrlOpsi42, ']');
    readln(input);
    while ((Pos('http', input) <> 1) and (input <> '-b') and (input <> '')) do
    begin
      if input = '-h' then
        writeln(rsInfoRepo)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryOpsiVersion
    else
    begin
      repo := input;
      if (input = '') and (opsiVersion = 'Opsi 4.1') then
        repo := baseUrlOpsi41
      else
      if (input = '') and (opsiVersion = 'Opsi 4.2') then
        repo := baseUrlOpsi42;
      QueryProxy;
    end;
  end;

  procedure TQuickInstall.QueryProxy;
  begin
    // proxy:
    writeln(rsUseProxy, rsYesNoOp);
    readln(input);
    while not ((input = 'y') or (input = 'n') or (input = '-b') or (input = '')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryRepo
    else
    begin
      if input = 'y' then
      begin
        writeln('Which Proxy would you like to use? [Example: "http://myproxy.dom.org:8080"]');
        readln(input);
        proxy := input;
      end
      else
        proxy := '';
      QueryRepoNoCache;
    end;
  end;

  procedure TQuickInstall.QueryRepoNoCache;
  begin
    // repo without cache proxy:
    if opsiVersion = 'Opsi 4.1' then
      writeln(rsRepoNoCache, ' [Example: ', baseUrlOpsi41, ']')
    else if opsiVersion = 'Opsi 4.2' then
      writeln(rsRepoNoCache, ' [Example: ', baseUrlOpsi42, ']');
    readln(input);
    while ((Pos('http', input) <> 1) and (input <> '-b') and (input <> '')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryProxy
    else
    begin
      repoNoCache := input;
      if input = '' then
        repoNoCache := repo;
      QueryBackend;
    end;
  end;

  procedure TQuickInstall.QueryBackend;
  begin
    // backend:
    writeln(rsBackend, rsBackendOp);
    readln(input);
    while not ((input = 'f') or (input = 'm') or (input = '-b') or
        (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoBackend)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryRepoNoCache
    else
    begin
      if input = 'm' then
        backend := 'mysql'
      else
        backend := 'file'; // cases input = 'f', input = ''
      if backend = 'mysql' then
        QueryModules
      else
        QueryRepoKind;
    end;
  end;

  procedure TQuickInstall.QueryModules;
  begin
    // copy modules:
    writeln(rsCopyModules, rsYesNoOp);
    readln(input);
    while not ((input = 'y') or (input = 'n') or (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoModules)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryBackend
    else
    begin
      if input = 'y' then
        copyMod := rsYes
      else
        copyMod := rsNo; // cases input = 'n', input = ''
      QueryRepoKind;
    end;
  end;

  procedure TQuickInstall.QueryRepoKind;
  begin
    // repo kind:
    writeln(rsRepoKind, rsRepoKindOp);
    readln(input);
    while not ((input = 'e') or (input = 's') or (input = 't') or
        (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoRepoKind)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
    begin
      if backend = 'mysql' then
        QueryModules
      else
        QueryBackend;
    end
    else
    begin
      if input = 'e' then
        repoKind := 'experimental'
      else if input = 't' then
        repoKind := 'testing'
      else
        repoKind := 'stable'; // cases input = 's', input = ''
      if distroName = 'Univention' then
        QueryUCS
      else
        QueryReboot;
    end;
  end;

  procedure TQuickInstall.QueryUCS;
  begin
    // ucs password:
    writeln(rsUCS);
    readln(input);
    if input = '-b' then // go back
    begin
      if setupType = 's' then
        QuerySetupType
      else
        QueryRepoKind;
    end
    else // go forward
    begin
      ucsPassword := input;
      if setupType = 'c' then
        QueryReboot
      else
        QueryDhcp;
    end;
  end;

  procedure TQuickInstall.QueryReboot;
  begin
    // reboot:
    writeln(rsReboot, rsYesNoOp);
    readln(input);
    while not ((input = 'y') or (input = 'n') or (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoReboot)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
    begin
      if distroName = 'Univention' then
        QueryUCS
      else
        QueryRepoKind;
    end
    else
    begin
      if input = 'y' then
        reboot := rsYes
      else
        reboot := rsNo; // cases input = 'n', input = ''
      QueryDhcp;
    end;
  end;

  procedure TQuickInstall.QueryDhcp;
  begin
    // dhcp:
    writeln(rsDhcp, rsYesNoOp);
    readln(input);
    while not ((input = 'y') or (input = 'n') or (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoDhcp)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
    begin
      if setupType = 'c' then
        QueryReboot
      else
      begin
        if distroName = 'Univention' then
          QueryUCS
        else
          QuerySetupType;
      end;
    end
    else
    begin
      if input = 'y' then
        dhcp := rsYes
      else
        dhcp := rsNo; // cases input = 'n', input = ''
      if dhcp = rsYes then
        // following queries only for dhcp
        QueryLink
      else
        QueryAdminName;
    end;
  end;

  procedure TQuickInstall.QueryLink;
  begin
    // link:
    writeln(rsTFTPROOT, rsLinkOp);
    readln(input);
    while not ((input = 'm') or (input = 'nom') or (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoTFTPROOT)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryDhcp
    else
    begin
      if input = 'm' then
        link := 'default.menu'
      else
        link := 'default.nomenu'; // cases input = 'nom', input = ''
      QueryNetmask;
    end;
  end;

  procedure TQuickInstall.QueryNetmask;
  begin
    // netmask:
    writeln(rsNetmask, rsNetmaskEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryLink
    else
    begin
      netmask := input;
      QueryNetworkAddress;
    end;
  end;

  procedure TQuickInstall.QueryNetworkAddress;
  begin
    // network address:
    writeln(rsNetworkAddress, rsNetworkAddressEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryNetmask
    else
    begin
      networkAddress := input;
      QueryDomain;
    end;
  end;

  procedure TQuickInstall.QueryDomain;
  begin
    // domain:
    writeln(rsDomain, rsDomainEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryNetworkAddress
    else
    begin
      domain := input;
      QueryNameserver;
    end;
  end;

  procedure TQuickInstall.QueryNameserver;
  begin
    // nameserver:
    writeln(rsNameserver, rsNameserverEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryDomain
    else
    begin
      nameserver := input;
      QueryGateway;
    end;
  end;

  procedure TQuickInstall.QueryGateway;
  begin
    // gateway:
    writeln(rsGateway, rsGatewayEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryNameserver
    else
    begin
      gateway := input;
      QueryAdminName;
    end;
  end;

  procedure TQuickInstall.QueryAdminName;
  begin
    // admin name:
    writeln(rsAdminName);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoAdmin);
      readln(input);
    end;
    if input = '-b' then
    begin
      if dhcp = rsYes then
        QueryGateway
      else
        QueryDhcp;
    end
    else
    begin
      adminName := input;
      if input = '' then
        QueryIPName
      else
        QueryAdminPass;
    end;
  end;

  procedure TQuickInstall.QueryAdminPass;
  begin
    // admin password:
    writeln(rsAdminPassword);
    readln(input);
    if input = '-b' then
      QueryAdminName
    else
    begin
      adminPassword := input;
      QueryIPName;
    end;
  end;

  procedure TQuickInstall.QueryIPName;
  begin
    // IP name:
    writeln(rsIPName);
    readln(input);
    if input = '-b' then
    begin
      if adminName = '' then
        QueryAdminName
      else
        QueryAdminPass;
    end
    else
    begin
      ipName := input;
      QueryIPNumber;
    end;
  end;

  procedure TQuickInstall.QueryIPNumber;
  begin
    // IP number:
    writeln(rsIPNumber);
    readln(input);
    if input = '-b' then
      QueryIPName
    else
    begin
      ipNumber := input;
      QueryOverview;
    end;
  end;

  procedure TQuickInstall.QueryOverview;
  var
    // for getting the number of asked questions
    Counter: integer;
    // for getting a list of the asked questions
    queries: TStringList;
    validInput, isInputInt: boolean;
  begin
    Counter := 1;
    queries := TStringList.Create;
    validInput := False;
    isInputInt := False;

    // Overview
    writeln('');
    writeln(rsOverview);
    if setupType = 's' then
      writeln(rsOpsiVersionO, opsiVersion)
    else
    begin
      writeln(Counter, ' ', rsOpsiVersionO, opsiVersion);
      queries.Add('1');
      Inc(Counter);
    end;
    {Custom installation}
    if setupType = 'c' then
    begin
      writeln(Counter, ' ', rsRepoO, repo);
      queries.Add('2');
      Inc(Counter);
      writeln(Counter, ' ', rsProxyO, proxy);
      queries.Add('3');
      Inc(Counter);
      writeln(Counter, ' ', rsRepoNoCacheO, repoNoCache);
      queries.Add('4');
      Inc(Counter);
      writeln(Counter, ' ', rsBackendO, backend);
      queries.Add('5');
      Inc(Counter);
      if backend = 'mysql' then
      begin
        writeln(Counter, ' ', rsCopyModulesO, copyMod);
        queries.Add('6');
        Inc(Counter);
      end;
      writeln(Counter, ' ', rsRepoKindO, repoKind);
      queries.Add('7');
      Inc(Counter);
    end;
    {Both}
    if distroName = 'Univention' then
    begin
      writeln(Counter, ' ', rsUCSO, ucsPassword);
      queries.Add('8');
      Inc(Counter);
    end;
    {Custom installation}
    if setupType = 'c' then
    begin
      writeln(Counter, ' ', rsRebootO, reboot);
      queries.Add('9');
      Inc(Counter);
    end;
    {Both}
    writeln(Counter, ' ', rsDhcpO, dhcp);
    queries.Add('10');
    Inc(Counter);
    if dhcp = rsYes then
    begin
      writeln(Counter, ' ', rsTFTPROOTO, link);
      queries.Add('11');
      Inc(Counter);
      writeln(Counter, ' ', rsNetmaskO, netmask);
      queries.Add('12');
      Inc(Counter);
      writeln(Counter, ' ', rsNetworkO, networkAddress);
      queries.Add('13');
      Inc(Counter);
      writeln(Counter, ' ', rsDomainO, domain);
      queries.Add('14');
      Inc(Counter);
      writeln(Counter, ' ', rsNameserverO, nameserver);
      queries.Add('15');
      Inc(Counter);
      writeln(Counter, ' ', rsGatewayO, gateway);
      queries.Add('16');
      Inc(Counter);
    end;
    writeln(Counter, ' ', rsAdminNameO, adminName);
    queries.Add('17');
    Inc(Counter);
    if adminName <> '' then
    begin
      writeln(Counter, ' ', rsAdminPasswordO, adminPassword);
      queries.Add('18');
      Inc(Counter);
    end;
    writeln(Counter, ' ', rsIPNameO, ipName);
    queries.Add('19');
    Inc(Counter);
    writeln(Counter, ' ', rsIPNumberO, ipNumber);
    queries.Add('20');

    {writeln('');
    writeln(queries.Text);}

    writeln('');
    writeln(rsContinue);
    // Jumping back to a query by the number in the overview:
    while validInput = False do
    begin
      validInput := True;
      readln(input);
      try
        Counter := input.ToInteger - 1;
        isInputInt := True;
      except
      end;
      if input = '' then
      begin
        WritePropsToFile;
        InstallOpsi;
      end
      else
      if isInputInt = True then
      begin
        if Counter in [0..queries.Count - 1] then
        begin
          if queries[Counter] = '1' then
            QueryOpsiVersion
          else
          if queries[Counter] = '2' then
            QueryRepo
          else
          if queries[Counter] = '3' then
            QueryProxy
          else
          if queries[Counter] = '4' then
            QueryRepoNoCache
          else
          if queries[Counter] = '5' then
            QueryBackend
          else
          if queries[Counter] = '6' then
            QueryModules
          else
          if queries[Counter] = '7' then
            QueryRepoKind
          else
          if queries[Counter] = '8' then
            QueryUCS
          else
          if queries[Counter] = '9' then
            QueryReboot
          else
          if queries[Counter] = '10' then
            QueryDhcp
          else
          if queries[Counter] = '11' then
            QueryLink
          else
          if queries[Counter] = '12' then
            QueryNetmask
          else
          if queries[Counter] = '13' then
            QueryNetworkAddress
          else
          if queries[Counter] = '14' then
            QueryDomain
          else
          if queries[Counter] = '15' then
            QueryNameserver
          else
          if queries[Counter] = '16' then
            QueryGateway
          else
          if queries[Counter] = '17' then
            QueryAdminName
          else
          if queries[Counter] = '18' then
            QueryAdminPass
          else
          if queries[Counter] = '19' then
            QueryIPName
          else
          if queries[Counter] = '20' then
            QueryIPNumber;
        end
        else
          // If input is integer but not a valid one:
        begin
          writeln('"', input, '"', rsNotValid);
          validInput := False;
        end;
      end
      else
        // If input is no integer and not '':
      begin
        writeln('"', input, '"', rsNotValid);
        validInput := False;
      end;
    end;
{procedure TQuickInstall.NoGuiQuery;
  begin
    SetDefaultValues;
    // input variables set by resourcestrings for usage in the overview and
    // for easier query handling, e.g. if dhcp = rsYes instead of
    // if (dhcp = 'Yes') or (dhcp = 'Ja') or (dhcp = 'Oui') or ...
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
      distroRelease := Copy(input, Pos(' ', input) + 1, Length(input) -
        Pos(' ', input));
    end;
    DistrInfo.SetInfo(distroName, distroRelease);
    if DistrInfo.MyDistr = other then
    begin
      writeln('');
      writeln(rsNoSupport + #10 + DistrInfo.Distribs);
      Exit;
    end;
    QuerySetupType;
  end;

  procedure TQuickInstall.QuerySetupType;
  begin
    // setup type:
    writeln(rsSetup, rsSetupOp);
    readln(input);
    while not ((input = rsStandard) or (input = rsCustom) or (input = '-b') or
        (input = '')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    // if input = -b then go back to the previous query
    if input = '-b' then
      NoGuiQuery
    else
    begin
      setupType := input;
      if input = '' then
        setupType := rsStandard;
      writeln('');
      writeln(rsCarryOut);
      writeln('');
      if setupType = rsCustom then
        // following queries only for custom setup
        QueryOpsiVersion
      else
      if distroName = 'Univention' then
        QueryUCS
      else
        QueryDhcp;
    end;
  end;

  procedure TQuickInstall.QueryOpsiVersion;
  begin
    // opsi version:
    writeln(rsOpsiVersion, rsOpsiVersionOp);
    readln(input);
    while not ((input = 'Opsi 4.1') or (input = 'Opsi 4.2') or
        (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoOpsiVersion)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QuerySetupType
    else
    begin
      opsiVersion := input;
      if input = '' then
        opsiVersion := 'Opsi 4.1';
      QueryRepo;
    end;
  end;

  procedure TQuickInstall.QueryRepo;
  begin
    // repo:
    if opsiVersion = 'Opsi 4.1' then
      writeln(rsRepo, ' [Example: ', baseUrlOpsi41, ']')
    else if opsiVersion = 'Opsi 4.2' then
      writeln(rsRepo, ' [Example: ', baseUrlOpsi42, ']');
    readln(input);
    while ((Pos('http', input) <> 1) and (input <> '-b') and (input <> '')) do
    begin
      if input = '-h' then
        writeln(rsInfoRepo)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryOpsiVersion
    else
    begin
      repo := input;
      if (input = '') and (opsiVersion = 'Opsi 4.1') then
        repo := baseUrlOpsi41
      else
      if (input = '') and (opsiVersion = 'Opsi 4.2') then
        repo := baseUrlOpsi42;
      QueryProxy;
    end;
  end;

  procedure TQuickInstall.QueryProxy;
  begin
    // proxy:
    writeln(rsUseProxy, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo) or (input = '-b') or (input = '')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryRepo
    else
    begin
      if input = rsYes then
      begin
        writeln('Which Proxy would you like to use? [Example: "http://myproxy.dom.org:8080"]');
        readln(input);
        proxy := input;
      end
      else
        proxy := '';
      QueryRepoNoCache;
    end;
  end;

  procedure TQuickInstall.QueryRepoNoCache;
  begin
    // repo without cache proxy:
    if opsiVersion = 'Opsi 4.1' then
      writeln(rsRepoNoCache, ' [Example: ', baseUrlOpsi41, ']')
    else if opsiVersion = 'Opsi 4.2' then
      writeln(rsRepoNoCache, ' [Example: ', baseUrlOpsi42, ']');
    readln(input);
    while ((Pos('http', input) <> 1) and (input <> '-b') and (input <> '')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryProxy
    else
    begin
      repoNoCache := input;
      if input = '' then
        repoNoCache := repo;
      QueryBackend;
    end;
  end;

  procedure TQuickInstall.QueryBackend;
  begin
    // backend:
    writeln(rsBackend, rsBackendOp);
    readln(input);
    while not ((input = 'file') or (input = 'mysql') or (input = '-b') or
        (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoBackend)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryRepoNoCache
    else
    begin
      backend := input;
      if input = '' then
        backend := 'file';
      if input = 'mysql' then
        QueryModules
      else
        QueryRepoKind;
    end;
  end;

  procedure TQuickInstall.QueryModules;
  begin
    // copy modules:
    writeln(rsCopyModules, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo) or (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoModules)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryBackend
    else
    begin
      copyMod := input;
      if input = '' then
        copyMod := rsNo;
      QueryRepoKind;
    end;
  end;

  procedure TQuickInstall.QueryRepoKind;
  begin
    // repo kind:
    writeln(rsRepoKind, rsRepoKindOp);
    readln(input);
    while not ((input = 'experimental') or (input = 'stable') or
        (input = 'testing') or (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoRepoKind)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
    begin
      if input = 'mysql' then
        QueryModules
      else
        QueryBackend;
    end
    else
    begin
      repoKind := input;
      if input = '' then
        repoKind := 'stable';
      if distroName = 'Univention' then
        QueryUCS
      else
        QueryReboot;
    end;
  end;

  procedure TQuickInstall.QueryUCS;
  begin
    // ucs password:
    writeln(rsUCS);
    readln(input);
    if input = '-b' then
    begin
      if setupType = rsStandard then
        QuerySetupType
      else
        QueryRepoKind;
    end
    else
    begin
      ucsPassword := input;
      if setupType = rsCustom then
        QueryReboot
      else
        QueryDhcp;
    end;
  end;

  procedure TQuickInstall.QueryReboot;
  begin
    // reboot:
    writeln(rsReboot, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo) or (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoReboot)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
    begin
      if distroName = 'Univention' then
        QueryUCS
      else
        QueryRepoKind;
    end
    else
    begin
      reboot := input;
      if input = '' then
        reboot := rsNo;
      QueryDhcp;
    end;
  end;

  procedure TQuickInstall.QueryDhcp;
  begin
    // dhcp:
    writeln(rsDhcp, rsYesNoOp);
    readln(input);
    while not ((input = rsYes) or (input = rsNo) or (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoDhcp)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
    begin
      if setupType = rsCustom then
        QueryReboot
      else
      begin
        if distroName = 'Univention' then
          QueryUCS
        else
          QuerySetupType;
      end;
    end
    else
    begin
      dhcp := input;
      if input = '' then
        dhcp := rsNo;
      if dhcp = rsYes then
        // following queries only for dhcp
        QueryLink
      else
        QueryAdminName;
    end;
  end;

  procedure TQuickInstall.QueryLink;
  begin
    // link:
    writeln(rsTFTPROOT, rsLinkOp);
    readln(input);
    while not ((input = 'default.menu') or (input = 'default.nomenu') or
        (input = '-b') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoTFTPROOT)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    if input = '-b' then
      QueryDhcp
    else
    begin
      link := input;
      if input = '' then
        link := 'default.nomenu';
      QueryNetmask;
    end;
  end;

  procedure TQuickInstall.QueryNetmask;
  begin
    // netmask:
    writeln(rsNetmask, rsNetmaskEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryLink
    else
    begin
      netmask := input;
      QueryNetworkAddress;
    end;
  end;

  procedure TQuickInstall.QueryNetworkAddress;
  begin
    // network address:
    writeln(rsNetworkAddress, rsNetworkAddressEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryNetmask
    else
    begin
      networkAddress := input;
      QueryDomain;
    end;
  end;

  procedure TQuickInstall.QueryDomain;
  begin
    // domain:
    writeln(rsDomain, rsDomainEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryNetworkAddress
    else
    begin
      domain := input;
      QueryNameserver;
    end;
  end;

  procedure TQuickInstall.QueryNameserver;
  begin
    // nameserver:
    writeln(rsNameserver, rsNameserverEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryDomain
    else
    begin
      nameserver := input;
      QueryGateway;
    end;
  end;

  procedure TQuickInstall.QueryGateway;
  begin
    // gateway:
    writeln(rsGateway, rsGatewayEx);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;
    if input = '-b' then
      QueryNameserver
    else
    begin
      gateway := input;
      QueryAdminName;
    end;
  end;

  procedure TQuickInstall.QueryAdminName;
  begin
    // admin name:
    writeln(rsAdminName);
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoAdmin);
      readln(input);
    end;
    if input = '-b' then
    begin
      if dhcp = rsYes then
        QueryGateway
      else
        QueryDhcp;
    end
    else
    begin
      adminName := input;
      if input = '' then
        QueryIPName
      else
        QueryAdminPass;
    end;
  end;

  procedure TQuickInstall.QueryAdminPass;
  begin
    // admin password:
    writeln(rsAdminPassword);
    readln(input);
    if input = '-b' then
      QueryAdminName
    else
    begin
      adminPassword := input;
      QueryIPName;
    end;
  end;

  procedure TQuickInstall.QueryIPName;
  begin
    // IP name:
    writeln(rsIPName);
    readln(input);
    if input = '-b' then
    begin
      if adminName = '' then
        QueryAdminName
      else
        QueryAdminPass;
    end
    else
    begin
      ipName := input;
      QueryIPNumber;
    end;
  end;

  procedure TQuickInstall.QueryIPNumber;
  begin
    // IP number:
    writeln(rsIPNumber);
    readln(input);
    if input = '-b' then
      QueryIPName
    else
    begin
      ipNumber := input;
      QueryOverview;
    end;
  end;

  procedure TQuickInstall.QueryOverview;
  var
    Counter: integer;
    queries: TStringList;
    validInput, isInputInt: boolean;
  begin
    Counter := 1;
    queries := TStringList.Create;
    validInput := False;
    isInputInt := False;

    // Overview
    writeln('');
    writeln(rsOverview);
    if setupType = rsStandard then
      writeln(rsOpsiVersionO, opsiVersion)
    else
    begin
      writeln(Counter, ' ', rsOpsiVersionO, opsiVersion);
      queries.Add('1');
      Inc(Counter);
    end;
    {Custom installation}
    if setupType = rsCustom then
    begin
      writeln(Counter, ' ', rsRepoO, repo);
      queries.Add('2');
      Inc(Counter);
      writeln(Counter, ' ', rsProxyO, proxy);
      queries.Add('3');
      Inc(Counter);
      writeln(Counter, ' ', rsRepoNoCacheO, repoNoCache);
      queries.Add('4');
      Inc(Counter);
      writeln(Counter, ' ', rsBackendO, backend);
      queries.Add('5');
      Inc(Counter);
      if backend = 'mysql' then
      begin
        writeln(Counter, ' ', rsCopyModulesO, copyMod);
        queries.Add('6');
        Inc(Counter);
      end;
      writeln(Counter, ' ', rsRepoKindO, repoKind);
      queries.Add('7');
      Inc(Counter);
    end;
    {Both}
    if distroName = 'Univention' then
    begin
      writeln(Counter, ' ', rsUCSO, ucsPassword);
      queries.Add('8');
      Inc(Counter);
    end;
    {Custom installation}
    if setupType = rsCustom then
    begin
      writeln(Counter, ' ', rsRebootO, reboot);
      queries.Add('9');
      Inc(Counter);
    end;
    {Both}
    writeln(Counter, ' ', rsDhcpO, dhcp);
    queries.Add('10');
    Inc(Counter);
    if dhcp = rsYes then
    begin
      writeln(Counter, ' ', rsTFTPROOTO, link);
      queries.Add('11');
      Inc(Counter);
      writeln(Counter, ' ', rsNetmaskO, netmask);
      queries.Add('12');
      Inc(Counter);
      writeln(Counter, ' ', rsNetworkO, networkAddress);
      queries.Add('13');
      Inc(Counter);
      writeln(Counter, ' ', rsDomainO, domain);
      queries.Add('14');
      Inc(Counter);
      writeln(Counter, ' ', rsNameserverO, nameserver);
      queries.Add('15');
      Inc(Counter);
      writeln(Counter, ' ', rsGatewayO, gateway);
      queries.Add('16');
      Inc(Counter);
    end;
    writeln(Counter, ' ', rsAdminNameO, adminName);
    queries.Add('17');
    Inc(Counter);
    if adminName <> '' then
    begin
      writeln(Counter, ' ', rsAdminPasswordO, adminPassword);
      queries.Add('18');
      Inc(Counter);
    end;
    writeln(Counter, ' ', rsIPNameO, ipName);
    queries.Add('19');
    Inc(Counter);
    writeln(Counter, ' ', rsIPNumberO, ipNumber);
    queries.Add('20');

    {writeln('');
    writeln(queries.Text);}

    writeln('');
    writeln('To continue, please press enter.');
    // Jumping back to a query by the number in the overview:
    while validInput = False do
    begin
      validInput := True;
      readln(input);
      try
        Counter := input.ToInteger - 1;
        isInputInt := True;
      except
      end;
      if input = '' then
      begin
        WritePropsToFile;
        InstallOpsi;
      end
      else
      if isInputInt = True then
      begin
        if Counter in [0..queries.Count - 1] then
        begin
          if queries[Counter] = '1' then
            QueryOpsiVersion
          else
          if queries[Counter] = '2' then
            QueryRepo
          else
          if queries[Counter] = '3' then
            QueryProxy
          else
          if queries[Counter] = '4' then
            QueryRepoNoCache
          else
          if queries[Counter] = '5' then
            QueryBackend
          else
          if queries[Counter] = '6' then
            QueryModules
          else
          if queries[Counter] = '7' then
            QueryRepoKind
          else
          if queries[Counter] = '8' then
            QueryUCS
          else
          if queries[Counter] = '9' then
            QueryReboot
          else
          if queries[Counter] = '10' then
            QueryDhcp
          else
          if queries[Counter] = '11' then
            QueryLink
          else
          if queries[Counter] = '12' then
            QueryNetmask
          else
          if queries[Counter] = '13' then
            QueryNetworkAddress
          else
          if queries[Counter] = '14' then
            QueryDomain
          else
          if queries[Counter] = '15' then
            QueryNameserver
          else
          if queries[Counter] = '16' then
            QueryGateway
          else
          if queries[Counter] = '17' then
            QueryAdminName
          else
          if queries[Counter] = '18' then
            QueryAdminPass
          else
          if queries[Counter] = '19' then
            QueryIPName
          else
          if queries[Counter] = '20' then
            QueryIPNumber;
        end
        else
          // If input is integer but not a valid one:
        begin
          writeln('"', input, '"', rsNotValid);
          validInput := False;
        end;
      end
      else
        // If input is no integer and not '':
      begin
        writeln('"', input, '"', rsNotValid);
        validInput := False;
      end;
    end;
  end;}
  end;

  //////////////////////////////////////////////////////////////////////////////
  // install opsi server with the default values from SetDefaultValues
  procedure TQuickInstall.ExecuteWithDefaultValues;
  begin
    LogDatei.log('Entered ExecuteWithDefaultValues', 0);
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
const
  logFileName = 'opsi_quickinstall_nogui.log';

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
  LogDatei.CreateTheLogfile(logFileName);
  LogDatei.log('Log file created', 0);

  //writeln(LowerCase((user = 'sudo').ToString(TUseBoolStrs.True)));

  // Get directory of l-opsi-server/CLIENT_DATA
  QuickInstall.DirClientData := ExtractFilePath(ParamStr(0));
  Delete(QuickInstall.DirClientData, Length(QuickInstall.DirClientData), 1);
  QuickInstall.DirClientData :=
    ExtractFilePath(QuickInstall.DirClientData) + 'l-opsi-server/CLIENT_DATA/';

  // get default language (system language)
  GetLanguageIDs(Lang, DefLang);
  // use default language for resourcestrings
  // use po-files of gui version (because LCL does not seem to be able...
  // ...to use po-files from other directories)
  TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
    '../gui/locale/opsi_quick_install_project.%s.po', Lang, DefLang);

  // do language selection here only for nogui installation
  if QuickInstall.HasOption('n', 'nogui') then
  begin
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
    writeln(QuickInstall.distroName, ' ', QuickInstall.distroRelease);
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

  writeln(LogDatei.StandardMainLogPath + logFileName);
  LogDatei.Free;
end.
