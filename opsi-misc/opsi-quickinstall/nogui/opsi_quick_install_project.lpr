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
  oslog,
  osnetworkcalculator,
  get_latest_lopsiserver,
  opsi_quick_install_resourcestrings;

type

  { TQuickInstall }

  TQuickInstall = class(TCustomApplication)
  private
  var
    input, setupType, distroName, distroRelease: string;
    DistrInfo: TDistributionInfo;
    opsiVersion, repo, proxy, repoNoCache: string;
    backend, copyMod, repoKind: string;
    ucsPassword, reboot, dhcp, symlink: string;
    NetworkDetails: array of string;
    netmask, networkAddress, domain, nameserver, gateway: string;
    adminName, adminPassword, ipName, ipNumber: string;
    FileText, PropsFile: TStringList;
    MyRepo: TLinuxRepository;
    QuickInstallCommand: TRunCommandElevated;
    DirClientData, url, shellCommand, Output: string;
  const
    baseUrlOpsi41 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    baseUrlOpsi42 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
    // set default values for all required variables
    procedure SetDefaultValues;
    // write properties in properties.conf file
    procedure WritePropsToFile;
    // install opsi-script, l-opsi-server
    procedure InstallOpsi;
    // query:
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
    // no query, directly use all default values for installation
    procedure ExecuteWithDefaultValues;
    // no query, read in values from a file
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
    ErrorMsg: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hgndf:', 'help gui nogui default file:');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;
    // parse parameters:
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;
    // out of use
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
    // query
    if HasOption('n', 'nogui') then
    begin
      NoGuiQuery;
      Terminate;
      Exit;
    end;
    // no query, directly use all default values for installation
    if HasOption('d', 'default') then
    begin
      ExecuteWithDefaultValues;
      Terminate;
      Exit;
    end;
    // no query, read in values from a file
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
    writeln('-d [-default]     : Use the default values for the opsi-server installation and immmediately start the installation.');
    writeln('-f [-file] <file> : Use the values from a file for the opsi-server installation and immmediately start the installation.');
    writeln('-h [-help]        : See this information.');
    writeln('-n [-nogui]       : Start a setup program, in which you can set the values for the installation seperately');
  end;

  // set default values for all variables that are required for the installation
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
    symlink := 'default.nomenu';
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

  // write properties in properties.conf file
  procedure TQuickInstall.WritePropsToFile;
  begin
    LogDatei.log('Entered WritePropsToFile', 0);
    // write file text
    FileText := TStringList.Create;
    QuickInstallCommand := TRunCommandElevated.Create('', False);

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
    FileText.Add('patch_default_link_for_bootimage=' + symlink);
    FileText.Add('proxy=' + proxy);
    FileText.Add('repo_kind=' + repoKind);
    FileText.Add('ucs_master_admin_password=' + ucsPassword);
    // update_test shall always be false
    FileText.Add('update_test=false');

    DirClientData := ExtractFilePath(ParamStr(0));
    Delete(DirClientData, Length(DirClientData), 1);
    DirClientData := ExtractFilePath(DirClientData) + 'l-opsi-server';
    // try downloading latest l-opsi-server and use respective DirClientData
    writeln(rsWait);
    if getLOpsiServer(QuickInstallCommand, distroName) then
    begin
      LogDatei.log('Latest l-opsi-server successfully downloaded', LLInfo);
      DirClientData += '_downloaded/CLIENT_DATA/';
    end
    else
    begin
      LogDatei.log('Downloading latest l-opsi-server failed. Using default l-opsi-server:',
        LLnotice);
      DirClientData += '/CLIENT_DATA/';
    end;

    // write in l-opsi-server.conf file(for tests):
    if not FileExists('l-opsi-server.conf') then
      QuickInstallCommand.Run('touch l-opsi-server.conf', Output);
    FileText.SaveToFile('l-opsi-server.conf');
    // write in properties.conf file:
    if not FileExists(DirClientData + 'properties.conf') then
      QuickInstallCommand.Run('touch ' + DirClientData + 'properties.conf', Output);
    QuickInstallCommand.Run('chown -c $USER ' + DirClientData +
      'properties.conf', Output);
    FileText.SaveToFile(DirClientData + 'properties.conf');

    FileText.Free;
  end;

  // install opsi-server
  // requires: opsiVersion, repoKind, distroName, DistrInfo, existing LogDatei
  procedure TQuickInstall.InstallOpsi;
  begin
    LogDatei.log('Entered InstallOpsi', 0);
    writeln(rsInstall + opsiVersion + ':');

    // for me as warning
    if (opsiVersion = 'Opsi 4.2') and (repoKind = 'stable') then
    begin
      writeln('Opsi 4.2 only works on the branches "experimental" and "testing" so far.');
      Exit;
    end;

    // Set text of result.conf to 'failed' first (for safety)
    FileText := TStringList.Create;
    FileText.Add('failed');
    if not FileExists(DirClientData + 'result.conf') then
      QuickInstallCommand.Run('touch ' + DirClientData + 'result.conf', Output);
    QuickInstallCommand.Run('chown -c $USER ' + DirClientData + 'result.conf', Output);
    FileText.SaveToFile(DirClientData + 'result.conf');

    writeln(rsCreateRepo);
    // first remove opsi.list to have a cleared opsi repository list
    if FileExists('/etc/apt/sources.list.d/opsi.list') then
      QuickInstallCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);
    // create repository (no password, user is root):
    MyRepo := TLinuxRepository.Create(DistrInfo.MyDistr, '', False);
    // set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
    if opsiVersion = 'Opsi 4.1' then
      url := MyRepo.GetDefaultURL(Opsi41, stringToOpsiBranch(repoKind))
    else
      url := MyRepo.GetDefaultURL(Opsi42, stringToOpsiBranch(repoKind));

    // !following lines need an existing LogDatei
    if (distroName = 'openSUSE') or (distroName = 'SUSE') then
    begin
      writeln('OpenSUSE/SUSE: Add Repo');
      MyRepo.Add(url, 'OpsiQuickInstallRepositoryNew');
    end
    else
      MyRepo.Add(url);

    // install opsi:
    shellCommand := GetPackageManagementShellCommand(distroName);
    // !following lines need an existing LogDatei
    QuickInstallCommand.Run(shellCommand + 'update', Output);
    writeln(rsInstall + 'opsi-script...');
    QuickInstallCommand.Run(shellCommand + 'install opsi-script', Output);
    //Output := InstallOpsiCommand.Run('opsi-script -silent -version');
    //writeln(Output);
    // remove the QuickInstall repo entry because it was only for installing opsi-script
    if FileExists('/etc/apt/sources.list.d/opsi.list') then
      QuickInstallCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);
    writeln(rsInstall + 'l-opsi-server... ' + rsSomeMin);
    // "opsi-script -batch" for installation with gui window,
    // "opsi-script-nogui -batch" for without?
    // new: opsi-script -silent for nogui
    QuickInstallCommand.Run('opsi-script -silent -batch ' + DirClientData +
      'setup.opsiscript /var/log/opsi-quick-install-l-opsi-server.log', Output);

    // get result from result file and print it
    FileText.LoadFromFile(DirClientData + 'result.conf');
    // adjust quick-install ExitCode
    if FileText[0] = 'failed' then
    begin
      ExitCode := 1;
      LogDatei.log('l-opsi-server installation failed', 1);
    end
    else
      LogDatei.log('l-opsi-server installation success', 6);
    // print result of installation
    writeln();
    writeln(FileText.Text);
    writeln(rsLog);
    writeln(LogOpsiServer);

    QuickInstallCommand.Free;
    MyRepo.Free;
    FileText.Free;
  end;

  /////////////////////////////////////////////////////////////////////////////
  procedure TQuickInstall.NoGuiQuery;
  begin
    SetDefaultValues;

    // Input variables not set by resourcestrings but by characters for no
    // requirement of a mouse.
    // distribution:
    writeln(rsDistr, ' ', distroName, ' ', distroRelease);
    writeln(rsIsCorrect, rsYesNoOp, '*');
    readln(input);
    while not ((input = 'y') or (input = 'n') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoDistribution + #10 + DistrInfo.Distribs)
      else
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
        //QueryOpsiVersion
        QueryRepo
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
    writeln(rsOpsiVersion, rsOpsiVersionOp, '*');
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
      writeln(rsRepo, ' [Example: ', baseUrlOpsi41, ']', '*')
    else if opsiVersion = 'Opsi 4.2' then
      writeln(rsRepo, ' [Example: ', baseUrlOpsi42, ']', '*');
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
      //QueryOpsiVersion
      QuerySetupType
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
    writeln(rsBackend, rsBackendOp, '*');
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
    writeln(rsCopyModules, rsYesNoOp, '*');
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
    writeln(rsRepoKind, rsRepoKindOp, '*');
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
    writeln(rsReboot, rsYesNoOp, '*');
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
    writeln(rsDhcp, rsYesNoOp, '*');
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
      begin
        // read in network details for dhcp queries (requires unit "osnetworkcalculator")
        NetworkDetails := getNetworkDetails(['IP4.ADDRESS[1]',
          'IP4.ADDRESS[2]', 'IP4.ADDRESS[3]', 'IP4.DOMAIN[1]',
          'IP4.DOMAIN[2]', 'IP4.DOMAIN[3]', 'IP4.DNS[1]', 'IP4.DNS[2]',
          'IP4.DNS[3]', 'IP4.GATEWAY']);
        // following queries only for dhcp
        QueryLink;
      end
      else
        QueryAdminName;
    end;
  end;

  procedure TQuickInstall.QueryLink;
  begin
    // link:
    writeln(rsTFTPROOT, rsLinkOp, '*');
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
        symlink := 'default.menu'
      else
        symlink := 'default.nomenu'; // cases input = 'nom', input = ''
      QueryNetmask;
    end;
  end;

  procedure TQuickInstall.QueryNetmask;
  var
    suggestion: string;
    network: array of string;
    index: integer;
  begin
    // netmask:
    // get netmask suggestions
    suggestion := '';
    // IP4.ADDRESS[1]
    index := 0;
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      suggestion += getNetmaskByIP4adr(network[1]) + ', ';
      // IP4.ADDRESS[2]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        suggestion += getNetmaskByIP4adr(network[1]) + ', ';
        // IP4.ADDRESS[3]
        index += 1;
        if NetworkDetails[index] <> '' then
        begin
          network := NetworkDetails[index].Split(['/']);
          suggestion += getNetmaskByIP4adr(network[1]) + ', ';
        end;
      end;
      Delete(suggestion, suggestion.Length - 1, 2);
      suggestion += ']';
      // query:
      writeln(rsNetmask, rsSuggestion, suggestion, '*');
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
  end;

  procedure TQuickInstall.QueryNetworkAddress;
  var
    suggestion: string;
    network: array of string;
    index: integer;
  begin
    // network address:
    suggestion := '';
    // IP4.ADDRESS[1]
    index := 0;
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      suggestion += getIP4NetworkByAdrAndMask(network[0], network[1]) + ', ';
      // IP4.ADDRESS[2]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        suggestion += getIP4NetworkByAdrAndMask(network[0], network[1]) + ', ';
        // IP4.ADDRESS[3]
        index += 1;
        if NetworkDetails[index] <> '' then
        begin
          network := NetworkDetails[index].Split(['/']);
          suggestion += getIP4NetworkByAdrAndMask(network[0], network[1]) + ', ';
        end;
      end;
    end;
    Delete(suggestion, suggestion.Length - 1, 2);
    suggestion += ']';
    // query:
    writeln(rsNetworkAddress, rsSuggestion, suggestion, '*');
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
  var
    suggestion: string;
    index: integer;
  begin
    // domain:
    suggestion := '';
    // IP4.DOMAIN[1]
    index := 3;
    if NetworkDetails[index] <> '' then
    begin
      suggestion += NetworkDetails[index] + ', ';
      // IP4.DOMAIN[2]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        suggestion += NetworkDetails[index] + ', ';
        // IP4.DOMAIN[3]
        index += 1;
        if NetworkDetails[index] <> '' then
          suggestion += NetworkDetails[index] + ', ';
      end;
    end;
    Delete(suggestion, suggestion.Length - 1, 2);
    suggestion += ']';
    // query:
    writeln(rsDomain, rsSuggestion, suggestion, '*');
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
  var
    suggestion: string;
    index: integer;
  begin
    // nameserver:
    suggestion := '';
    // IP4.DNS[1]
    index := 6;
    if NetworkDetails[index] <> '' then
    begin
      suggestion += NetworkDetails[index] + ', ';
      // IP4.DNS[2]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        suggestion += NetworkDetails[index] + ', ';
        // IP4.DNS[3]
        index += 1;
        if NetworkDetails[index] <> '' then
          suggestion += NetworkDetails[index] + ', ';
      end;
      Delete(suggestion, suggestion.Length - 1, 2);
      suggestion += ']';
      // query:
      writeln(rsNameserver, rsSuggestion, suggestion, '*');
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
  end;

  procedure TQuickInstall.QueryGateway;
  var
    suggestion: string;
  begin
    // gateway:
    suggestion := '';
    // IP4.GATEWAY
    if NetworkDetails[9] <> '' then
      suggestion += NetworkDetails[9];
    suggestion += ']';
    // query:
    writeln(rsGateway, rsSuggestion, suggestion, '*');
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
    writeln(rsAdminName, '*');
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
    writeln(rsAdminPassword, '*');
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
    // number of asked questions
    Counter: integer;
    // list of the asked questions by numbers
    queries: TStringList;
  begin
    Counter := 1;
    queries := TStringList.Create;

    // Overview
    // Print the overview and in 'queries' save the questions that were asked
    // (depending on setup type and distribution=Univention) by their number:
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
      writeln(Counter, ' ', rsTFTPROOTO, symlink);
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
    //writeln(queries.Text);

    writeln('');
    writeln(rsContinue);
    // Jumping back to a query by the number in the overview:
    {while validInput = False do
    begin
      // first test if input is valid, i.e. input = '' or input in
      validInput := True;
      readln(input);
      try
        Counter := input.ToInteger - 1;
        isInputInt := True;
      except
        writeln('"', input, '"', rsNotValid);
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
      end;}
    readln(input);
    // only elements of 'queries' (jumping back) or '' (start installation) are valid inputs
    while not ((queries.IndexOf(input) <> -1) or (input = '')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    // install opsi
    if input = '' then
    begin
      WritePropsToFile;
      InstallOpsi;
    end
    else
      // jump back to the respective question
    begin
      if input = '1' then
        QueryOpsiVersion
      else
      if input = '2' then
        QueryRepo
      else
      if input = '3' then
        QueryProxy
      else
      if input = '4' then
        QueryRepoNoCache
      else
      if input = '5' then
        QueryBackend
      else
      if input = '6' then
        QueryModules
      else
      if input = '7' then
        QueryRepoKind
      else
      if input = '8' then
        QueryUCS
      else
      if input = '9' then
        QueryReboot
      else
      if input = '10' then
        QueryDhcp
      else
      if input = '11' then
        QueryLink
      else
      if input = '12' then
        QueryNetmask
      else
      if input = '13' then
        QueryNetworkAddress
      else
      if input = '14' then
        QueryDomain
      else
      if input = '15' then
        QueryNameserver
      else
      if input = '16' then
        QueryGateway
      else
      if input = '17' then
        QueryAdminName
      else
      if input = '18' then
        QueryAdminPass
      else
      if input = '19' then
        QueryIPName
      else
      if input = '20' then
        QueryIPNumber;
    end;
  end;

  /////////////////////////////////////////////////////////////////////////////
  // no query, directly use all default values for installation
  procedure TQuickInstall.ExecuteWithDefaultValues;
  begin
    LogDatei.log('Entered ExecuteWithDefaultValues', 0);
    SetDefaultValues;
    WritePropsToFile;
    InstallOpsi;
  end;
  // no query, read in values from a file
  procedure TQuickInstall.ReadProps;
  var
    i: integer;
  begin
    // Read from file what is required for adding the repo
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
    // take text of PropsFile as text for properties.conf
    PropsFile.SaveToFile(DirClientData + 'properties.conf');

    InstallOpsi;
  end;

var
  QuickInstall: TQuickInstall;
  user, userID, customLanguage, Lang, DefLang: string;
  //r: TTranslateUnitResult;
const
  logFileName = 'opsi_quickinstall_nogui.log';

{$R *.res}

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

  // initialize log file:
  // log file in /tmp/opsi_quickinstall.log
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile(logFileName);
  LogDatei.log('Log file created', 0);

  //writeln(LowerCase((user = 'sudo').ToString(TUseBoolStrs.True)));

  QuickInstall := TQuickInstall.Create(nil);

  // get default language (system language)
  GetLanguageIDs(Lang, DefLang);
  // use default language for resourcestrings
  // Use po-files of gui version (because LCL (from the gui version) does not
  // seem to be able to use po-files from other directories while the nogui
  // version is flexible).
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
    while not ((customLanguage = 'de') or (customLanguage = 'en') or
        (customLanguage = 'fr') or (customLanguage = 'es')) do
    begin
      writeln('"', customLanguage, '"', rsNotValid);
      readln(customLanguage);
    end;
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      '../gui/locale/opsi_quick_install_project.' + customLanguage + '.po');

    writeln(rsCarryOut);
    sleep(50);
  end;

  // For default installation to indicate the start of the program (also nice
  // nice for the test environment log to identify the start of QuickInstall).
  if QuickInstall.HasOption('d', 'default') then
  begin
    writeln('');
    writeln('Start opsi-quickinstall');
  end;

  with QuickInstall do
  begin
    // distribution info:
    distroName := getLinuxDistroName;
    distroRelease := getLinuxDistroRelease;
    writeln(distroName, ' ', distroRelease);
    DistrInfo := TDistributionInfo.Create;
    DistrInfo.SetInfo(distroName, distroRelease);
    // In the nogui query the checking of the distribution will be done later,
    // for the options -d and -f do it here (for the other options like -h we
    // don't need the correct distribution):
    if HasOption('d', 'default') or HasOption('f', 'file') then
    begin
      if DistrInfo.MyDistr = other then
      begin
        writeln(rsNoSupport + #10 + DistrInfo.Distribs);
        Exit;
      end;
    end;

    // run TQuickInstall
    Run;

    DistrInfo.Free;
    // free QuickInstall
    Free;
  end;

  writeln(LogDatei.StandardMainLogPath + logFileName);
  writeln();
  LogDatei.Free;
end.
