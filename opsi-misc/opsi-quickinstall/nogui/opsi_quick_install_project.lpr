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
  DistributionInfo,
  osRunCommandElevated,
  osfunclin,
  osLinuxRepository,
  oslog,
  osnetworkcalculator,
  LOpsiServerDownloader,
  opsi_quick_install_resourcestrings,
  opsiquickinstall_data;

type

  { TQuickInstall }

  TQuickInstall = class(TCustomApplication)
  private
  var
    input: string;
    NetworkDetails: array of string;
    FileText, PropsFile: TStringList;
    QuickInstallCommand: TRunCommandElevated;
    DirClientData, Output: string;
    two_los_to_test, one_installation_failed: boolean;
    name_los_default, name_los_downloaded, name_current_los: string;
    version_los_default, version_los_downloaded: string;
  const
    procedure SetDefaultValues;
    procedure defineDirClientData;
    procedure writePropsToFile;
    procedure addRepo;
    procedure executeLOSscript;
    procedure installOpsi;
    procedure NoGuiQuery;
    procedure QuerySetupType;
    //procedure QueryOpsiVersion;
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
    procedure ReadPropsFromFile;
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
      (*project := ExtractFilePath(ParamStr(0));
      Delete(project, Length(project), 1);
      project := ExtractFilePath(project) + 'gui/opsi_quick_install_project';
      writeln(project);
      ExecuteProcess(project, '', []);*)
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
      // read properties from file
      PropsFile := TStringList.Create;
      try
        begin
          //writeln(getOptionValue('f', 'file'));
          //writeln(FileExists(getOptionValue('f', 'file')).ToString(TUseBoolStrs.true));
          PropsFile.LoadFromFile(getOptionValue('f', 'file'));
          ReadPropsFromFile;
        end;
      except
        writeln('Executing Opsi Quick-Install with properties file didn''t work!');
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
    LogDatei.log('Set default values', LLdebug);
    Data.opsiVersion := 'Opsi 4.2';
    if Data.opsiVersion = 'Opsi 4.1' then
      Data.repo := Data.baseRepoUrlOpsi41
    else
      Data.repo := Data.baseRepoUrlOpsi42;
    Data.proxy := '';
    Data.repoNoCache := Data.repo;
    Data.backend := 'file';
    Data.copyMod.SetEntries(rsNo, 'false');
    Data.repoKind := 'stable';
    Data.ucsPassword := '';
    Data.reboot.SetEntries(rsNo, 'false');
    Data.dhcp.SetEntries(rsNo, 'false');
    Data.symlink := 'default.nomenu';
    Data.netmask := '255.255.0.0';
    Data.networkAddress := '192.168.0.0';
    Data.domain := 'uib.local';
    Data.nameserver := '192.168.1.245';
    Data.gateway := '192.168.1.245';
    Data.adminName := 'adminuser';
    Data.adminPassword := 'linux123';
    Data.ipName := 'auto';
    Data.ipNumber := 'auto';
  end;

  procedure TQuickInstall.DefineDirClientData;
  var
    los_default_search, los_downloaded_search: TSearchRec;
  begin
    DirClientData := ExtractFilePath(ParamStr(0));
    Delete(DirClientData, Length(DirClientData), 1);
    //DirClientData := ExtractFilePath(DirClientData) + 'l-opsi-server';
    DirClientData := ExtractFilePath(DirClientData);

    if two_los_to_test then writeln(rsWait);
    // try downloading latest l-opsi-server and set DirClientData for the latest version
    if two_los_to_test and DownloadLOS(QuickInstallCommand, Data.DistrInfo) then
    begin
      // extract and compare version numbers of default and downloaded los
      if (FindFirst('../l-opsi-server_4.*', faAnyFile and faDirectory,
        los_default_search) = 0) and
        (FindFirst('../downloaded_l-opsi-server_4.*', faAnyFile and
        faDirectory, los_downloaded_search) = 0) then
      begin
        name_los_default := los_default_search.Name;
        name_los_downloaded := los_downloaded_search.Name;
        // extract version numbers
        version_los_default := los_default_search.Name;
        Delete(version_los_default, 1, Pos('_', version_los_default));
        version_los_downloaded := los_downloaded_search.Name;
        Delete(version_los_downloaded, 1, Pos('_', version_los_downloaded));
        Delete(version_los_downloaded, 1, Pos('_', version_los_downloaded));
        // compare and use latest l-opsi-server version
        if version_los_downloaded > version_los_default then
          name_current_los := name_los_downloaded
        else
        begin
          name_current_los := name_los_default;
          if version_los_downloaded = version_los_default then
            two_los_to_test := False;
        end;
      end;
    end
    else
    if one_installation_failed then
    begin
      // if there is a downloaded los but the latest los version failed to install,
      // switch between name_los_default and name_los_downloaded to get the dir of
      // the older version
      if version_los_downloaded > version_los_default then
        name_current_los := name_los_default
      else
        name_current_los := name_los_downloaded;
    end
    else
    // otherwise, in the case that downloading the latest l-opsi-server failed,
    // use the default one
    if FindFirst('../l-opsi-server_4.*', faAnyFile and faDirectory,
      los_default_search) = 0 then
    begin
      name_los_default := los_default_search.Name;
      // extract version numbers
      version_los_default := los_default_search.Name;
      Delete(version_los_default, 1, Pos('_', version_los_default));
      name_current_los := name_los_default;
      two_los_to_test := False;
    end;
    DirClientData += name_current_los + '/CLIENT_DATA/';
  end;

  // write properties in properties.conf file
  procedure TQuickInstall.WritePropsToFile;
  begin
    LogDatei.log('Entered WritePropsToFile', LLdebug);
    // write file text
    FileText := TStringList.Create;

    FileText.Add('allow_reboot=' + Data.reboot.PropertyEntry);
    FileText.Add('backend=' + Data.backend);
    FileText.Add('dnsdomain=' + Data.domain);
    FileText.Add('force_copy_modules=' + Data.copyMod.PropertyEntry);
    FileText.Add('gateway=' + Data.gateway);
    FileText.Add('install_and_configure_dhcp=' + Data.dhcp.PropertyEntry);
    FileText.Add('myipname=' + Data.ipName);
    FileText.Add('myipnumber=' + Data.ipNumber);
    FileText.Add('nameserver=' + Data.nameserver);
    FileText.Add('netmask=' + Data.netmask);
    FileText.Add('network=' + Data.networkAddress);
    FileText.Add('opsi_admin_user_name=' + Data.adminName);
    FileText.Add('opsi_admin_user_password=' + Data.adminPassword);
    FileText.Add('opsi_online_repository=' + Data.repo);
    FileText.Add('opsi_noproxy_online_repository=' + Data.repoNoCache);
    FileText.Add('patch_default_link_for_bootimage=' + Data.symlink);
    FileText.Add('proxy=' + Data.proxy);
    FileText.Add('repo_kind=' + Data.repoKind);
    FileText.Add('ucs_master_admin_password=' + Data.ucsPassword);
    // update_test shall always be false
    FileText.Add('update_test=false');

    DefineDirClientData;

    // write in properties.conf file:
    if not FileExists(DirClientData + 'properties.conf') then
      QuickInstallCommand.Run('touch ' + DirClientData + 'properties.conf', Output);
    QuickInstallCommand.Run('chown -c $USER ' + DirClientData +
      'properties.conf', Output);
    FileText.SaveToFile(DirClientData + 'properties.conf');

    FileText.Free;
  end;

  procedure TQuickInstall.AddRepo;
  var
    url: string;
    ReleaseKeyRepo: TLinuxRepository;
  begin
    writeln(rsCreateRepo);
    // first remove opsi.list to have a cleared opsi repository list
    if FileExists('/etc/apt/sources.list.d/opsi.list') then
      QuickInstallCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);
    // create repository (no password, user is root):
    ReleaseKeyRepo := TLinuxRepository.Create(Data.DistrInfo.Distr, '', False);
    // set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
    if Data.opsiVersion = 'Opsi 4.1' then
      ReleaseKeyRepo.GetDefaultURL(Opsi41, stringToOpsiBranch(Data.repoKind))
    else
      ReleaseKeyRepo.GetDefaultURL(Opsi42, stringToOpsiBranch(Data.repoKind));
    // define repo url
    url := Data.repo + Data.repoKind + '/' + Data.DistrInfo.DistrRepoUrlPart;

    // !following lines need an existing LogDatei
    if (Data.DistrInfo.DistroName = 'openSUSE') or (Data.DistrInfo.DistroName = 'SUSE') then
    begin
      writeln('OpenSUSE/SUSE: Add Repo');
      ReleaseKeyRepo.Add(url, 'OpsiQuickInstallRepositoryNew');
    end
    else
      ReleaseKeyRepo.Add(url);

    ReleaseKeyRepo.Free;
  end;

  // install opsi-script and execute l-opsi-server script
  procedure TQuickInstall.ExecuteLOSscript;
  begin
    // Set text of result.conf to 'failed' first (for safety)
    FileText := TStringList.Create;
    FileText.Add('failed');
    if not FileExists(DirClientData + 'result.conf') then
      QuickInstallCommand.Run('touch ' + DirClientData + 'result.conf', Output);

    QuickInstallCommand.Run('chown -c $USER ' + DirClientData + 'result.conf', Output);
    FileText.SaveToFile(DirClientData + 'result.conf');
    FileText.Free;

    Data.DistrInfo.SetPackageManagementShellCommand;
    // !following lines need an existing LogDatei
    // if one installation failed, then opsi-script was already installed
    if not one_installation_failed then
    begin
      QuickInstallCommand.Run(Data.DistrInfo.PackageManagementShellCommand +
        'update', Output);
      writeln(rsInstall + 'opsi-script...');
      QuickInstallCommand.Run(Data.DistrInfo.PackageManagementShellCommand +
        'install opsi-script', Output);
    end;
    //Output := InstallOpsiCommand.Run('opsi-script -silent -version');
    //writeln(Output);
    // remove the QuickInstall repo entry because it was only for installing opsi-script
    if FileExists('/etc/apt/sources.list.d/opsi.list') then
      QuickInstallCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);

    writeln(rsInstall + name_current_los + '... ' + rsSomeMin);
    // "opsi-script -batch" for installation with gui window,
    // "opsi-script-nogui -batch" for without?
    // new: opsi-script -silent for nogui
    QuickInstallCommand.Run('opsi-script -silent -batch ' + DirClientData +
      'setup.opsiscript /var/log/opsi-quick-install-l-opsi-server.log', Output);
  end;

  // install opsi-server
  // requires: opsiVersion, repoKind, distroName, DistrInfo, existing LogDatei
  procedure TQuickInstall.InstallOpsi;
  var
    installationResult: string;
  begin
    LogDatei.log('Entered InstallOpsi', LLdebug);
    writeln(rsInstall + Data.opsiVersion + ':');
    addRepo;

    // install opsi-server
    two_los_to_test := True;
    one_installation_failed := False;
    if HasOption('f', 'file') then
    begin
      defineDirClientData;
      // take text of PropsFile as text for properties.conf
      PropsFile.SaveToFile(DirClientData + 'properties.conf');
    end
    else
      writePropsToFile;

    executeLOSscript;

    // get result from result file and print it
    FileText := TStringList.Create;
    FileText.LoadFromFile(DirClientData + 'result.conf');
    // adjust quick-install ExitCode
    if (FileText[0] = 'failed') and two_los_to_test then
    begin
      // if installation of latest l-opsi-server failed, try the older version:
      writeln(rsInstallation + rsFailed + '. ' + rsTryOlderLOS + '.');
      Sleep(1000);
      LogDatei.log('Installation failed: ' + name_current_los, LLessential);
      LogDatei.log('Try older version of l-opsi-server:', LLnotice);
      two_los_to_test := False;
      one_installation_failed := True;
      FileText.Free;
      if HasOption('f', 'file') then
      begin
        DefineDirClientData;
        // take text of PropsFile as text for properties.conf
        PropsFile.SaveToFile(DirClientData + 'properties.conf');
      end
      else
        WritePropsToFile;

      executeLOSscript;
      FileText := TStringList.Create;
      FileText.LoadFromFile(DirClientData + 'result.conf');
    end;

    if FileText[0] = 'failed' then
    begin
      installationResult := rsFailed;
      writeln(rsInstallation + rsFailed + '.');
      LogDatei.log('Installation failed: ' + name_current_los, LLessential);
      LogDatei.log(Data.opsiVersion + ' installation failed', LLessential);
      ExitCode := 1;
    end
    else
    begin
      installationResult := rsSuccess;
      LogDatei.log('Installation successful: ' + name_current_los, LLessential);
      LogDatei.log(Data.opsiVersion + ' installation successful', LLessential);
    end;
    // print result of installation
    Sleep(1000);
    writeln();
    writeln(rsInstallationOf + Data.opsiVersion + ' ' + installationResult + '!');
    Sleep(1000);
    writeln();
    writeln(rsLog);
    writeln(LogOpsiServer);

    QuickInstallCommand.Free;
    FileText.Free;
  end;

  /////////////////////////////////////////////////////////////////////////////
  {$REGION 'query' fold}
  procedure TQuickInstall.NoGuiQuery;
  var
    UserEditedDistroName, UserEditedDistroRelease: string;
  begin
    SetDefaultValues;

    // Input variables not set by resourcestrings but by characters for no
    // requirement of a mouse.
    // distribution:
    writeln(rsDistr, ' ', Data.DistrInfo.DistroName, ' ', Data.DistrInfo.DistroRelease);
    writeln(rsIsCorrect, rsYesNoOp, '*');
    readln(input);
    while not ((input = 'y') or (input = 'n') or (input = '')) do
    begin
      if input = '-h' then
        writeln(rsInfoDistribution + #10 + Data.DistrInfo.Distribs)
      else
        writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    // if distribution isn't correct, read the correct one
    if input = 'n' then
    begin
      writeln(rsOtherDistr);
      readln(input);
      UserEditedDistroName := Copy(input, 1, Pos(' ', input) - 1);
      UserEditedDistroRelease :=
        Copy(input, Pos(' ', input) + 1, Length(input) - Pos(' ', input));
      Data.DistrInfo.CorrectDistributionNameAndRelease(UserEditedDistroName,
        UserEditedDistroRelease);
    end;
    Data.DistrInfo.SetDistrAndUrlPart;
    if Data.DistrInfo.Distr = other then
    begin
      writeln('');
      writeln(rsNoSupport + #10 + Data.DistrInfo.Distribs);
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
      if input = '' then
        Data.CustomSetup := false
        else Data.CustomSetup := true;
      writeln('');
      writeln(rsCarryOut);
      writeln('');
      if Data.CustomSetup then
        // following queries only for custom setup
        //QueryOpsiVersion
        QueryRepo
      else
      if Data.DistrInfo.DistroName = 'Univention' then
        QueryUCS
      else
        QueryDhcp;
    end;
  end;

  {procedure TQuickInstall.QueryOpsiVersion;
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
  end;}

  procedure TQuickInstall.QueryRepo;
  begin
    // repo:
    if Data.opsiVersion = 'Opsi 4.1' then
      writeln(rsRepo, ' [Example: ', Data.baseRepoUrlOpsi41, ']', '*')
    else if Data.opsiVersion = 'Opsi 4.2' then
      writeln(rsRepo, ' [Example: ', Data.baseRepoUrlOpsi42, ']', '*');
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
      Data.repo := input;
      if (input = '') and (Data.opsiVersion = 'Opsi 4.1') then
        Data.repo := Data.baseRepoUrlOpsi41
      else
      if (input = '') and (Data.opsiVersion = 'Opsi 4.2') then
        Data.repo := Data.baseRepoUrlOpsi42;
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
        Data.proxy := input;
      end
      else
        Data.proxy := '';
      QueryRepoNoCache;
    end;
  end;

  procedure TQuickInstall.QueryRepoNoCache;
  begin
    // repo without cache proxy:
    if Data.opsiVersion = 'Opsi 4.1' then
      writeln(rsRepoNoCache, ' [Example: ', Data.baseRepoUrlOpsi41, ']')
    else if Data.opsiVersion = 'Opsi 4.2' then
      writeln(rsRepoNoCache, ' [Example: ', Data.baseRepoUrlOpsi42, ']');
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
      Data.repoNoCache := input;
      if input = '' then
        Data.repoNoCache := Data.repo;
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
        Data.backend := 'mysql'
      else
        Data.backend := 'file'; // cases input = 'f', input = ''
      if Data.backend = 'mysql' then
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
        Data.copyMod.SetEntries(rsYes, 'true')
      else
        Data.copyMod.SetEntries(rsNo, 'false'); // cases input = 'n', input = ''
      QueryRepoKind;
    end;
  end;

  procedure TQuickInstall.QueryRepoKind;
  begin
    // repo kind:
    writeln(rsRepoKind, rsRepoKindOp, '*');
    readln(input);
    while not ((input = 'e') or (input = 't') or (input = 's') or
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
      if Data.backend = 'mysql' then
        QueryModules
      else
        QueryBackend;
    end
    else
    begin
      if input = 'e' then
        Data.repoKind := 'experimental'
      else if input = 't' then
        Data.repoKind := 'testing'
      else
        Data.repoKind := 'stable'; // cases input = 's', input = ''
      if Data.DistrInfo.DistroName = 'Univention' then
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
      if not Data.CustomSetup then
        QuerySetupType
      else
        QueryRepoKind;
    end
    else // go forward
    begin
      Data.ucsPassword := input;
      if Data.CustomSetup then
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
      if Data.DistrInfo.DistroName = 'Univention' then
        QueryUCS
      else
        QueryRepoKind;
    end
    else
    begin
      if input = 'y' then
        Data.reboot.SetEntries(rsYes, 'true')
      else
        Data.reboot.SetEntries(rsNo, 'false'); // cases input = 'n', input = ''
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
      if Data.CustomSetup then
        QueryReboot
      else
      begin
        if Data.DistrInfo.DistroName = 'Univention' then
          QueryUCS
        else
          QuerySetupType;
      end;
    end
    else
    begin
      if input = 'y' then
        Data.dhcp.SetEntries(rsYes, 'true')
      else
        Data.dhcp.SetEntries(rsNo, 'false'); // cases input = 'n', input = ''
      if Data.dhcp.PropertyEntry = 'true' then
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
        Data.symlink := 'default.menu'
      else
        Data.symlink := 'default.nomenu'; // cases input = 'nom', input = ''
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
        Data.netmask := input;
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
      Data.networkAddress := input;
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
      Data.domain := input;
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
        Data.nameserver := input;
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
      Data.gateway := input;
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
      if Data.dhcp.PropertyEntry = 'true' then
        QueryGateway
      else
        QueryDhcp;
    end
    else
    begin
      Data.adminName := input;
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
      Data.adminPassword := input;
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
      if Data.adminName = '' then
        QueryAdminName
      else
        QueryAdminPass;
    end
    else
    begin
      Data.ipName := input;
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
      Data.ipNumber := input;
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
    if not Data.CustomSetup then
      writeln(rsOpsiVersionO, Data.opsiVersion)
    else
    begin
      writeln(Counter, ' ', rsOpsiVersionO, Data.opsiVersion);
      queries.Add('1');
      Inc(Counter);
    end;
    {Custom installation}
    if Data.CustomSetup then
    begin
      writeln(Counter, ' ', rsRepoO, Data.repo);
      queries.Add('2');
      Inc(Counter);
      writeln(Counter, ' ', rsProxyO, Data.proxy);
      queries.Add('3');
      Inc(Counter);
      writeln(Counter, ' ', rsRepoNoCacheO, Data.repoNoCache);
      queries.Add('4');
      Inc(Counter);
      writeln(Counter, ' ', rsBackendO, Data.backend);
      queries.Add('5');
      Inc(Counter);
      if Data.backend = 'mysql' then
      begin
        writeln(Counter, ' ', rsCopyModulesO, Data.copyMod.OverviewEntry);
        queries.Add('6');
        Inc(Counter);
      end;
      writeln(Counter, ' ', rsRepoKindO, Data.repoKind);
      queries.Add('7');
      Inc(Counter);
    end;
    {Both}
    if Data.DistrInfo.DistroName = 'Univention' then
    begin
      writeln(Counter, ' ', rsUCSO, Data.ucsPassword);
      queries.Add('8');
      Inc(Counter);
    end;
    {Custom installation}
    if Data.CustomSetup then
    begin
      writeln(Counter, ' ', rsRebootO, Data.reboot.OverviewEntry);
      queries.Add('9');
      Inc(Counter);
    end;
    {Both}
    writeln(Counter, ' ', rsDhcpO, Data.dhcp.OverviewEntry);
    queries.Add('10');
    Inc(Counter);
    if Data.dhcp.PropertyEntry = 'true' then
    begin
      writeln(Counter, ' ', rsTFTPROOTO, Data.symlink);
      queries.Add('11');
      Inc(Counter);
      writeln(Counter, ' ', rsNetmaskO, Data.netmask);
      queries.Add('12');
      Inc(Counter);
      writeln(Counter, ' ', rsNetworkO, Data.networkAddress);
      queries.Add('13');
      Inc(Counter);
      writeln(Counter, ' ', rsDomainO, Data.domain);
      queries.Add('14');
      Inc(Counter);
      writeln(Counter, ' ', rsNameserverO, Data.nameserver);
      queries.Add('15');
      Inc(Counter);
      writeln(Counter, ' ', rsGatewayO, Data.gateway);
      queries.Add('16');
      Inc(Counter);
    end;
    writeln(Counter, ' ', rsAdminNameO, Data.adminName);
    queries.Add('17');
    Inc(Counter);
    if Data.adminName <> '' then
    begin
      writeln(Counter, ' ', rsAdminPasswordO, Data.adminPassword);
      queries.Add('18');
      Inc(Counter);
    end;
    writeln(Counter, ' ', rsIPNameO, Data.ipName);
    queries.Add('19');
    Inc(Counter);
    writeln(Counter, ' ', rsIPNumberO, Data.ipNumber);
    queries.Add('20');
    //writeln(queries.Text);

    writeln('');
    writeln(rsContinue);
    // Jumping back to a query by the number in the overview:
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
      //WritePropsToFile;
      InstallOpsi;
    end
    else
      // jump back to the respective question
    begin
      {if input = '1' then
        QueryOpsiVersion
      else}
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

  {$ENDREGION}
  /////////////////////////////////////////////////////////////////////////////

  // no query options:

  procedure TQuickInstall.ExecuteWithDefaultValues;
  begin
    LogDatei.log('Execute with default values:', LLdebug);
    SetDefaultValues;
    InstallOpsi;
  end;

  procedure TQuickInstall.ReadPropsFromFile;
  begin
    LogDatei.log('Read properties from file:', LLdebug);
    // Read from file what is required for adding the repo
    Data.repoKind := PropsFile.Values['repo_kind'];
    Data.repo := PropsFile.Values['opsi_online_repository'];

    // Read opsi version from repo url
    if Pos('4.1', Data.repo) > 0 then
      Data.opsiVersion := 'Opsi 4.1'
    else
      Data.opsiVersion := 'Opsi 4.2';

    InstallOpsi;
  end;

  {Program}

  procedure CheckThatUserIsRoot;
  var
    user, userID: string;
  begin
    // get user and as safeguard also the user id
    if (RunCommand('/bin/sh', ['-c', 'echo | id -nu'], user) and
      RunCommand('/bin/sh', ['-c', 'echo | id -u'], userID)) then
    begin
      Delete(user, user.Length, 1);
      Delete(userID, userID.Length, 1);
    end;

    if not ((user = 'root') and (userID = '0')) then
    begin
      writeln('Please execute Opsi Quick-Install as root!');
      exit;
    end;
  end;

  procedure InitializeLogfile(LogfileName: string);
  begin
    // log file will be created in /tmp/opsi_quickinstall.log
    LogDatei := TLogInfo.Create;
    LogDatei.CreateTheLogfile(LogfileName);
    LogDatei.log('Log file created', LLdebug);
    SetCurrentDir(ExtractFilePath(ParamStr(0)));
    LogDatei.log('Working directory: ' + GetCurrentDir, LLessential);
  end;

  procedure UseSystemLanguageForResourcestrings;
  var
    Lang, DefLang: string;
  begin
    // get default language (system language)
    GetLanguageIDs(Lang, DefLang);
    // use po-files of gui version (because LCL (from the gui version) does not
    // seem to be able to use po-files from other directories while the nogui
    // version is flexible)
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      '../gui/locale/opsi_quick_install_project.%s.po', Lang, DefLang);
  end;

  procedure WelcomeUser;
  begin
    writeln('');
    writeln(rsWelcome);
  end;

  procedure UseUserDefinedLanguage;
  var
    UserDefinedLang: string;
  begin
    writeln(rsSelLanguage, rsLangOp);
    readln(UserDefinedLang);
    // check for right input
    while not ((UserDefinedLang = 'de') or (UserDefinedLang = 'en') or
        (UserDefinedLang = 'fr') or (UserDefinedLang = 'es')) do
    begin
      writeln('"', UserDefinedLang, '"', rsNotValid);
      readln(UserDefinedLang);
    end;
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      '../gui/locale/opsi_quick_install_project.' + UserDefinedLang + '.po');

    writeln(rsCarryOut);
    sleep(50);
  end;

  procedure CheckThatOqiSupportsDistribution(QuickInstall: TQuickInstall);
  begin
    // In the nogui query the checking of the distribution will be done later to
    // give the user the option to edit a wrongly detected distribution.
    if QuickInstall.HasOption('d', 'default') or QuickInstall.HasOption('f', 'file') then
    begin
      if Data.DistrInfo.Distr = other then
      begin
        writeln(rsNoSupport + #10 + Data.DistrInfo.Distribs);
        Exit;
      end;
    end;
  end;

  procedure InitializeDistributionInfo(QuickInstall: TQuickInstall);
  begin
    Data.DistrInfo := TDistributionInfo.Create(getLinuxDistroName,
      getLinuxDistroRelease);
    LogDatei.log(Data.DistrInfo.DistroName + ' ' +
      Data.DistrInfo.DistroRelease, LLessential);
    Data.DistrInfo.SetDistrAndUrlPart;
    CheckThatOqiSupportsDistribution(QuickInstall);
  end;

var
  QuickInstall: TQuickInstall;
  //r: TTranslateUnitResult;
const
  LogfileName = 'opsi_quickinstall_nogui.log';

{$R *.res}

begin
  // Only execute Opsi-QuickInstall(oqi) if user is root
  CheckThatUserIsRoot;
  InitializeLogfile(LogfileName);

  QuickInstall := TQuickInstall.Create(nil);
  Data := TQuickInstallData.Create;
  QuickInstall.QuickInstallCommand := TRunCommandElevated.Create('', False);

  UseSystemLanguageForResourcestrings;
  // do language selection here only for nogui installation
  if QuickInstall.HasOption('n', 'nogui') then
  begin
    WelcomeUser;
    UseUserDefinedLanguage;
  end;

  // Indicate start of program for default installation (also good
  // for test environment log to identify start of QuickInstall).
  if QuickInstall.HasOption('d', 'default') then
  begin
    writeln('');
    writeln('Start opsi-quickinstall');
  end;

  InitializeDistributionInfo(QuickInstall);
  QuickInstall.Run;

  QuickInstall.Free;

  writeln(LogDatei.StandardMainLogPath + logFileName);
  writeln();
  LogDatei.Free;
end.
