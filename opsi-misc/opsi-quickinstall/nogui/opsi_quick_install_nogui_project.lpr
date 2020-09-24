program opsi_quick_install_nogui_project;

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
  opsi_quick_install_resourcestrings;

type

  { TQuickInstall }

  TQuickInstall = class(TCustomApplication)
  private
  var
    // Never write the log file in variables as follows!
    //LogDatei: TLogInfo;
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
    fileName, url, shellCommand, Output: string;
  const
    baseUrlOpsi41 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    baseUrlOpsi42 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';

    procedure SetDefaultValues;
    procedure NoGuiQuery;
    procedure ExecuteWithDefaultValues;
    procedure ReadProps;
    // write properties in l-opsi-server.conf and properties.conf file
    procedure WritePropsToFile;
    // install opsi-server
    procedure InstallOpsi;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  user: string;
  LogDatei: TLogInfo;

{resourcestring
  rsHi = 'Hello';
  rsMorning = 'Morning';}

  { TQuickInstall }

  procedure TQuickInstall.DoRun;
  var
    ErrorMsg: string;
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
      writeln(getOptionValue('f', 'file'));
      // read properties from file
      PropsFile := TStringList.Create;
      try
        begin
          PropsFile.LoadFromFile(getOptionValue('f', 'file'));
          ReadProps;
        end;
      except
        writeln('Executing Opsi Qiock-Install didn''t work!');
      end;
      PropsFile.Free;
      Terminate;
      Exit;
    end;

    { add your program here }
    writeln('Huhu?');
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
    writeln('Help please');
    writeln('Write:');
    readln;
    writeln('Exit');
  end;

  procedure TQuickInstall.SetDefaultValues;
  begin
    // set default values:
    opsiVersion := 'opsi 4.1';
    repo := baseUrlOpsi41;
    proxy := '';
    repoNoCache := repo;
    backend := 'file';
    copyMod := 'no';
    repoKind := 'stable';
    ucsPassword := 'linux123';
    reboot := 'false';
    dhcp := 'no';
    link := 'default.nomenu';
    netmask := '255.255.0.0';
    networkAddress := '192.168.0.0';
    domain := 'uib.local';
    nameserver := '192.168.1.245';
    gateway := '192.168.1.245';
    adminName := 'Anne-Marie';
    adminPassword := 'linux123';
    ipName := 'auto';
    ipNumber := 'auto';
  end;

  // write properties in l-opsi-server.conf and properties.conf file
  procedure TQuickInstall.WritePropsToFile;
  begin
    // write properties in l-opsi-server.conf and properties.conf file:
    FileText := TStringList.Create;
    FileText.Add('allow_reboot=' + reboot);
    FileText.Add('backend=' + backend);
    FileText.Add('dnsdomain=' + domain);
    FileText.Add('force_copy_modules=' + copyMod);
    FileText.Add('gateway=' + gateway);
    FileText.Add('install_and_configure_dhcp=' + dhcp);
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
    fileName := ExtractFilePath(ParamStr(0));
    FileText.SaveToFile(fileName + 'l-opsi-server.conf');
    // write in properties.conf file:
    // navigate to CLIENT_DATA in l-opsi-server
    Delete(fileName, Length(fileName), 1);
    fileName := ExtractFilePath(fileName) + 'l-opsi-server/CLIENT_DATA/';
    FileText.SaveToFile(fileName + 'properties.conf');

    FileText.Free;
  end;

  // install opsi-server
  procedure TQuickInstall.InstallOpsi;
  begin
    fileName := ExtractFilePath(ParamStr(0));
    Delete(fileName, Length(fileName), 1);
    fileName := ExtractFilePath(fileName) + 'l-opsi-server/CLIENT_DATA/';

    // Important for getting the result 'failed' in case of a wrong password...
    // ...cause in this case the RunCommands aren't executed...
    // ...and therefore setup.opsiscript, that usually does it, isn't too:
    FileText := TStringList.Create;
    FileText.Add('failed');
    FileText.SaveToFile(fileName + 'result.conf');

    //writeln(user = 'sudo');
    //writeln(LowerCase((user = 'sudo').ToString(TUseBoolStrs.True)));

    // create repository
    MyRepo := TLinuxRepository.Create(DistrInfo.MyDistr, '', False);
    // Set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
    if opsiVersion = 'opsi 4.1' then
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
    // following two lines need an existing LogDatei
    MyRepo.Add(url);
    InstallOpsiCommand := TRunCommandElevated.Create('', False);
    shellCommand := DistrInfo.GetPackageManagementShellCommand(distroName);
    // following lines need an existing LogDatei
    Output := InstallOpsiCommand.Run(shellCommand + 'update');
    //writeln(Output);
    Output := InstallOpsiCommand.Run(shellCommand + 'install opsi-script');
    //writeln(Output);
    Output := InstallOpsiCommand.Run('opsi-script -batch ' + fileName +
      'setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log');

    // get result from result file and print it
    FileText.LoadFromFile(fileName + 'result.conf');
    writeln(FileText.Text);

    FileText.Free;
    InstallOpsiCommand.Free;
    MyRepo.Free;
  end;

  /////////////////////////////////////////////////////////////////////////////
  procedure TQuickInstall.NoGuiQuery;
  begin
    SetDefaultValues;

    // setup type:
    writeln(rsSetup, rsSetupOp);
    readln(input);
    while not ((input = 'standard') or (input = 'custom')) do
    begin
      writeln(input, rsNotValid);
      readln(input);
    end;
    setupType := input;

    writeln('');
    writeln(rsCarryOut);
    writeln('');

    // distribution:
    distroName := getLinuxDistroName;
    distroRelease := getLinuxDistroRelease;
    //writeln(distroName, distroRelease);
    DistrInfo := TDistributionInfo.Create;
    writeln(rsDistr, ' ', distroName, ' ', distroRelease);
    writeln(rsIsCorrect, rsYesNoOp);
    readln(input);
    while not ((input = 'yes') or (input = 'no')) do
    begin
      writeln(input, rsNotValid);
      readln(input);
    end;
    // if distribution isn't correct, read the correct one
    if input = 'no' then
    begin
      writeln(rsOtherDistr);
      readln(input);
      distroName := Copy(input, 1, Pos(' ', input) - 1);
      distroRelease := Copy(input, Pos(' ', input) + 1, Length(input) - Pos(' ', input));
      //What to do with unknown distribution like ubuntu 20.04?
    end;
    DistrInfo.SetInfo(distroName, distroRelease);

    if setupType = 'custom' then
      // following queries only for custom setup
    begin
      // opsi version:
      writeln(rsOpsiVersion, rsOpsiVersionOp);
      readln(input);
      while not ((input = 'opsi 4.1') or (input = 'opsi 4.2')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      opsiVersion := input;
      // repo:
      if opsiVersion = 'opsi 4.1' then
        writeln(rsRepo, ' [Example: ', baseUrlOpsi41, ']')
      else if opsiVersion = 'opsi 4.2' then
        writeln(rsRepo, ' [Example: ', baseUrlOpsi42, ']');
      readln(input);
      while not Pos('http', input) = 1 do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      repo := input;
      // proxy:
      writeln(rsUseProxy, rsYesNoOp);
      readln(input);
      while not ((input = 'yes') or (input = 'no')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      if input = 'yes' then
      begin
        writeln('Which Proxy would you like to use? [Example: "http://myproxy.dom.org:8080"]');
        readln(input);
        proxy := input;
      end;
      // repo wihout cache proxy:
      if opsiVersion = 'opsi 4.1' then
        writeln(rsRepoNoCache, ' [Example: ', baseUrlOpsi41, ']')
      else if opsiVersion = 'opsi 4.2' then
        writeln(rsRepoNoCache, ' [Example: ', baseUrlOpsi42, ']');
      readln(input);
      while not Pos('http', input) = 1 do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      repoNoCache := input;

      // backend
      writeln(rsBackend, rsBackendOp);
      readln(input);
      while not ((input = 'file') or (input = 'mysql')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      backend := input;
      if input = 'mysql' then
      begin
        // copy modules
        writeln(rsCopyModules, rsYesNoOp);
        readln(input);
        while not ((input = 'yes') or (input = 'no')) do
        begin
          writeln(input, rsNotValid);
          readln(input);
        end;
        copyMod := input;
      end;
      // repo kind
      writeln(rsRepoKind, rsRepoKindOp);
      readln(input);
      while not ((input = 'experimental') or (input = 'stable') or
          (input = 'testing')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      repoKind := input;

      // ucs password
      if distroName = 'Univention' then
      begin
        writeln(rsUCS);
        readln(input);
        ucsPassword := input;
      end;
      // reboot
      writeln(rsReboot, rsYesNoOp);
      readln(input);
      while not ((input = 'yes') or (input = 'no')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      reboot := input;
    end;
    // dhcp
    writeln(rsDhcp, rsYesNoOp);
    readln(input);
    while not ((input = 'yes') or (input = 'no')) do
    begin
      writeln(input, rsNotValid);
      readln(input);
    end;
    dhcp := input;
    if input = 'yes' then
      // following queries only for dhcp
    begin
      // link
      writeln(rsTFTPROOT, rsLinkOp);
      readln(input);
      while not ((input = 'default.menu') or (input = 'default.nomenu')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      link := input;
      // netmask
      writeln(rsNetmask, rsNetmaskEx);
      readln(input);
      netmask := input;
      // network address
      writeln(rsNetworkAddress, rsNetworkAddressEx);
      readln(input);
      networkAddress := input;
      // domain
      writeln(rsDomain, rsDomainEx);
      readln(input);
      domain := input;
      // nameserver
      writeln(rsNameserver, rsNameserverEx);
      readln(input);
      nameserver := input;
      // gateway
      writeln(rsGateway, rsGatewayEx);
      readln(input);
      gateway := input;
    end;

    // user name
    writeln(rsAdminName);
    readln(input);
    adminName := input;
    // user password
    writeln(rsAdminPassword);
    readln(input);
    adminPassword := input;
    // IP name
    writeln(rsIPName);
    readln(input);
    ipName := input;
    // IP number
    writeln(rsIPNumber);
    readln(input);
    ipNumber := input;

    // Overview???

    InstallOpsi;
    DistrInfo.Free;
  end;

  //////////////////////////////////////////////////////////////////////////////
  procedure TQuickInstall.ExecuteWithDefaultValues;
  begin
    SetDefaultValues;

    DistrInfo := TDistributionInfo.Create;
    DistrInfo.SetInfo('Ubuntu', '18.04');

    WritePropsToFile;
    InstallOpsi;
    DistrInfo.Free;
  end;

  procedure TQuickInstall.ReadProps;
  var
    i: integer;
  begin
    // distribution:
    distroName := getLinuxDistroName;
    distroRelease := getLinuxDistroRelease;
    //writeln(distroName, distroRelease);
    DistrInfo := TDistributionInfo.Create;
    DistrInfo.SetInfo(distroName, distroRelease);

    // IndexOf(...) is 0-based
    for i := 0 to PropsFile.Count do
    begin
      //writeln(i);
      if Pos('repo_kind', PropsFile[i]) = 1 then
      begin
        repoKind := Copy(PropsFile[i], PropsFile[i].IndexOf('=') +
          2, PropsFile[i].Length - PropsFile[i].IndexOf('=') + 1);
      end;
    end;
    opsiVersion := 'opsi 4.1';
    writeln(user, repoKind, opsiVersion);

    fileName := ExtractFilePath(ParamStr(0));
    Delete(fileName, Length(fileName), 1);
    fileName := ExtractFilePath(fileName) + 'l-opsi-server/CLIENT_DATA/properties.conf';
    PropsFile.SaveToFile(fileName);
    writeln(PropsFile.Text);

    InstallOpsi;
    DistrInfo.Free;
  end;

var
  QuickInstall: TQuickInstall;
  customLanguage, Lang, DefLang, userID: string;
  //r: TTranslateUnitResult;
begin
  QuickInstall := TQuickInstall.Create(nil);
  // only execute QuickInstall if user is root:
  if RunCommand('id -nu', user) then
  begin
    Delete(user, user.Length, 1);
    if RunCommand('id -u', userID) then
      Delete(userID, userID.Length, 1);
  end;
  // exit if not
  if not ((user = 'root') and (userID = '0')) then
  begin
    writeln('Please execute Opsi Quick-Install as root!');
    exit;
  end;

  // .../lazarus/common/oslog.pas
  // log file in /tmp/opsi_quickinstall.log
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile('opsi_quickinstall_nogui.log');
  // do language selection here only for nogui installation
  if QuickInstall.HasOption('n', 'nogui') then
  begin
    // get default language (system language)
    GetLanguageIDs(Lang, DefLang);
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      'locale/opsi_quick_install_nogui_project.%s.po', Lang, DefLang);

    writeln('');
    writeln(rsWelcome);
    writeln('');
    // language:
    writeln(rsSelLanguage, rsLangOp);
    readln(customLanguage);
    // check for right input
    while not ((customLanguage = 'de') or (customLanguage = 'en')) do
    begin
      writeln(customLanguage, rsNotValid);
      readln(customLanguage);
    end;
    writeln('');
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      'locale/opsi_quick_install_nogui_project.' + customLanguage + '.po');
  end;

  QuickInstall.Run;
  QuickInstall.Free;
end.
