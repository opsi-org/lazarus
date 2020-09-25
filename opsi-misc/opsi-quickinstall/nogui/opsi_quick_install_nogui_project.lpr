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
    opsiVersion := 'opsi 4.1';
    // repo depending on opsi version
    if opsiVersion = 'opsi 4.1' then
      repo := baseUrlOpsi41
    else
      repo := baseUrlOpsi42;
    proxy := '';
    repoNoCache := repo;
    backend := 'file';
    copyMod := 'no';
    repoKind := 'stable';
    ucsPassword := '';
    reboot := 'false';
    dhcp := 'no';
    link := 'default.nomenu';
    netmask := '255.255.0.0';
    networkAddress := '192.168.0.0';
    domain := 'uib.local';
    nameserver := '192.168.1.245';
    gateway := '192.168.1.245';
    adminName := 'Amina';
    adminPassword := 'linux123';
    ipName := 'auto';
    ipNumber := 'auto';
  end;

  // write properties in l-opsi-server.conf and properties.conf file
  procedure TQuickInstall.WritePropsToFile;
  begin
    // write file text
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
    FileText.SaveToFile(ExtractFilePath(ParamStr(0)) + 'l-opsi-server.conf');
    // write in properties.conf file:
    FileText.SaveToFile(DirClientData + 'properties.conf');
    FileText.Free;
  end;

  // install opsi-server
  // requires: opsiVersion, repoKind, distroName, DistrInfo, existing LogDatei
  procedure TQuickInstall.InstallOpsi;
  begin
    // Set text of result.conf to 'failed' first (for safety)
    FileText := TStringList.Create;
    FileText.Add('failed');
    FileText.SaveToFile(DirClientData + 'result.conf');

    // create repository (no password, user is root)
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
    writeln('');

    // setup type:
    writeln(rsSetup, rsSetupOp);
    readln(input);
    while not ((input = 'standard') or (input = 'custom')) do
    begin
      writeln('"', input, '"', rsNotValid);
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
      writeln('"', input, '"', rsNotValid);
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

    // following queries only for custom setup
    if setupType = 'custom' then
    begin
      // opsi version:
      writeln(rsOpsiVersion, rsOpsiVersionOp);
      readln(input);
      while not ((input = 'opsi 4.1') or (input = 'opsi 4.2')) do
      begin
        writeln('"', input, '"', rsNotValid);
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
        writeln('"', input, '"', rsNotValid);
        readln(input);
      end;
      repo := input;
      // proxy:
      writeln(rsUseProxy, rsYesNoOp);
      readln(input);
      while not ((input = 'yes') or (input = 'no')) do
      begin
        writeln('"', input, '"', rsNotValid);
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
        writeln('"', input, '"', rsNotValid);
        readln(input);
      end;
      repoNoCache := input;

      // backend
      writeln(rsBackend, rsBackendOp);
      readln(input);
      while not ((input = 'file') or (input = 'mysql')) do
      begin
        writeln('"', input, '"', rsNotValid);
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
          writeln('"', input, '"', rsNotValid);
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
        writeln('"', input, '"', rsNotValid);
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
        writeln('"', input, '"', rsNotValid);
        readln(input);
      end;
      reboot := input;
    end;
    // dhcp
    writeln(rsDhcp, rsYesNoOp);
    readln(input);
    while not ((input = 'yes') or (input = 'no')) do
    begin
      writeln('"', input, '"', rsNotValid);
      readln(input);
    end;
    dhcp := input;
    // following queries only for dhcp
    if dhcp = 'yes' then
    begin
      // link
      writeln(rsTFTPROOT, rsLinkOp);
      readln(input);
      while not ((input = 'default.menu') or (input = 'default.nomenu')) do
      begin
        writeln('"', input, '"', rsNotValid);
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

    // Overview

    InstallOpsi;
    DistrInfo.Free;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // install opsi server with the default values from SetDefaultValues
  procedure TQuickInstall.ExecuteWithDefaultValues;
  begin
    SetDefaultValues;
    DistrInfo := TDistributionInfo.Create;
    DistrInfo.SetInfo(getLinuxDistroName, getLinuxDistroRelease);
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
    DistrInfo := TDistributionInfo.Create;
    DistrInfo.SetInfo(distroName, distroRelease);

    // Read from file what is required
    for i := 0 to PropsFile.Count-1 do
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
          opsiVersion := 'opsi 4.1'
        else
          opsiVersion := 'opsi 4.2';
      end;
    end;

    // Take text of PropsFile as text for properties.conf
    PropsFile.SaveToFile(DirClientData + 'properties.conf');

    InstallOpsi;
    DistrInfo.Free;
  end;

var
  QuickInstall: TQuickInstall;
  user, userID, customLanguage, Lang, DefLang: string;
  //r: TTranslateUnitResult;
begin
  // only execute QuickInstall if user is root:
  if (RunCommand('id -nu', user) and RunCommand('id -u', userID)) then
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

  //writeln(LowerCase((user = 'sudo').ToString(TUseBoolStrs.True)));

  // .../lazarus/common/oslog.pas
  // log file in /tmp/opsi_quickinstall.log
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile('opsi_quickinstall_nogui.log');
  QuickInstall := TQuickInstall.Create(nil);
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
      writeln('"', customLanguage, '"', rsNotValid);
      readln(customLanguage);
    end;
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      'locale/opsi_quick_install_nogui_project.' + customLanguage + '.po');
  end;
  QuickInstall.Run;
  QuickInstall.Free;
  LogDatei.Free;
end.
