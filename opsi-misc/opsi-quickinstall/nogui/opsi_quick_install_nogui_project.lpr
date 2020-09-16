program opsi_quick_install_nogui_project;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Process, {LCLtranslator,}
  { you can add units after this }
  opsi_quick_install_resourcestrings,
  osDistributionInfo,
  osRunCommandElevated,
  osfunclin,
  osLinuxRepository,
  oslog;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure NoGuiQuery;
    procedure ExecuteWithDefaultValues;
  end;

  { TMyApplication }

  procedure TMyApplication.DoRun;
  var
    ErrorMsg, project: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('htgnd', 'help test gui nogui default');
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
      writeln('test');
      //Terminate;
      //Exit;
    end;

    if HasOption('g', 'gui') then
    begin
      project := ExtractFilePath(ParamStr(0));
      Delete(project, Length(project), 1);
      project := ExtractFilePath(project) + 'gui/opsi_quick_install_project';
      writeln(project);
      ExecuteProcess(project, '', []);
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

    { add your program here }
    writeln('Huhu?');
    // stop program loop
    Terminate;
    Exit;
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
    writeln('Help please');
    writeln('Write:');
    readln;
    writeln('Exit');
  end;
  /////////////////////////////////////////////////////////////////////////////
  procedure TMyApplication.NoGuiQuery;
  var
    input, setupType, distroName, distroRelease: string;
    DistrInfo: TDistributionInfo;
    opsiVersion, repo, proxy, repoNoCache: string;
    backend, copyMod, repoKind: string;
    ucsPassword, reboot, dhcp, link: string;
    netmask, networkAddress, domain, nameserver, gateway: string;
    adminName, adminPassword, ipName, ipNumber: string;
    user, password: string;
    FileText: TStringList;
    MyRepo: TLinuxRepository;
    RunCommand: TRunCommandElevated;
    fileName, url, shellCommand, Output: string;
  begin
    // set default values:
    opsiVersion := 'opsi 4.1';
    repo := 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    proxy := '';
    repoNoCache := repo;
    backend := 'file';
    // ...ToDo

    LogDatei := TLogInfo.Create;
    LogDatei.CreateTheLogfile('opsi_quickinstall_nogui.log');

    //writeln(GetDefaultLang);
    writeln(rsWelcome);
    //Sleep(2000);

    // language:
    writeln(rsSelLanguage, rsLangOp);
    readln(input);
    // check for right input
    while not ((input = 'de') or (input = 'en')) do
    begin
      writeln(input, rsNotValid);
      readln(input);
    end;
    {if input = 'de' then
      SetDefaultLang('de')
    else if input = 'en' then
      SetDefaultLang('en');}

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
    //Sleep(200);
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
      writeln(rsRepo, ' [Example: ...]');
      readln(input);
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
      writeln(rsRepo, ' [Example: ...]');
      readln(input);
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
      writeln(rsNetmask, ' [Examples: "225.225.0.0", "225.225.225.0"]');
      readln(input);
      netmask := input;
      // network address
      writeln(rsNetworkAddress,
        ' [Examples: "10.100.0.0", "172.16.166.0", "192.168.0.0"]');
      readln(input);
      networkAddress := input;
      // domain
      writeln(rsDomain, rsDomainEx);
      readln(input);
      domain := input;
      // nameserver
      writeln(rsNameserver,
        ' [Examples: "10.100.1.2", "172.16.166.1", "192.168.1.245"]');
      readln(input);
      nameserver := input;
      // gateway
      writeln(rsGateway, ' [Examples: "10.100.1.2", "172.16.166.1", "192.168.1.245"]');
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


    // authentication
    writeln(rsRights, rsRightsOp);
    readln(input);
    while not ((input = 'root') or (input = 'sudo')) do
    begin
      writeln(input, rsNotValid);
      readln(input);
    end;
    user := input;
    writeln(rsPassword);
    readln(input);
    password := input;


    // write user input in l-opsi-server.conf and properties.conf file:
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
    //writeln(FileText.Text);

    // write in l-opsi-server.conf file:
    fileName := ExtractFilePath(ParamStr(0));
    //writeln(fileName);
    FileText.SaveToFile(fileName + 'l-opsi-server.conf');
    // write in properties.conf file:
    // navigate to CLIENT_DATA in l-opsi-server
    Delete(fileName, Length(fileName), 1);
    fileName := ExtractFilePath(fileName) + 'l-opsi-server/CLIENT_DATA/';
    //writeln(fileName);
    FileText.SaveToFile(fileName + 'properties.conf');

    // Important for getting the result 'failed' in case of a wrong password...
    // ...cause in this case the RunCommands below aren't executed...
    // ...and therefore setup.opsiscript, that usually does it, isn't too:
    FileText.Clear;
    FileText.Add('failed');
    FileText.SaveToFile(fileName + 'result.conf');

    // create repository
    MyRepo := TLinuxRepository.Create(DistrInfo.MyDistr, password, user = 'sudo');
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
    RunCommand := TRunCommandElevated.Create(password, user = 'sudo');
    shellCommand := DistrInfo.GetPackageManagementShellCommand(distroName);
    Output := RunCommand.Run(shellCommand + 'update');
    //writeln(Output);
    Output := RunCommand.Run(shellCommand + 'install opsi-script');
    //writeln(Output);
    Output := RunCommand.Run('opsi-script -batch ' + fileName +
      'setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log');

    FileText.LoadFromFile(fileName + 'result.conf');
    writeln(FileText.Text);

    FileText.Free;
    DistrInfo.Free;
    RunCommand.Free;
    MyRepo.Free;
  end;


  //////////////////////////////////////////////////////////////////////////////
  procedure TMyApplication.ExecuteWithDefaultValues;
  var
    user, password: string;
    DistrInfo: TDistributionInfo;
    FileText: TStringList;
    MyRepo: TLinuxRepository;
    RunCommand: TRunCommandElevated;
    fileName, url, shellCommand, Output: string;
  begin
    LogDatei := TLogInfo.Create;
    LogDatei.CreateTheLogfile('opsi_quickinstall_nogui.log');

    DistrInfo := TDistributionInfo.Create;
    DistrInfo.SetInfo('Ubuntu', '18.04');
    user := 'sudo';
    password := 'linux123';

    // write user input in l-opsi-server.conf and properties.conf file:
    FileText := TStringList.Create;
    FileText.Add('allow_reboot=false');
    FileText.Add('backend=file');
    FileText.Add('dnsdomain=uib.local');
    FileText.Add('force_copy_modules=false');
    FileText.Add('gateway=192.168.1.245');
    FileText.Add('install_and_configure_dhcp=false');
    FileText.Add('myipname=auto');
    FileText.Add('myipnumber=auto');
    FileText.Add('nameserver=192.168.1.245');
    FileText.Add('netmask=255.255.0.0');
    FileText.Add('network=192.168.0.0');
    FileText.Add('opsi_admin_user_name=Anne');
    FileText.Add('opsi_admin_user_password=linux123');
    FileText.Add(
      'opsi_online_repository=http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/');
    FileText.Add(
      'opsi_noproxy_online_repository=http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/');
    FileText.Add('patch_default_link_for_bootimage=default.nomenu');
    FileText.Add('proxy=');
    FileText.Add('repo_kind=stable');
    FileText.Add('ucs_master_admin_password=linux123');
    // update_test shall always be false
    FileText.Add('update_test=false');
    //writeln(FileText.Text);

    // write in l-opsi-server.conf file:
    fileName := ExtractFilePath(ParamStr(0));
    //writeln(fileName);
    FileText.SaveToFile(fileName + 'l-opsi-server.conf');
    // write in properties.conf file:
    // navigate to CLIENT_DATA in l-opsi-server
    Delete(fileName, Length(fileName), 1);
    fileName := ExtractFilePath(fileName) + 'l-opsi-server/CLIENT_DATA/';
    //writeln(fileName);
    FileText.SaveToFile(fileName + 'properties.conf');

    // Important for getting the result 'failed' in case of a wrong password...
    // ...cause in this case the RunCommands below aren't executed...
    // ...and therefore setup.opsiscript, that usually does it, isn't too:
    FileText.Clear;
    FileText.Add('failed');
    FileText.SaveToFile(fileName + 'result.conf');

    //writeln(user = 'sudo');
    //writeln(LowerCase((user = 'sudo').ToString(TUseBoolStrs.True)));

    // create repository
    MyRepo := TLinuxRepository.Create(DistrInfo.MyDistr, password, user = 'sudo');
    // Set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
    url := MyRepo.GetDefaultURL(Opsi41, stable);
    //writeln(url);
    // following two lines need an existing LogDatei
    MyRepo.Add(url);
    MyRepo.Add(MyRepo.GetDefaultURL(Opsi41, stable));

    RunCommand := TRunCommandElevated.Create(password, user = 'sudo');
    shellCommand := DistrInfo.GetPackageManagementShellCommand('Ubuntu');
    //writeln(shellCommand);
    // following lines need an existing LogDatei
    Output := RunCommand.Run(shellCommand + 'update');
    //writeln(Output);
    Output := RunCommand.Run(shellCommand + 'install opsi-script');
    //writeln(Output);
    Output := RunCommand.Run('opsi-script -batch ' + fileName +
      'setup.opsiscript  /var/log/opsi-quick-install-l-opsi-server.log');

    FileText.LoadFromFile(fileName + 'result.conf');
    writeln(FileText.Text);

    FileText.Free;
    DistrInfo.Free;
    RunCommand.Free;
    MyRepo.Free;
  end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
