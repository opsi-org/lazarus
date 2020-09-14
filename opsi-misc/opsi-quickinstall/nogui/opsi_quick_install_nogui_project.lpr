program opsi_quick_install_nogui_project;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Process, {LCLtranslator,}
  { you can add units after this }
  opsi_quick_install_resourcestrings {,
  osDistributionInfo,
  osfunclin, osLinuxRepository};

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
  end;

  { TMyApplication }

  procedure TMyApplication.DoRun;
  var
    ErrorMsg, project: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('htgn', 'help test gui nogui');
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
    //DistrInfo: TDistributionInfo;
    opsiVersion, repo, proxy, repoNoCache: string;
    backend, copyMod, repoKind: string;
    ucsPassword, reboot, dhcp, link: string;
    netmask, networkAddress, domain, nameserver, gateway: string;
    adminName, adminPassword, ipName, ipNumber: string;
  begin
    {LogDatei := TLogInfo.Create;
    LogDatei.CreateTheLogfile('opsi_quickinstall_nogui.log');}

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

    writeln(rsCarryOut);
    //Sleep(200);


    // distribution:
    {distroName := getLinuxDistroName;
    distroRelease := getLinuxDistroRelease;
    DistrInfo := TDistributionInfo.Create;}
    writeln(rsDistr, '...');
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
      //!!!define distribution
      //What to do with unknown distribution like ubuntu 20.04?
    end;


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
      //!!!if distribution = univention:
      writeln(rsUCS);
      readln(input);
      ucsPassword := input;

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
      writeln(rsDomain, rsDomainOp);
      readln(input);
      while not ((input = 'ucs.test') or (input = 'uib.local') or
          (input = 'vmnat.local')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
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
  end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.
