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
  ///////////////////////////////////////////////////////////////////////////////
  procedure TMyApplication.NoGuiQuery;
  var
    input, setupType, distroName, distroRelease: string;
    //DistrInfo: TDistributionInfo;
    opsiVersion, repo, proxy, repoNoCache: string;
    backend, copyMod, repoKind: string;
    ucsPassword, reboot, dhcp, link: string;
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
    while not ((input = 's') or (input = 'c')) do
    begin
      writeln(input, rsNotValid);
      readln(input);
    end;
    if input = 's' then
      setupType := 's'
    else if input = 'c' then
      setupType := 'c';

    writeln(rsCarryOut);
    //Sleep(200);


    // distribution:
    {distroName := getLinuxDistroName;
    distroRelease := getLinuxDistroRelease;
    DistrInfo := TDistributionInfo.Create;}
    writeln(rsDistr, '...');
    writeln(rsIsCorrect, rsYesNoOp);
    readln(input);
    while not ((input = 'y') or (input = 'n')) do
    begin
      writeln(input, rsNotValid);
      readln(input);
    end;
    // if distribution isn't correct, read the correct one
    if input = 'n' then
    begin
      writeln(rsOtherDistr);
      readln(input);
      //define distribution
      //What to do with unknown distribution like ubuntu 20.04?
    end;


    if setupType = 'c' then
    begin
      // opsi version:
      writeln(rsOpsiVersion, rsOpsiVersionOp);
      readln(input);
      while not ((input = '4.1') or (input = '4.2')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      if input = '4.1' then
        opsiVersion := 'opsi 4.1'
      else if input = '4.2' then
        opsiVersion := 'opsi 4.2';

      // repo:
      writeln(rsRepo, ' [Example: ...]');
      readln(input);
      repo := input;

      // proxy:
      writeln(rsUseProxy, rsYesNoOp);
      readln(input);
      while not ((input = 'y') or (input = 'n')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      if input = 'y' then
      begin
        writeln('Which Proxy would you like to use? Example: "http://myproxy.dom.org:8080"');
        readln(input);
        proxy := input;
      end;

      // repo wihout cache proxy:
      writeln(rsRepo, ' [Example: ...]');
      readln(input);
      repoNoCache := input;


      // backend
      writeln(rsBackend, rsBackendOp);
      while not ((input = 'f') or (input = 'm')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      if input = 'm' then
      begin
        backend := 'mysql';
        // copy modules
        writeln(rsCopyModules);
        readln(input);
        while not ((input = 'y') or (input = 'n')) do
        begin
          writeln(input, rsNotValid);
          readln(input);
        end;
        if input = 'y' then
          copyMod := 'yes'
        else
          copyMod := 'no';
      end
      else
        backend := 'file';

      // repo kind
      writeln(rsRepoKind, rsRepoKindOp);
      while not ((input = 'e') or (input = 's') or (input = 't')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      if input = 's' then
        repoKind := 'stable'
      else if input = 't' then
        repoKind := 'testing'
      else if input = 'e' then
        repoKind := 'experimental';


      // ucs password
      //!!!if distribution = univention:
      writeln(rsUCS);
      readln(input);
      ucsPassword := input;

      // reboot
      writeln(rsReboot, rsYesNoOp);
      while not ((input = 'y') or (input = 'n')) do
      begin
        writeln(input, rsNotValid);
        readln(input);
      end;
      if input = 'y' then
          reboot := 'yes'
        else
          reboot := 'no';

    end;
    // dhcp
    // link
    // netmask
    // network address
    // domain
    // nameserver
    // gateway

    // user name

    // user password

    // IP name

    // IP number
  end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.
