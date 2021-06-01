unit opsi_quick_install_resourcestrings;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF GUI}
  {$ENDIF GUI}
  Classes, SysUtils;

resourcestring
  {nogui}
  rsNotValid = ' is not a valid input.';
  rsLangOp =
    ' [Options: "de" for Deutsch, "en" for English, "fr" for Français, "es" for Español]';
  rsSetupOp = ' [Options: "s" for standard, "c" for custom]';
  rsIsCorrect = 'Is this correct?';
  rsYesNoOp = ' [Options: "y" for yes, "n" for no]';
  rsOtherDistr = 'Please type in the correct distribution:';
  rsOpsiVersionOp = ' [Options: "1" for Opsi 4.1, "2" for Opsi 4.2]';
  rsUseProxy = 'Would you like to use a proxy?';
  rsBackendOp = ' [Options: "f" for file, "m" for mysql]';
  rsRepoKindOp = ' [Options: "s" for stable, "t" for testing, "e" for experimental]';
  rsLinkOp = ' [Options: "m" for default.menu, "nom" for default.nomenu]';
  rsSuggestion = '[Suggestions: ';
  rsContinue = 'To continue and start the installation, please press enter.' +
    #10 + 'If you like to jump back to a question, please press the number key of the respective number in the overview.';
  rsInstall = 'Installing ';
  rsCreateRepo = 'Creating repository...';
  rsSomeMin = '(This may take some minutes)';

  {universal}
  rsYes = 'Yes';
  rsNo = 'No';
  rsNext = ' next > ';
  rsBack = ' < back ';
  rsOverview = 'Overview';
  rsFinish = ' finish ';

  // QuickInstall:
  rsCapDistr = 'Distribution';
  rsCapQuery = 'Query';
  rsCapQuery2 = 'Query (part 2)';
  rsCapQuery3 = 'Query (part 3)';
  rsCapQueryDhcp = 'Query (dhcp)';
  rsCapQueryUserInfo = 'Query (user info)';

  {Language}
  rsWelcome = 'Welcome to the installation of opsi-server on this computer!';
  rsSelLanguage = 'Please choose a language for this setup program:';
  rsSetup = 'Please select a setup type:';
  rsStandard = 'Standard';
  rsCustom = 'Custom';
  rsCarryOut =
    'To carry out the remaining installation automatically, we need some information first.';

  {Distribution}
  rsDistr = 'We recognized the following distribution on this computer:';
  rsInfoDistribution = 'The following distributions are supported:';
  rsCorrect =
    'If this is correct, please click "next >" to continue. If not, please edit the distribution and then continue with "next >".';
  rsNoSupport =
    'Unfortunately, opsi does not support your distribution. Only the following distributions are supported:';

  {Query}
  rsOpsiVersion = 'Which opsi version would you like to install?';
  rsOpsi41 = 'Opsi 4.1';
  rsOpsi42 = 'Opsi 4.2';
  rsInfoOpsiVersion =
    'It''s recommended to install opsi 4.2 since it is the latest version; the support for opsi 4.1 will end soon.';
  rsRepo = '(Base-) Repository for the opsi-server installation:';
  rsRepoOther = 'Other:';
  rsInfoRepo = 'Source from which opsi will be installed.';
  rsProxy = 'Proxy to use (http://myproxy.dom.org:8080):';
  rsProxyNone = 'None';
  rsProxyOther = 'Other:';
  rsRepoNoCache =
    '(Base-) Repository for the opsi-server installation (without any cache proxy):';

  {Query2}
  rsBackend = 'Which backend should be installed? (mysql needs a valid activation file)';
  rsInfoBackend = 'Should the data of opsi be stored in a file or a mysql database?';
  rsCopyModules = 'Should we copy modules even if /etc/opsi/modules still exists?';
  rsInfoModules =
    'For mysql you need active modules. For further information and prices please write an e-mail to info@uib.de';
  rsRepoKind = 'From which repo kind should be installed?';
  // #10 does a linebreak :D
  rsInfoRepoKind = 'Select the stability of the installation:' + #10 +
    'Stable is reliable, testing is the beta version, experimental may cause some errors.';
  //rsUpdate = 'Install first from stable and than update to repo kind?';

  {Query4}
  rsUCS =
    'What is the password of the administrator of the UCS domain controller (needed for other roles to join)?';
  rsReboot = 'May the server reboot if this script is finished?';
  rsInfoReboot =
    'If yes, shortly after the opsi-server is installed, your computer will reboot.';
  rsDhcp = 'Should we run the opsi dhcp-server on this machine?';
  rsInfoDhcp = 'Select "No" if you already have a dhcp-server.' +
    #10 + 'Select "Yes" if you want to install the dhcp-server on the opsi-server.';
  rsTFTPROOT = 'Which file should the TFTPROOT default symlink point to?';
  rsInfoTFTPROOT =
    'Choose "default.menu" if you want to show a small menu when no netboot product is on setup (bios only).';

  {Query5_dhcp}
  rsInfoNetwork = 'Information on the network in which the opsi-server will be installed.';
  rsNetmask = 'Netmask (for dhcp):';
  rsNetmaskOther = 'Other:';
  rsNetworkAddress = 'Network address (for dhcp):';
  rsNetworkAddressOther = 'Other:';
  rsDomain = 'DNS domain (for dhcp):';
  rsDomainOther = 'Other:';
  rsNameserver = 'Primary nameserver (for dhcp):';
  rsNameserverOther = 'Other:';
  rsGateway = 'Gateway (option routers for dhcp):';
  rsGatewayOther = 'Other:';

  {Query6}
  rsAdminName = 'What is the name of the opsi admin user to create? (empty = nothing created)';
  rsInfoAdmin = 'User to create for the operation of opsi.' + #10 +
    'This user can not manage system settings.';
  rsAdminPassword =
    'What is the password of the opsi admin user to create? (empty = nothing created)';
  rsIPName =
    'Set a different IP name ("auto" = use automatically determined FQDN of this computer)';
  rsIPNumber =
    'Set a different IP number ("auto" = use automatically determined IP address of this computer)';
  rsOverviewBtn = ' overview ';

  {Overview}
  rsOpsiVersionO = 'Opsi version to install: ';
  rsRepoO = 'Repository: ';
  rsProxyO = 'Proxy: ';
  rsRepoNoCacheO = 'Repository (without cache proxy): ';
  rsBackendO = 'Backend: ';
  rsCopyModulesO = 'Copy modules: ';
  rsRepoKindO = 'Repo kind: ';
  //rsUpdateO = 'Install from stable, update to repo kind: ';
  //rsProds = 'Products in depot: ';
  rsUCSO = 'Password of administrator of UCS domain controller: ';
  rsRebootO = 'Reboot after script is finished: ';
  rsDhcpO = 'Run opsi dhcp-server: ';
  rsTFTPROOTO = 'TFTPROOT symlink points to: ';
  rsNetmaskO = 'Netmask: ';
  rsNetworkO = 'Network address: ';
  rsDomainO = 'DNS domain: ';
  rsNameserverO = 'Primary nameserver: ';
  rsGatewayO = 'Gateway: ';
  rsAdminNameO = 'Opsi admin user name: ';
  rsAdminPasswordO = 'Opsi admin user password: ';
  rsIPNameO = 'IP name: ';
  rsIPNumberO = 'IP number: ';

  rsStartInstallation =
    'Finally click "finish" to start the installation of opsi-server.';

  {Password}
  rsRights = 'Authentication is required to install opsi-server. Who are you logged in as?';
  rsPassword = 'Password';
  rsShowPassword = 'Show password';
  rsWait = 'Please wait for the installation to start...';
  rsLog = 'You can find the log files here:';
  rsWrongPassword = 'Authentication failed. Please check your password!';

const
  LogOpsiServer = '/var/log/opsi-quick-install-l-opsi-server.log';

implementation

end.
