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
  rsLangOp = ' [Options: "de" for german, "en" for english, ...]';
  rsWrongDistr = '';
  rsSetupOp = ' [Options: "Standard", "Custom"]';
  rsIsCorrect = 'Is this correct?';
  rsYesNoOp = ' [Options: "Yes", "No"]';
  rsOtherDistr = 'Please type in the correct distribution:';
  rsOpsiVersionOp = ' [Options: "Opsi 4.1", "Opsi 4.2"]';
  rsUseProxy = 'Would you like to use a proxy?';
  rsBackendOp = ' [Options: "file", "mysql"]';
  rsRepoKindOp = ' [Options: "stable", "testing", "experimental"]';
  rsLinkOp = ' [Options: "default.menu", "default.nomenu"]';
  rsNetmaskEx = ' [Examples: "225.225.0.0", "225.225.225.0"]';
  rsNetworkAddressEx = ' [Examples: "10.100.0.0", "172.16.166.0", "192.168.0.0"]';
  rsDomainEx = ' [Examples: "uib.local", "ucs.test", "vmnat.local"]';
  rsNameserverEx = ' [Examples: "10.100.1.2", "172.16.166.1", "192.168.1.245"]';
  rsGatewayEx = ' [Examples: "10.100.1.2", "172.16.166.1", "192.168.1.245"]';

  {universal}
  rsYes = 'Yes';
  rsNo = 'No';
  rsNext = ' next > ';
  rsBack = ' < back ';
  rsOverview = 'Overview';
  rsFinish = ' finish ';

  {QuickInstall}
  rsWelcome = 'Welcome to the installation of opsi-server on this computer!';
  rsSelLanguage = 'Please choose a language for this setup program:';
  rsSetup = 'Please select a setup type:';
  rsStandard = 'Standard';
  rsCustom = 'Custom';
  rsCarryOut =
    'To carry out the remaining installation automatically, we need some information first.';

  {Distribution}
  rsDistr = 'We recognized the following distribution on this computer:';
  rsCorrect =
    'If this is correct, please click "next >" to continue. If not, please edit the distribution and then continue with "next >".';
  rsNoSupport =
    'Unfortunately, Opsi does not support your distribution. Only the following distributions are supported:';

  {Query}
  rsOpsiVersion = 'Which opsi version would you like to install?';
  rsOpsi41 = 'Opsi 4.1';
  rsOpsi42 = 'Opsi 4.2';
  rsInfoOpsiVersion = 'Opsi 4.2 is the latest version...';
  rsRepo = '(Base-) Repository for opsi-server installation:';
  rsRepoOther = 'Other:';
  rsInfoRepo = 'Source from which opsi will be installed.';
  rsProxy = 'Proxy to use (http://myproxy.dom.org:8080):';
  rsProxyNone = 'None';
  rsProxyOther = 'Other:';
  rsRepoNoCache =
    '(Base-) Repository for opsi-server installation (without any cache proxy):';

  {Query2}
  rsBackend = 'Which backend should be installed? (mysql needs valid activation file)';
  rsInfoBackend = 'Shall the data of opsi be stored in a file or a mysql database?';
  rsCopyModules = 'Should we copy modules even if /etc/opsi/modules still exists?';
  rsInfoModules =
    'For mysql you need active modules. For infos and prices please write an e-mail to info@uib.de';
  rsRepoKind = 'From which repo kind should be installed?';
  // #10 does a linebreak :D
  rsInfoRepoKind = 'Select the stability of the installation:' + #10 +
    'Experimental...; testing is the beta version; stable...';
  //rsUpdate = 'Install first from stable and than update to repo kind?';

  {Query4}
  rsQuery = 'Opsi Quick Install - Query';
  rsQuery3 = 'Opsi Quick Install - Query (part 3)';
  rsUCS =
    'What is the Password of the user Administrator of the UCS Domain Controller (needed for other roles to join)?';
  rsReboot = 'May the server reboot if the script is finished?';
  rsInfoReboot =
    'If true, shortly after thei script is finished and l-opsi-server is installed, your computer will reboot (and you can see the newly created user).';
  rsDhcp = 'Should we run the opsi dhcp server on this machine?';
  rsInfoDhcp = 'Select "No" if you already have a dhcp server.' +
    #10 + 'Select "Yes" if you want to install the dhcp server on the opsi server.';
  rsTFTPROOT = 'Which file should the TFTPROOT default symlink point to?';
  rsInfoTFTPROOT =
    'Choose "default.menu" if you want to show a small menu when no netboot product is on setup (bios only).';

  {Query5_dhcp}
  rsInfoNetwork = 'Information on the network in which the opsi server shall be installed.';
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
  rsAdminName = 'What is the name of the opsi_admin_user to create? (empty = nothing created)';
  rsInfoAdmin = 'User to create for the operation of opsi.' + #10 +
    'This user can not manage system settings.';
  rsAdminPassword =
    'What is the password of the opsi_admin_user to create? (empty = nothing created)';
  rsIPName = 'Set a different IP name (FQDN) ("auto" = use standard)';
  rsIPNumber = 'Set a different IP number ("auto" = use standard)';
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
  rsDhcpO = 'Run opsi dhcp server: ';
  rsTFTPROOTO = 'TFTPROOT symlink points to: ';
  rsNetmaskO = 'Netmask: ';
  rsNetworkO = 'Network address: ';
  rsDomainO = 'DNS Domain: ';
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
  rsPassword = 'Password:';
  rsShowPassword = 'Show password';
  rsWait = 'Please wait for the installation to start...'+ #10 + '';
  rsSomeMin= 'This may take some minutes...' + #10;
  rsLog = 'You can find the log files here:';

  const
  LogQuickInstall = '/tmp/opsi_quickinstall.log';
  LogOpsiServer = '/var/log/opsi_quick-install-l-opsi-server.log';

implementation

end.
