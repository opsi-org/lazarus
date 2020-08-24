unit opsi_quick_install_resourcestrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  {universal}
  rsYes = 'Yes';
  rsNo = 'No';
  rsNext = ' next > ';
  rsBack = ' < back ';
  rsOverview = 'overview';
  rsFinish = ' finish ';

  {QuickInstall}
  rsWelcome = 'Welcome to the installation of opsi-server on this computer!';
  rsSelLanguage = 'Please choose a language for this setup program:';
  rsSetup = 'Please select a setup type:';
  rsStandard = 'Standard';
  rsCustom = 'Custom';
  rsCarryOut =
    'To carry out the remaing installation automatically, we need some information first.';

  {Distribution}
  rsDistr = 'We recognized the following distribution on this computer:';
  rsCorrect =
    'If this is correct, please click ''next >'' to continue. If not, please edit the distribution and then continue with ''next >''.';

  {Query}
  rsOpsiVersion = 'Which opsi version would you like to install?';
  rsOpsi41 = 'opsi 4.1';
  rsOpsi42 = 'opsi 4.2';
  rsRepo = '(Base-) Repository for opsi-server installation';
  rsRepoOther = 'Other:';
  rsProxy = 'Proxy to use (http://myproxy.dom.org:8080)';
  rsProxyNone = 'None';
  rsProxyOther = 'Other:';
  rsRepoNoCache = '(Base-) Repository for opsi-server installation (without any cache proxy)';

  {Query2}
  rsBackend = 'Which backend should be installed? (mysql needs valid activation file)';
  rsCopyModules = 'Should we copy modules even if /etc/opsi/modules still exists?';
  rsRepoKind = 'From which repo kind should be installed?';
  rsUpdate = 'Install first from stable and than update to repo kind?';

  {Query4}
  rsUCS = 'What is the Password of the user Administrator of the UCS Domain Controller (needed for other roles to join)?';
  rsReboot = 'May the server reboot if the script is finished?';
  rsDhcp = 'Should we run the opsi dhcp server on this machine?';
  rsTFTPROOT = 'Which file should the TFTPROOT default symlink point to?';

  {Query5_dhcp}
  rsNetmask = 'Netmask (for dhcp)';
  rsNetmaskOther = 'Other:';
  rsNetworkAddress = 'Network address (for dhcp)';
  rsNetworkAddressOther = 'Other:';
  rsDomain = 'DNS domain (for dhcp)';
  rsDomainOther = 'Other:';
  rsNameserver = 'Primary nameserver (for dhcp)';
  rsNameserverOther = 'Other:';
  rsGateway = 'Gateway (option routers for dhcp)';
  rsGatewayOther = 'Other:';

  {Query6}
  rsAdminName = 'What is the name of the opsi_admin_user to create? (empty = nothing created)';
  rsAdminPassword = 'What is the password of the opsi_admin_user to create? (empty = nothing created)';
  rsIPName = 'Set a different IP name (FQDN) (''auto'' = use standard)';
  rsIPNumber = 'Set a different IP number (''auto'' = use standard)';

  {Overview}
  rsOpsiVersionO = 'Opsi version to install: ';
  rsRepoO = 'Repository: ';
  rsProxyO = 'Proxy: ';
  rsRepoNoCacheO = 'Repository (without cache proxy): ';
  rsBackendO = 'Backend: ';
  rsCopyModulesO = 'Copy modules: ';
  rsRepoKindO = 'Repo kind: ';
  rsUpdateO = 'Install from stable, update to repo kind: ';
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

  rsStartInstallation = 'Finally click ''finish'' to start the installation of opsi-server.';

  {Password}
  rsRights = 'Authentication is required to install opsi-server. Who are you logged in as?';
  rsPassword = 'Password:';
  rsShowPassword = 'Show password';


implementation

end.
