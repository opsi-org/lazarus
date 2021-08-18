unit opsi_configed_installer_resourcestrings;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF GUI}
  {$ENDIF GUI}
  Classes, SysUtils;

resourcestring
  {nogui}
  rsCreateRepo = 'Creating repository...';

  {universal}
  rsYes = 'Yes';
  rsNo = 'No';
  rsNext = ' next > ';
  rsBack = ' < back ';
  rsOverview = 'Overview';
  rsFinish = ' finish ';

  // ConfigedInstaller:
  rsCapDistr = 'Distribution';
  rsCapQuery = 'Query';
  rsCapQuery2 = 'Query (part 2)';
  rsCapQuery3 = 'Query (part 3)';
  rsCapQueryDhcp = 'Query (dhcp)';
  rsCapQueryUserInfo = 'Query (user info)';

  {Language}
  rsWelcome = 'Welcome to the installation of the opsi-configed on this computer!';
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

  rsStartInstallation =
    'Finally click "finish" to start the installation of opsi-server.';

  {Password}
  rsRights = 'Authentication is required to install opsi-server. Who are you logged in as?';
  rsPassword = 'Password';
  rsShowPassword = 'Show password';
  rsWait = 'Please wait for the installation to start...';
  rsLog = 'You can find the log files here:';
  rsWrongPassword = 'Authentication failed.' + #10 + 'Please check your password!';

const
  LogOpsiServer = '/var/log/opsi-configed-installer-configed.log';

implementation

end.
