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

  {Query}
  rsMem = 'Memory requirement for the operation of the opsi-configed' + #10 + '(in megabytes)';
  rsScalingFactor = 'Scaling factor for Java Swing App' + #10 + '(working with Java version >= 10)';

  {Password}
  rsRights = 'Authentication is required to install the opsi-configed. Do you identify as root or per sudo?';
  rsPassword = 'Password';
  rsShowPassword = 'Show password';
  rsWait = 'Please wait for the installation to start...';
  rsLog = 'You can find the log files here:';
  rsWrongPassword = 'Authentication failed.' + #10 + 'Please check your password!';

const
  LogOpsiServer = '/var/log/opsi-configed-installer-configed.log';

implementation

end.
