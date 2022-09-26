unit PasswordFormResourceStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


resourcestring

  {universal}
  rsBack = ' < back ';
  rsFinish = ' finish ';

  {Password}
  rsRights = 'Authentication is required for the installation. Are you identified as root or per sudo?';
  rsPassword = 'Password';
  rsShowPassword = 'Show password';
  rsWrongPassword = 'Authentication failed.' + #10 + 'Please check your password!';

implementation

end.
