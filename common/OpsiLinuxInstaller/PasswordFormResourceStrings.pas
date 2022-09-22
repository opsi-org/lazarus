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
  rsRights = 'Authentication is required to install opsi-server. Who are you logged in as?';
  rsPassword = 'Password';
  rsShowPassword = 'Show password';
  rsWrongPassword = 'Authentication failed.' + #10 + 'Please check your password!';

implementation

end.
