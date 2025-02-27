unit WelcomeResourceStrings;

(*
    The resourcestrings for the welcoming have their own file here so that they can not only be used in the GUI
    but also in a NOGUI version of the installer.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


resourcestring

  rsWelcome = 'Welcome to the installation of the [] on this computer!';
  rsSelLanguage = 'Please choose a language for this setup program:';
  rsCarryOut =
    'To carry out the remaining installation automatically, we need some information first.';
  rsNext = ' Next ';

implementation

end.
