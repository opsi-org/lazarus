unit opsi_configed_installer_unit_wait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TWait }

  TWait = class(TForm)
    LabelWait: TLabel;
    procedure FormActivate(Sender: TObject);
  end;

var
  Wait: TWait;

implementation

uses
  opsi_configed_installer_unit_language,
  opsi_configed_installer_resourcestrings;

{$R *.lfm}

{ TWait }

procedure TWait.FormActivate(Sender: TObject);
begin
  Left := ConfigedInstaller.Left + Round(ConfigedInstaller.Width / 2) - Round(Width / 2);
  Top := ConfigedInstaller.Top + Round(ConfigedInstaller.Height / 2) - Round(Height / 2);
  // text by resourcestrings
  LabelWait.Caption := rsWait + #10 + ' ';
end;

end.

