unit helpinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, proginfo,
  lcltranslator;

type

  { TFormHelpInfo }

  TFormHelpInfo = class(TForm)
    MemoInfo: TMemo;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormHelpInfo: TFormHelpInfo;

implementation

{$R *.lfm}

{ TFormHelpInfo }

procedure TFormHelpInfo.FormCreate(Sender: TObject);
begin
  MemoInfo.Lines.Add(
  'Documentation: www.opsi.org' + LineEnding +
  '-----------------------------------' + LineEnding +
  'opsi-client-kiosk' + LineEnding +
  'Display language: ' + GetDefaultLang + Lineending +
  'Version: ' + ProgramInfo.Version + Lineending +
  'CopyRight: uib gmbh (http://uib.de) under AGPLv3' + LineEnding +
  'http://opsi.org' + Lineending +
  'Credits to: Lazarus/FPC,indy,sqllite');
end;

end.

