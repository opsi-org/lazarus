unit helpinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, proginfo,
  lcltranslator, Buttons;

type

  { TFormHelpInfo }

  TFormHelpInfo = class(TForm)
    ButtonClose: TButton;
    ButtonAbout: TButton;
    CheckBoxExpertMode: TCheckBox;
    MemoInfo: TMemo;
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormHelpInfo: TFormHelpInfo;

{resourcestring
  rsExpertMode = 'Expert mode';}


implementation

{$R *.lfm}

{ TFormHelpInfo }

procedure TFormHelpInfo.FormCreate(Sender: TObject);
begin
   //SpeedButtonExpertMode.Caption := rsExpertMode;
   MemoInfo.Lines.Add(
  'Documentation: www.opsi.org' + LineEnding +
  '-----------------------------------' + LineEnding +
  'opsi-client-kiosk' + LineEnding +
  'Display language: ' + GetDefaultLang + Lineending +
  'Version: ' + ProgramInfo.Version + Lineending +
  'CopyRight: uib gmbh (http://uib.de) under AGPLv3' + LineEnding +
  'http://opsi.org' + Lineending +
  'Credits to: Lazarus/FPC,synapse,sqllite');
end;


procedure TFormHelpInfo.ButtonAboutClick(Sender: TObject);
begin
    ShowMessage(
  'Documentation: www.opsi.org' + LineEnding +
  '-----------------------------------' + LineEnding +
  'opsi-client-kiosk' + LineEnding +
  'Display language: ' + GetDefaultLang + Lineending +
  'Version: ' + ProgramInfo.Version + Lineending +
  'CopyRight: uib gmbh (http://uib.de) under AGPLv3' + LineEnding +
  'http://opsi.org' + Lineending +
  'Credits to: Lazarus/FPC,synapse,sqllite');
end;

procedure TFormHelpInfo.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

end.

