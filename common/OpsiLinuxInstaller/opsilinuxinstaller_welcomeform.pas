unit OpsiLinuxInstaller_WelcomeForm;

// Parent form for the first form shown by the installer, includes language selection.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OpsiLinuxInstaller_BaseForm,
  OpsiLinuxInstaller_LanguageObject,
  WelcomeResourceStrings;

type
  TOpsiLinuxInstallerWelcomeForm = class(TOpsiLinuxInstallerBaseForm)
    BtnBack: TButton;
    BtnFinish: TButton;
    ComboBoxLanguages: TComboBox;
    WelcomePanel: TPanel;
    LabelWelcome: TLabel;
    LabelSelLanguage: TLabel;
    CarryOutPanel: TPanel;
    LabelCarryOut: TLabel;
    procedure SetTextsByResourceStrings; virtual;
  end;

implementation

procedure TOpsiLinuxInstallerWelcomeForm.SetTextsByResourceStrings;
begin
  Language.TranslateResourceStrings('WelcomeResourceStrings',
    'Welcome.' + Language.Abbreviation + '.po');

  LabelWelcome.Caption := rsWelcome;
  LabelSelLanguage.Caption := rsSelLanguage;
  LabelCarryOut.Caption := rsCarryOut;
  BtnNext.Caption := rsNext;
end;

end.
