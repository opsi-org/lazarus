unit OpsiLinuxInstaller_WelcomeForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OpsiLinuxInstaller_BaseForm;

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
  end;

implementation

end.

