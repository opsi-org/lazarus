unit OpsiLinuxInstaller_QueryForm;

// Parent form for all query forms.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OpsiLinuxInstaller_BaseForm;

type
  TOpsiLinuxInstallerQueryForm = class(TOpsiLinuxInstallerBaseForm)
    BtnBack: TButton;
    procedure BtnBackClick(Sender: TObject); virtual; abstract;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); virtual; abstract;
  end;

implementation

end.

