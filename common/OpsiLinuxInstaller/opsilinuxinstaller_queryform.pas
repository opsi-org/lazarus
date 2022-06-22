unit OpsiLinuxInstaller_QueryForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OpsiLinuxInstaller_BaseForm;

type
  TOpsiLinuxInstallerQueryForm = class(TOpsiLinuxInstallerBaseForm)
    BtnBack: TButton;
  private
  public
    procedure BtnBackClick(Sender: TObject); virtual; abstract;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); virtual; abstract;
  end;

implementation

end.

