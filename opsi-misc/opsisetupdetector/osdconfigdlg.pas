unit osdconfigdlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  osdbasedata;

type

  { TFOSDConfigdlg }

  TFOSDConfigdlg = class(TForm)
    TIPropertyGrid1: TTIPropertyGrid;
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  FOSDConfigdlg: TFOSDConfigdlg;

implementation

{$R *.lfm}

{ TFOSDConfigdlg }

procedure TFOSDConfigdlg.FormActivate(Sender: TObject);
begin
  TIPropertyGrid1.TIObject := myconfiguration;
end;

end.

