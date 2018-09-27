unit osdconfigdlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, osdbasedata, PropEdits;

type

  { TFOSDConfigdlg }

  TFOSDConfigdlg = class(TForm)
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
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
  TIPropertyGrid1.CheckboxForBoolean:=true;
  //TIPropertyGrid1.PropertyEditorHook;
end;

initialization

RegisterPropertyEditor(TypeInfo(string),TConfiguration,'workbench_path',TDirectoryPropertyEditor);

end.

