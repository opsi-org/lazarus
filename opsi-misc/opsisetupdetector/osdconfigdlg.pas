unit osdconfigdlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, osdbasedata, PropEdits;

type

  { TFOSDConfigdlg }

  TFOSDConfigdlg = class(TForm)
    FlowPanel1: TFlowPanel;
    MemoConfigHint: TMemo;
    Panel1: TPanel;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure FormActivate(Sender: TObject);
    procedure TIPropertyGrid1Click(Sender: TObject);
    procedure TIPropertyGrid1Exit(Sender: TObject);
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

procedure TFOSDConfigdlg.TIPropertyGrid1Click(Sender: TObject);
var
  activeprop : string;
begin
  if sender = TIPropertyGrid1 then
  begin
    activeprop := TIPropertyGrid1.GetActiveRow.Name;
    MemoConfigHint.Text:= myconfigurationhints.Values[activeprop];
  end;

end;

procedure TFOSDConfigdlg.TIPropertyGrid1Exit(Sender: TObject);
begin

end;

initialization

RegisterPropertyEditor(TypeInfo(string),TConfiguration,'workbench_path',TDirectoryPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStrings),TConfiguration,'import_libraries',TStringsPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStrings),TConfiguration,'preInstallLines',TStringsPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStrings),TConfiguration,'postInstallLines',TStringsPropertyEditor);

end.

