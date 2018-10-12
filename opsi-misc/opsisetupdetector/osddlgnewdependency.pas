unit osddlgnewdependency;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
  osdbasedata;

type

  { TFNewDepDlg }

  TFNewDepDlg = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    TIPropertyGridNewDep: TTIPropertyGrid;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FNewDepDlg: TFNewDepDlg;
  newdependency : TPDependency;

implementation

{$R *.lfm}

{ TFNewDepDlg }

procedure TFNewDepDlg.FormShow(Sender: TObject);
begin
  // create new dependency
  newdependency:= TPDependency(aktProduct.dependencies.add);
  newdependency.init;
  TIPropertyGridNewDep.TIObject := newdependency;


end;

procedure TFNewDepDlg.FormHide(Sender: TObject);
begin
  TIPropertyGridNewDep.ConsistencyCheck;
  TIPropertyGridNewDep.TIObject := nil;
end;

end.

