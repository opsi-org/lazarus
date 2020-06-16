unit debug;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType,
  //LMessages, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls, Buttons;

type

  { TFDebug }

  TFDebug = class(TForm)
    ButtonClose: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  FDebug: TFDebug;

implementation

uses uibdata;

{$R *.lfm}

procedure TFDebug.FormCreate(Sender: TObject);
begin
  memo1.Align := alClient;
  SpinEdit1.Value := DataModule1.getdebuglevel;
  Visible := False;
  FDebug.Hide;
  memo1.Append('create: hide');
  {could not be done here because myFont is created later }
  //TForm(sender).Font.Name:=myFont;
end;

procedure TFDebug.BitBtnCloseClick(Sender: TObject);
begin
  DataModule1.ShowDebugWindow1Click(Sender);
end;

procedure TFDebug.FormActivate(Sender: TObject);
begin
  if Assigned(datamodule1) then
  begin
    datamodule1.debugOut(5, 'FDebug.FormActivate', 'Activate FDebug');
    datamodule1.debugOut(5, 'FDebug.FormActivate', 'Activate by: ' +
      Sender.UnitName + ' ' + Sender.ToString);
    if not DataModule1.ShowDebugWindow1.Checked then
      hide;
    SpinEdit1.Value := DataModule1.getdebuglevel();
  end;
end;

procedure TFDebug.FormShow(Sender: TObject);
begin
  if Assigned(datamodule1) then
    datamodule1.debugOut(5, 'FDebug.FormShow', 'Show by: ' +
      Sender.UnitName + ' ' + Sender.ToString);
end;

procedure TFDebug.SpinEdit1Change(Sender: TObject);
begin
  DataModule1.setdebuglevel(SpinEdit1.Value);
end;

initialization
  //FDebug.Visible := false;

end.
