unit nachf;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, {LMessages, Messages,} SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  StdCtrls, Buttons, DBCtrls, ExtCtrls, linhandlewin;

type

  { TFNachfrage }

  TFNachfrage = class(TForm)
    Label1: TLabel;
    DBText1: TDBText;
    BitBtn1: TBitBtn;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    //procedure CustomExceptionHandler(Sender: TObject; E: Exception);
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  FNachfrage: TFNachfrage;

implementation

uses uibdata{, debug};

{$R *.lfm}

procedure TFNachfrage.Timer1Timer(Sender: TObject);
begin
  datamodule1.debugOut(5, 'FNachf', 'Timer1 pos and front start');
  bringtofront;
  SetWindowPos(handle, HWND_TOPMOST, 100, 100, 500, 240,
    SWP_NOMOVE and SWP_NOSIZE);
  datamodule1.debugOut(5, 'FNachf', 'Timer1 pos and front end');
end;

procedure TFNachfrage.FormShow(Sender: TObject);
begin
  if not setwindowtoalldesktops(FNachfrage.Caption) then
    datamodule1.debugOut(2, 'nachf', 'failed nachf to all desktops');
end;

procedure TFNachfrage.FormMouseEnter(Sender: TObject);
begin
  datamodule1.debugOut(5, 'FNachf', 'Mouse Enter');
end;

procedure TFNachfrage.FormCreate(Sender: TObject);
begin
  DataModule1.SetFontName(TControl(sender),myFont);
  (*
  TForm(sender).Font.Name:=myFont;
  TForm(sender).ch
  Label1.Font.Name:=myFont;
  DBText1.Font.Name:=myFont;
  BitBtn1.Font.Name:=myFont;;
  *)
end;

procedure TFNachfrage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  modalresult := mrOK;
  DataModule1.TimerOnTop.Enabled:=true;
end;

procedure TFNachfrage.BitBtn1Click(Sender: TObject);
begin

end;

procedure TFNachfrage.FormPaint(Sender: TObject);
begin
  datamodule1.debugOut(5, 'FNachf', 'Repaint');
end;

procedure TFNachfrage.Timer2Timer(Sender: TObject);
begin
  timer2.Enabled := False;
  // Fontop.timer2timer(Sender);
  modalresult := mrAbort;
end;

procedure TFNachfrage.FormActivate(Sender: TObject);
begin
  datamodule1.debugOut(5, 'FNachf', 'Activate');
end;

(*
procedure TFNachfrage.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
  Application.ShowException(E);
  DumpExceptionCallStack(E);
  datamodule1.debugOut(1, 'Exception', 'Exception dumped - terminating');
  //Application.Terminate;
end;
*)

end.
