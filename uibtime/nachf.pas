unit nachf;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, {LMessages, Messages,} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DBCtrls, ExtCtrls, runprocess;

type

  { TFNachfrage }

  TFNachfrage = class(TForm)
    Label1: TLabel;
    DBText1: TDBText;
    BitBtn1: TBitBtn;
    Timer1: TTimer;
    Timer2: TTimer;
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
 bringtofront;
 SetWindowPos(handle,HWND_TOPMOST,100,100,500,240,
                SWP_NOMOVE and SWP_NOSIZE);
end;

procedure TFNachfrage.FormShow(Sender: TObject);
begin
  if not setwindowtoalldesktops('uibtime') then
     datamodule1.debugOut(2,'nachf', 'failed nachf to all desktops');
end;

procedure TFNachfrage.Timer2Timer(Sender: TObject);
begin
 timer2.enabled := false;
// Fontop.timer2timer(Sender);
 modalresult := mrAbort;
end;

procedure TFNachfrage.FormActivate(Sender: TObject);
begin
 datamodule1.debugOut(5,'Activate FNachf');
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
