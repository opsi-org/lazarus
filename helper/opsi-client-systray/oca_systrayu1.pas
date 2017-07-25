unit oca_systrayu1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrayIcon1.PopUpMenu := PopupMenu1;
  trayIcon1.Show;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Trayicon1.BalloonFlags:=bfInfo;
  TrayIcon1.BalloonHint:= 'info';
  TrayIcon1.BalloonTitle:= 'info-title';
  TrayIcon1.ShowBalloonHint;
end;

end.

