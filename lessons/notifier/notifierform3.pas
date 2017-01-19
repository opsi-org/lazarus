unit notifierform3;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  windows ,
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  xatom, X, xlib,
  gtk2,
  gdk2x,
  //GTKProc,
  //GtkDef,
  ctypes,
  {$ENDIF LINUX}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtDlgs, ExtCtrls, Buttons, Spin, notifierform3a;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    ButtonTransparent3: TButton;
    ButtonTransparent2: TButton;
    ButtonTransparent: TButton;
    ButtonFadeIn: TButton;
    ButtonMoveInRB2: TButton;
    ButtonMoveInRB: TButton;
    ButtonLoad: TButton;
    ButtonBorder: TButton;
    ButtonCenter1: TButton;
    ButtonRightBottom: TButton;
    ButtonCenter2: TButton;
    ButtonRightTop: TButton;
    ButtonShow: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    SpinEdit1: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure ButtonFadeInClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonBorderClick(Sender: TObject);
    procedure ButtonCenter1Click(Sender: TObject);
    procedure ButtonMoveInRB2Click(Sender: TObject);
    procedure ButtonMoveInRBClick(Sender: TObject);
    procedure ButtonRightBottomClick(Sender: TObject);
    procedure ButtonCenter2Click(Sender: TObject);
    procedure ButtonRightTopClick(Sender: TObject);
    procedure ButtonShowClick(Sender: TObject);
    procedure ButtonTransparent2Click(Sender: TObject);
    procedure ButtonTransparent3Click(Sender: TObject);
    procedure ButtonTransparentClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  procedure WindowTransparent(const f: THandle; const tp: integer);

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonLoadClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Form2.Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    //Form2.Show;
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.ButtonFadeInClick(Sender: TObject);
var
  i : integer;
begin
  form2.AlphaBlend:=true;
  form2.AlphaBlendValue:=0;
  for i := 1 to 255 do
  begin
    sleep(1);
    form2.AlphaBlendValue:=i;
    form2.Repaint;
    Application.ProcessMessages;
  end;
end;

procedure TForm1.ButtonBorderClick(Sender: TObject);
begin
  form2.BorderStyle:= bsNone;
end;

procedure TForm1.ButtonCenter1Click(Sender: TObject);
begin
  Form2.Position:=poScreenCenter;
end;

procedure TForm1.ButtonMoveInRB2Click(Sender: TObject);
var
  x,y,i : integer;
begin
  x:= screen.Width;
  y := screen.WorkAreaHeight;
  Form2.Top:= y;
  Form2.Left:=x - form2.Width;
  for i := 1 to form2.Height do
  begin
    Sleep(1);
    Form2.Top:= y - i;
  end;
end;

procedure TForm1.ButtonMoveInRBClick(Sender: TObject);
var
  x,y,i, startheight : integer;
begin
  x:= screen.Width;
  startheight :=  form2.Height;
  form2.Height:=0;
  y := screen.Height;
  Form2.Top:= y;
  Form2.Left:=x - form2.Width;
  for i := 1 to startheight do
  begin
    Sleep(1);
    Form2.Top:= y - i;
    form2.Height:= Form2.Height + 1;
    form2.Repaint;
    Application.ProcessMessages;
  end;
end;


procedure TForm1.ButtonRightBottomClick(Sender: TObject);
var
  x,y : integer;
begin
  x:= screen.Width;
  y := screen.Height;
  Form2.Top:= y - form2.Height;
  Form2.Left:=x - form2.Width;
end;

procedure TForm1.ButtonCenter2Click(Sender: TObject);
var
  x,y : integer;
begin
  x:= screen.Width div 2;
  y := screen.Height div 2;
  Form2.Top:= y - (form2.Height div 2);
  Form2.Left:=x - (form2.Width div 2);
end;

procedure TForm1.ButtonRightTopClick(Sender: TObject);
var
  x : integer;
begin
  x:= screen.Width;
  Form2.Top:= 20;
  Form2.Left:=x - form2.Width - 20;
end;

procedure TForm1.ButtonShowClick(Sender: TObject);
begin
  form2.FormStyle:=fsSystemStayOnTop;
  form2.Show;
end;

procedure TForm1.ButtonTransparent2Click(Sender: TObject);
begin
  form2.Image1.Transparent:=true;
  form2.AlphaBlend:=true;
  form2.AlphaBlendValue:=128;
  SpinEdit1.Value:=form2.AlphaBlendValue;
end;

procedure TForm1.ButtonTransparent3Click(Sender: TObject);
begin
  form2.Image1.Transparent:=true;
  WindowTransparent(Form2.Handle, 10);
end;

procedure TForm1.ButtonTransparentClick(Sender: TObject);
begin
  form2.Image1.Transparent:=true;
  //form2.FormStyle:=fsSplash;
  // http://forum.lazarus.freepascal.org/index.php?topic=34338.0
  // http://lazplanet.blogspot.de/2013/04/make-your-forms-transparent.html
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  form2.AlphaBlendValue:=SpinEdit1.Value;
end;


{$IFDEF WINDOWS}
// from
// http://stackoverflow.com/questions/41068387/how-to-make-transparent-form-in-lazarus
procedure WindowTransparent(const f: THandle; const tp: integer);
begin
Form2.Color:=clRed;
SetWindowLongPtr(f, GWL_EXSTYLE,GetWindowLongPtr(f, GWL_EXSTYLE) or WS_EX_LAYERED);
SetLayeredWindowAttributes(f, clRed, 0, LWA_COLORKEY);
end;
{$ENDIF WINDOWS}

{$IFDEF LINUX}
// from:
// Lazarus [Delphi] := Form1.Transparent ; // Crashkurs (18)
// https://www.youtube.com/watch?v=oPFyQI0GF_o
// https://drive.google.com/drive/folders/0B24-JPyA8ovSV3JIczBnSXBCSVk?tid=0B24-JPyA8ovSOURZa2lpUzZjWU0
procedure WindowTransparent(const f: THandle; const tp: integer);
var
 transparency: double;
 opacity: double;
 atom_window_opacity: TAtom;
 dpy: PDisplay;
 win: TWindow;
begin
 dpy := XOpenDisplay(nil);
 win := GDK_WINDOW_XWINDOW(PGtkWidget(PtrUInt(f))^.window);
 transparency := tp / 100;
 opacity      := trunc($FFFFFFFF * transparency);
 atom_window_opacity := XInternAtom(dpy, '_NET_WM_WINDOW_OPACITY', False);
 XChangeProperty(dpy, win, atom_window_opacity, XA_CARDINAL,
   32, PropModeReplace, @PcuChar(opacity), 1);
 XCloseDisplay(dpy);
end;
{$ENDIF LINUX}

end.

