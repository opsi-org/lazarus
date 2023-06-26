unit lazscreeninfo_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  lazscreeninfo_u2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  dlgs: array of TForm2;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var
  i, moncount : integer;
  tmpstr : string;
begin
  memo1.Clear;
  moncount := screen.MonitorCount;
  memo1.Append('Screen:');
  tmpstr := 'Width: '+IntToStr(Screen.Width)+ ' Height: '+IntToStr(Screen.Height)
            +' PPI: '+IntToStr(Screen.PixelsPerInch);
  memo1.Append(tmpstr);
  memo1.Append('Desktop:');
  tmpstr := 'DesktopWidth: '+IntToStr(Screen.DesktopWidth)+ ' DesktopHeight: '+IntToStr(Screen.DesktopHeight);
  memo1.Append(tmpstr);
  memo1.Append('Monitors: '+IntToStr(moncount));
  for i := 0 to moncount -1 do
    begin
      tmpstr := 'Monitor '+IntToStr(i)+':';
      memo1.Append(tmpstr);
      tmpstr := 'Width: '+IntToStr(Screen.Monitors[i].Width)+ ' Height: '+IntToStr(Screen.Monitors[i].Height)
                +' PPI: '+IntToStr(Screen.Monitors[i].PixelsPerInch)
                +' Primary: '+BoolToStr(Screen.Monitors[i].Primary,true);
      memo1.Append(tmpstr);
    end;
  memo1.Append('WorkArea:');
  tmpstr := 'WorkAreaWidth: '+IntToStr(Screen.WorkAreaWidth)+ ' WorkAreaHeight: '+IntToStr(Screen.WorkAreaHeight);
  memo1.Append(tmpstr);

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
    Label2.Caption:=inttostr(top);
  Label4.Caption:=inttostr(left);
  Label6.Caption:=inttostr(Form1.Monitor.MonitorNum);
  Label8.Caption:=inttostr(Form1.Monitor.Top);
  Label10.Caption:=inttostr(Form1.Monitor.Left);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i, moncount : integer;
  tmpstr : string;
begin
   moncount := screen.MonitorCount;
  SetLength(dlgs,moncount);
  for i := 0 to moncount -1 do
    begin
      dlgs[i] := TForm2.Create(form1);
      dlgs[i].Left := Screen.Monitors[i].Left;
      dlgs[i].Show;
    end;
end;

begin


end.

