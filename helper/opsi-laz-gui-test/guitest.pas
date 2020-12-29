unit guitest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  osversioninfo,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  //Sleep(1000);
  //Application.Terminate;
   Timer1.Interval:=10000;
   writeln('opsi-laz.gui-test: Version: '+getversioninfo+' : start form / gtk test ...');
   Timer1.Enabled:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Timer1.Enabled:=false;
   writeln('terminating regulary');;
   Application.Terminate;
   halt(0);
end;

end.

