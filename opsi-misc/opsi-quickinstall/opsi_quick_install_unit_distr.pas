unit opsi_quick_install_unit_distr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TDistribution }

  TDistribution = class(TForm)
    BtnBack: TButton;
    BtnNext: TButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
  private

  public
  var
    GoOn: boolean;
  end;

var
  Distribution: TDistribution;

implementation

{$R *.lfm}

{ TDistribution }

procedure TDistribution.BtnNextClick(Sender: TObject);
begin
  GoOn:= True;
  Distribution.Close;
end;

procedure TDistribution.BtnBackClick(Sender: TObject);
begin
  GoOn:=False;
  Distribution.Close;
end;

end.

