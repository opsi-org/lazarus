unit highdpitestdm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtCtrls,
  highdpitestu1;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.Timer1Timer(Sender: TObject);
begin
  form1.Show;
end;

end.

