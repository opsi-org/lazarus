unit DaemonMapperOpsiClientd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type

  { TOpsiClientdMapper }

  TOpsiClientdMapper = class(TDaemonMapper)
    procedure OpsiClientdMapperCreate(Sender: TObject);
  private

  public

  end;

var
  OpsiClientdMapper: TOpsiClientdMapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TOpsiClientdMapper)
end;

{$R *.lfm}

{ TOpsiClientdMapper }

procedure TOpsiClientdMapper.OpsiClientdMapperCreate(Sender: TObject);
begin

end;


initialization
  RegisterMapper;
end.

