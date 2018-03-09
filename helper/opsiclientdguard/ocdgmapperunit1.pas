unit ocdgmapperunit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type

  { TDaemonMapper1 }

  TDaemonMapper1 = class(TDaemonMapper)
    procedure DaemonMapper1DaemonDefs0CreateInstance(Sender: TObject);
    procedure DaemonMapper1Install(Sender: TObject);
    procedure DaemonMapper1Run(Sender: TObject);
    procedure DaemonMapper1UnInstall(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DaemonMapper1: TDaemonMapper1;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TDaemonMapper1)
end;

{$R *.lfm}

{ TDaemonMapper1 }

procedure TDaemonMapper1.DaemonMapper1DaemonDefs0CreateInstance(Sender: TObject
  );
begin
  Application.Log(etDebug, 'DaemonMapper.Create');
end;

procedure TDaemonMapper1.DaemonMapper1Install(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Install');
end;

procedure TDaemonMapper1.DaemonMapper1Run(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Run');
end;

procedure TDaemonMapper1.DaemonMapper1UnInstall(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Uninstall');
end;


initialization
  RegisterMapper;
end.

