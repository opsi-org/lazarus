unit wilogsrv;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Dialogs,
  IdBaseComponent, IdComponent, IdTCPServer, IdCustomTCPServer, IdContext,
  IdAntiFreeze, IdSimpleServer, IdMappedPortTCP, IdUDPServer, wiglob,
  IdSocketHandle, IdGlobal;

type

  { TDataModuleLogServer }

  TDataModuleLogServer = class(TDataModule)
    IdUDPServer1: TIdUDPServer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure IdUDPServer1UDPRead(AThread: TIdUDPListenerThread;
      AData: TIdBytes; ABinding: TIdSocketHandle);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DataModuleLogServer: TDataModuleLogServer;

implementation

{ TDataModuleLogServer }



procedure TDataModuleLogServer.IdUDPServer1UDPRead(AThread: TIdUDPListenerThread;
  AData: TIdBytes; ABinding: TIdSocketHandle);
var
  logline, receiveline: string;
  level: integer;
begin
  if (Length(AData) > 0) then
  begin
    receiveline := BytesToString(AData);
    level := StrToInt(copy(receiveline, 2, 1));
    logline := copy(receiveline, 4, length(receiveline));
    logdatei.DependentAdd(logline, level);
  end;
end;

procedure TDataModuleLogServer.DataModuleDestroy(Sender: TObject);
begin
  IdUDPServer1.Active := False;
end;

procedure TDataModuleLogServer.DataModuleCreate(Sender: TObject);
begin
  try
    IdUDPServer1.Active := True;
  except
    logdatei.DependentAddError('exception activating udp logsrv', LLError);
  end;
end;


initialization
  {$I wilogsrv.lrs}

end.

