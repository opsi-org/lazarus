unit SetRights;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpsiConnection;

Type
  TShowProgressEvent = procedure(Info: String; Progress:integer) of Object;

  TSetRightsThread = class(TThread)
  private
    InfoText : string;
    ProgressStatus : integer;
    OnShowProgressEvent: TShowProgressEvent;
    procedure ShowProgress;
  protected
    procedure Execute; override;
  public
    Constructor Create(CreateSuspended : boolean);
    property OnShowProgress: TShowProgressEvent read OnShowProgressEvent write OnShowProgressEvent;
  end;

implementation

constructor TSetRightsThread.Create(CreateSuspended : boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

procedure TSetRightsThread.ShowProgress;
// Diese Methode wird vom MainThread ausgeführt und kann deshalb auf alle GUI-Elemente zugreifen
begin
  if Assigned(OnShowProgress) then
  begin
    OnShowProgress(InfoText, ProgressStatus);
  end;
end;

procedure TSetRightsThread.Execute;
var
  NewInfo : string;
begin
  InfoText := 'TMyThread Starting...';
  Synchronize(@ShowProgress);
  InfoText := 'TMyThread Running...';
  while (not Terminated) do
    begin
      { here goes the code of the main thread loop }

      if NewInfo <> InfoText then
        begin
          InfoText := NewInfo;
          Synchronize(@ShowProgress);
        end;
    end;
end;

end.

