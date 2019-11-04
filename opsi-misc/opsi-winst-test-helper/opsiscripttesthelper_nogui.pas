program opsiscripttesthelper_nogui;

//{$mode objfpc}{$H+}
{$MODE Delphi}

//{$APPTYPE CONSOLE}



uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, LResources,
    custapp,
  {$IFDEF UNIX}
  {$ENDIF }
  opsiscripttesthelper_main;


type

  { Tcliapp }

  Tcliapp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    //procedure WriteHelp; virtual;
  end;

var
  Application: Tcliapp;

{ Tcliapp }
constructor Tcliapp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tcliapp.Destroy;
begin
  inherited Destroy;
end;

procedure Tcliapp.DoRun;
begin
  main;
end;

{$R *.res}

begin
  Application:=Tcliapp.Create(nil);
  Application.Title:='opsiscripttesthelper_nogui';
  Application.Initialize;
  Datamodule1 := TDataModule1.Create(nil);
  //Application.Create(TDataModule1, DataModule1);
  DataModule1.Application := Application;
  main;
  Application.DoRun;
  Application.Free;
end.



