program helperchild_nogui;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, //LResources,
    custapp,
  {$IFDEF UNIX}
  baseunix,
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
  Application.Title:='helperchild_nogui';
  Application.Initialize;
  opsiscripttesthelper_main.Application := Application;
    main;
 // Application.Run;
  //application.Terminate;
  Application.Free;
end.


