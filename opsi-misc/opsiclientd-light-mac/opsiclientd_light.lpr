program opsiclientd_light;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} //{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}//{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ServerGUI, OpsiJSONRequest, OpsiJSONResponse, opsiHTTPSListeningThread,
  OpsiHTTPMessageBody, OpsiHTMLMessageBody, OpsiJSONMessageBody,
  OpsiJSONrpcObject, opsiclientdlog;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

