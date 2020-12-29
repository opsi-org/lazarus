unit selectfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  osversioninfo,
  beautifyopsiscript,
  oslog,
  Buttons, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    FlowPanel3: TFlowPanel;
    FlowPanel4: TFlowPanel;
    Label1: TLabel;
    Label2: TLabel;
    LabelLogFile: TLabel;
    Labelfilename: TLabel;
    Label3: TLabel;
    LabelState: TLabel;
    OpenDialog1: TOpenDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure initLogging;
var
  logfilename : string;
begin
  logdatei := TLogInfo.Create;
  logfilename := 'opsi-script-beautifier.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := 7;
  (*
  for i := 0 to preLogfileLogList.Count-1 do
    logdatei.log(preLogfileLogList.Strings[i], LLessential);
  preLogfileLogList.Free;
  *)
  logdatei.log('opsi-script-beautyfier version: ' + getversioninfo, LLessential);
  //logdatei.log('Called as: ' + ExtractFileNameOnly(reencode(ParamStr(0), 'system')), LLessential);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  initLogging;
  LabelLogFile.Caption:=LogDatei.FileName;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  beautycfg, beautyfile : string;
begin
  beautycfg := ExtractFileDir(ParamStr(0))+PathDelim+'beautify.ini';
  if OpenDialog1.Execute then
  begin;
    beautyfile := OpenDialog1.FileName;
    Labelfilename.Caption:=beautyfile;
    LabelState.Caption:='working';
    beautifyopsiscript.initialize(beautycfg,beautyfile);
    LabelState.Caption:='finished';
  end;
end;

end.

