unit selectfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  osversioninfo,
  beautifyopsiscript,
  oslog,
  Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
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
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  beautycfg, beatyfile : string;
begin
  beautycfg := ExtractFileDir(ParamStr(0))+PathDelim+'beautify.ini';
  if OpenDialog1.Execute then
  begin;
    beatyfile := OpenDialog1.FileName;
    beautifyopsiscript.initialize(beautycfg,beatyfile);
  end;
end;

end.

