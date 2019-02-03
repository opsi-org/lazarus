unit oslogtestu1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  {$IFDEF LINUX}
  baseUnix,   // used for  fpGetEUid
  {$ENDIF LINUX}
  oslog;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure closelog;
begin
  LogDatei.log('Close the log', LLNotice);
  LogDatei.Close;
end;

procedure initlog;
var
  lfilename: string;
begin
  // Initialize logging
  // create the object
  LogDatei := TLogInfo.Create;
  // we do not need part logs
  LogDatei.WritePartLog := False;
  // set the loglevel
  LogDatei.LogLevel := 8;
  // get the name for the log file
  lfilename := ExtractFileName(Application.ExeName);
  lfilename := ExtractFileNameWithoutExt(lfilename);
  // set the name for the log file
  // yes that is confusing and has to be cleaned up
  LogDatei.FileName := lfilename;
  LogDatei.StandardLogFileext := '.log';
  LogDatei.StandardLogFilename := lfilename;
  LogDatei.CreateTheLogfile(lfilename + '.log', True);
  // now we can start with the inital logentry
  LogDatei.log('Log for: ' + Application.exename + ' opend at : ' +
    DateTimeToStr(now), LLNotice);
  // now you can use logdatei everywhere if oslog is in the uses list
  // do not forget to close the log at the end of the program

  // The following part is not needed and only for your information:

  // some pre compiler definitions that influence the logpath
  // have a look at the end of the oslog.pas for mor information
  // have a look at project settings / compiler settings / custom settings
  // to create cutom defines ( WINDOWS or LINUX are defined automatically)
  {$IFDEF WINDOWS}
  LogDatei.log('WINDOWS is defined', LLNotice);
  {$IFDEF OPSI}
  LogDatei.log('OPSI is defined', LLNotice);
  {$IFDEF OPSI_AS_USER}
  LogDatei.log('OPSI_AS_USER is defined', LLNotice);
  {$ENDIF OPSI_AS_USER}
  {$ELSE OPSI}
  LogDatei.log('OPSI is not defined', LLNotice);
  {$ENDIF OPSI}
  {$ELSE WINDOWS}
  LogDatei.log('WINDOWS is not defined', LLNotice);
  if 0 = fpGetEUid then
  begin
    {$IFDEF OPSI}
    LogDatei.log('OPSI is defined and we running as root', LLNotice);
    {$ELSE OPSI}
    LogDatei.log('OPSI is not defined and we running as root', LLNotice);
    {$ENDIF OPSI}
  end
  else
  begin
    LogDatei.log('We are not running as root', LLNotice);
  end;
  {$ENDIF}
  LogDatei.log('Log path is: ' + LogDatei.FileName, LLNotice);


end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  initlog;
  Label1.Caption:= 'logfile: '+LogDatei.FileName;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    closelog;
end;

end.

