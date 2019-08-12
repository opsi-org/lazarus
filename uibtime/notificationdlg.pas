unit notificationdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ButtonPanel, ComCtrls, uibdata, IniFiles, LazFileUtils;

type

  { TFnotificationdlg }

  TFnotificationdlg = class(TForm)
    CheckBoxTray: TCheckBox;
    LabelTrayInterval: TLabel;
    SpinEditTrayInterval: TSpinEdit;
    ToolBar1: TToolBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Fnotificationdlg: TFnotificationdlg;

implementation

{$R *.lfm}

{ TFnotificationdlg }

procedure TFnotificationdlg.FormShow(Sender: TObject);
begin
  CheckBoxTray.Checked := Trayshow;
  SpinEditTrayInterval.Value := TrayInterval;
  DataModule1.TimerTrayIcon.Interval := TrayInterval * 60 * 1000;
  DataModule1.TrayIcon1.Visible := Trayshow;
  DataModule1.TimerTrayIcon.Enabled := Trayshow;
end;

procedure TFnotificationdlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  logdir, logfeilname: string;
  myini: TIniFile;
begin
  Trayshow := CheckBoxTray.Checked;
  TrayInterval := SpinEditTrayInterval.Value;
  DataModule1.TimerTrayIcon.Interval := TrayInterval * 60 * 1000;
  DataModule1.TrayIcon1.Visible := Trayshow;
  DataModule1.TimerTrayIcon.Enabled := Trayshow;
  // we will use logdir for logging and for configuration
  logdir := SysUtils.GetAppConfigDir(False);
  if logdir = '' then
  begin
    logdir := SysUtils.GetUserDir;
    logdir := logdir + '\uibtime';
  end;
  logdir := ExpandFileName(logdir);
  ForceDirectories(logdir);
  logfeilname := ExpandFileNameUTF8(logdir + '\uibtime.conf');
  myini := TIniFile.Create(logfeilname);
  if myini = nil then
  begin
    //Fdebug.Memo1.Append('myini = nil: coud not open :' + logfeilname);
    datamodule1.debugOut(2, 'notificationdlg.FormClose',
      'myini = nil: coud not open :' + logfeilname);
    ShowMessage('Fehler in Konfigurations Datei. Bitte Log sichern. Programm wird beendet');
    Application.Terminate;
  end;
  Trayshow := myini.ReadBool('general', 'showTray', True);
  myini.WriteBool('general', 'showTray', Trayshow);
  myini.WriteInteger('general', 'TrayInterval', TrayInterval);
  myini.UpdateFile;
  myini.Free;
end;

end.
