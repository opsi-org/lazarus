unit notifierform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons,
  {$IFDEF WINDOWS}
  systemcriticalu,
  {$ENDIF WINDOWS}
  oslog,
  contnrs;
  //bgraanimatedgif;

type

  { TNform }

  TNform = class(TForm)
    Image1: TImage;
    //objlist : TObjectList;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    //objlist: TObjectList;
  public
    { public declarations }
    procedure choiceClick(Sender: TObject);
    procedure mymouseenter(Sender: TObject);
    procedure mymouseleave(Sender: TObject);
    procedure cboxClick(Sender: TObject);
    procedure cboxEditdone(Sender: TObject);
  end;

var
  Nform: TNform;
  designPPI : integer;
  screenPPI : integer;

implementation

uses
  notifierguicontrol;

{$R *.lfm}

var
  mousein: boolean = False;

{ TNform }

procedure TNform.FormCreate(Sender: TObject);
begin
  //objlist := TObjectList.Create;
  // prevents screensaver to start while running: start
  designPPI := nform.DesignTimePPI;
  screenPPI := Screen.PixelsPerInch;
  {$IFDEF WINDOWS}
  SystemCritical.IsCritical := True;
{$ENDIF WINDOWS}
end;

procedure TNform.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //hideNForm;
end;

procedure TNform.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //prevents closing notifier via ALT-F4
  CloseAction := caNone;
end;

procedure TNform.FormActivate(Sender: TObject);
begin
  Application.BringToFront;
  Application.ProcessMessages;
end;

procedure TNform.FormHide(Sender: TObject);
begin
  // prevents screensaver to start while running: stop
  {$IFDEF WINDOWS}
  SystemCritical.IsCritical := False;
{$ENDIF WINDOWS}
end;

procedure TNform.FormShow(Sender: TObject);
var
  oldFsStyle: TFormstyle;
begin
  (*
  oldFsStyle := FormStyle;
  FormStyle := fsSystemStayOnTop;
  BringToFront;
  Application.ProcessMessages;
  //FormStyle := oldFsStyle;
  repaint;
  Application.ProcessMessages;
  *)
end;

procedure TNform.choiceClick(Sender: TObject);
begin
  logdatei.log('Button clicked.', LLDebug);
  myChoiceClick(Sender);
end;

procedure TNform.cboxClick(Sender: TObject);
begin
  logdatei.log('cbox clicked.', LLDebug);
end;

procedure TNform.cboxEditdone(Sender: TObject);
begin
  logdatei.log('cboxEditdone', LLDebug);
  logdatei.log('cbox: ' + TComboBox(Sender).Items[TComboBox(Sender).ItemIndex], LLDebug);
end;


procedure TNform.mymouseenter(Sender: TObject);
begin
  if not mousein then
  begin
    mousein := True;
    logdatei.log('mouse enter.', LLDebug);
    //logmouseenter(Sender);
  end;
end;

procedure TNform.mymouseleave(Sender: TObject);
begin
  if mousein then
  begin
    mousein := False;
    logdatei.log('mouse leave.', LLDebug);
    //logmouseleave(Sender);
  end;
end;


end.
