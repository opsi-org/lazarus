unit notifierform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  {$IFDEF WINDOWS}
  systemcriticalu,
  {$ENDIF WINDOWS}
  oslog,
  contnrs;

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
  end;

var
  Nform: TNform;

implementation

uses
  notifierguicontrol;

{$R *.lfm}

{ TNform }

procedure TNform.FormCreate(Sender: TObject);
begin
  //objlist := TObjectList.Create;
  // prevents screensaver to start while running: start
  {$IFDEF WINDOWS} SystemCritical.IsCritical := true; {$ENDIF WINDOWS}
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
  {$IFDEF WINDOWS} SystemCritical.IsCritical := false; {$ENDIF WINDOWS}
end;

procedure TNform.FormShow(Sender: TObject);
var
  oldFsStyle : TFormstyle;
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
  logdatei.log('Button clicked.', LLDebug2);
  myChoiceClick(Sender);
end;

end.

