unit notifierform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, contnrs;

type

  { TNform }

  TNform = class(TForm)
    Image1: TImage;
    //objlist : TObjectList;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { private declarations }
    objlist: TObjectList;
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
  objlist := TObjectList.Create;
end;

procedure TNform.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //hideNForm;
end;

procedure TNform.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //hideNForm;
end;

procedure TNform.FormHide(Sender: TObject);
begin
  //hideNForm;
end;

procedure TNform.choiceClick(Sender: TObject);
begin
  //hideNForm;
  myChoiceClick(Sender);
end;

end.

