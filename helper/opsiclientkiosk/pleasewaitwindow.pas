unit pleasewaitwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TFormPleaseWait }

  TFormPleaseWait = class(TForm)
    LabelPleaseWait: TLabel;
    LabelInfo: TLabel;
    ProgressBarWaiting: TProgressBar;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormPleaseWait: TFormPleaseWait;

implementation

{$R *.lfm}

{ TFormPleaseWait }

procedure TFormPleaseWait.FormCreate(Sender: TObject);
begin
  //while True do Application.ProcessMessages;
end;

procedure TFormPleaseWait.FormShow(Sender: TObject);
begin
  //while True do
  Application.ProcessMessages;
end;

procedure TFormPleaseWait.FormActivate(Sender: TObject);
begin
  //while True do Application.ProcessMessages;
end;

end.

