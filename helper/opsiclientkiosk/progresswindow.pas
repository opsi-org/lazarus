unit progresswindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type
  TFormProgressWindow = class(TForm)
  ProgressBar1:TProgressBar;
  ProgressBarDetail:TProgressBar;
  LabelDataLoad:TLabel;
  LabelDataLoadDetail:TLabel;
  LabelInfo:TLabel;
  private

  public
    //procedure ProcessMess;
  end;

var
  FormProgressWindow: TFormProgressWindow;

implementation

{$R *.lfm}

{procedure TFormProgressWindow.ProcessMess;
begin
  Application.ProcessMessages;
end;}

end.

