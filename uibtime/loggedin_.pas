unit loggedin_;

{$MODE Delphi}

interface

uses
(*    {$IFDEF WINDOWS}
  Windows,
     {$ENDIF WINDOWS}
*)
 { Messages,} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, Buttons, {ToolWin, ComCtrls,} ExtCtrls, linhandlewin;

type
  TFLoggedin = class(TForm)
    ListBox1: TListBox;
    Splitter1: TSplitter;
    Panel1: TPanel;
    BtnAktualisieren: TSpeedButton;
    StaticText1: TStaticText;
    SpinEdit1: TSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure BtnAktualisierenClick(Sender: TObject);
    procedure SpinEditAktualisierungsIntervallChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure CheckBoxOntopClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FLoggedin: TFLoggedin;

implementation

uses uibdata;

{$R *.lfm}

procedure TFLoggedin.FormShow(Sender: TObject);
begin
  try
    datamodule1.debugOut(5, 'start von FLoggedin.FormShow');
    left := 0;
    top := 0;
    Datamodule1.getLoggedInList(listbox1.items, False);
    Datamodule1.setTimerQueryLoggedIn(60000 * SpinEdit1.Value, True);
    datamodule1.debugOut(5, 'ende von FLoggedin.FormShow');
  except
    on e: Exception do
    begin
      datamodule1.debugOut(3, 'exception in FLoggedin.FormShow');
      datamodule1.debugOut(3, e.Message);
      raise;
    end;
  end;
end;

procedure TFLoggedin.FormHide(Sender: TObject);
begin
  try
    datamodule1.debugOut(5, 'start von FLoggedin.FormHide');
    Datamodule1.setTimerQueryLoggedIn(60000 * SpinEdit1.Value, False);
    datamodule1.debugOut(5, 'ende von FLoggedin.FormHide');
  except
    on e: Exception do
    begin
      datamodule1.debugOut(3, 'exception in FLoggedin.FormHide');
      datamodule1.debugOut(3, e.Message);
      raise;
    end;
  end;
end;

procedure TFLoggedin.BtnAktualisierenClick(Sender: TObject);
begin
  Datamodule1.getLoggedInList(listbox1.items, False);
end;

procedure TFLoggedin.SpinEditAktualisierungsIntervallChange(Sender: TObject);
begin
  Datamodule1.setTimerQueryLoggedIn(60000 * SpinEdit1.Value, True);
end;


procedure TFLoggedin.FormCreate(Sender: TObject);
begin
  DataModule1.setfloggedin_created(True);
  if DataModule1.Weristda1.Checked then
  begin
    FLoggedin.Visible:= true;
    (*
    if not setwindowtoalldesktops(FLoggedin.Caption) then
        datamodule1.debugOut(2,'ontop', 'failed presenz to all desktops');
        *)
  end
  else FLoggedin.Visible:= false;
end;

end.
