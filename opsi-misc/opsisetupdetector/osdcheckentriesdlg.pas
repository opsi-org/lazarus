unit osdcheckentriesdlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Menus;

type

  { TFCheckenties }

  TFCheckenties = class(TForm)
    BitBtn1: TBitBtn;
    CheckBoxDoNotShowCheckEntries: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FCheckenties: TFCheckenties;


  resourcestring

  //  rscheckEntriesMsg = 'You have to check every data field if this data are correct or plausible.';
(*
    rscheckEntriesMsg =
        'The following data are automatically detected. ' +
        'You have to check every data field if this data are correct or plausible. ' +
        'For some data you may have to install the program once  ' +
        'and than get the needed data from the completed installation.';
        *)

  rscheckEntriesMsg =
    'The following data are automatically detected.' + Lineending +
    'You have to check every data field if this data are correct or plausible.' +  Lineending +
    'For some data you may have to install the program once ' +  Lineending +
    'and than get the needed data from the completed installation.';

implementation

{$R *.lfm}


{ TFCheckenties }

procedure TFCheckenties.FormCreate(Sender: TObject);
begin
  //Memo1.Text:= rscheckEntriesMsg;
  label2.Caption:= rscheckEntriesMsg;
end;

end.

