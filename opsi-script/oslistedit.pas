unit oslistedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ExtCtrls, StdCtrls, Buttons;

type

  { TFListedit }

  TFListedit = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ValueListEditor1: TValueListEditor;
  private
    { private declarations }
  public
    { public declarations }
  end;

  function checkMapGUI(list : Tstringlist) : Tstringlist;

var
  FListedit: TFListedit;

implementation

{$R *.lfm}

function checkMapGUI(list : Tstringlist) : Tstringlist;
begin
  result := Tstringlist.Create;
  FListedit.ValueListEditor1.Strings.Clear;
  FListedit.ValueListEditor1.Strings.AddStrings(list);
  if mrOK = FListedit.ShowModal then
     result.AddStrings(FListedit.ValueListEditor1.Strings);
end;

end.

