unit opsi_bitmap_viewer_f1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ExtDlgs, Buttons, StdCtrls;

type

  { TFBitmapViewer }

  TFBitmapViewer = class(TForm)
    BtnOpen: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    procedure BtnOpenClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FBitmapViewer: TFBitmapViewer;

implementation

{$R *.lfm}

{ TFBitmapViewer }

procedure TFBitmapViewer.BtnOpenClick(Sender: TObject);
var
  selected: boolean;
  fname: string;
begin
  try
    selected := OpenPictureDialog1.Execute;
    fname := OpenPictureDialog1.FileName;
  except
    on E: Exception do
    begin
      ShowMessage('Exception while preview file: ' + E.Message);
      selected := False;
    end;
  end;
  if selected then
  begin
    Label1.Caption := fname;
    try
      Image1.Picture.LoadFromFile(fname);
    except
      on E: Exception do
      begin
        ShowMessage('Exception while load from file: ' + E.Message);
      end;
    end;
  end;
end;

end.

