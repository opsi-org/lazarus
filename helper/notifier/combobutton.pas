unit combobutton;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  Menus,
  StdCtrls,
  EditBtn,
  ExtCtrls,
  Buttons,
  LazFileutils,
  LResources;

type

   TComboButton = class(TComponent)
  private
    Fbutton_only : boolean;
  published
    panel : TPanel;
    cbox : TComboBox;
    btn : TSpeedButton;
    ImgList: TImageList;
    property button_only : boolean
      read Fbutton_only write Fbutton_only;

  //published
    // properties
  public
    { public declarations }
    constructor Create(AOwner: TComponent; pathtoicon : string = ''; btnonly : boolean = true);
    destructor Destroy;
  end;

implementation



constructor TComboButton.Create(AOwner: TComponent; pathtoicon : string = ''; btnonly : boolean = true);
var
  Picture : TPicture;
  SrcBmp : TBitmap;
  tmpstr : string;
begin
  panel := TPanel.Create(self);
  cbox := TComboBox.Create(panel);
  btn := TSpeedButton.Create(panel);
  ImgList:= TImageList.Create(panel);
  button_only := btnonly;
  //inherited.
  panel.BevelWidth:= 0;
  panel.BevelOuter:= bvNone;
  btn.Parent := panel;
  cbox.Parent := panel;
  if button_only then
  begin
    cbox.Align:= alLeft;
    cbox.Width:= 0;
    cbox.Visible:= false;
    btn.Parent := panel;
    btn.Align:= alClient;
    btn.Visible:= true;
    btn.Caption:= 'Button';
  end
  else
  begin
     Picture := TPicture.Create;
  try
    if pathtoicon = '' then
      Picture.LoadFromLazarusResource('ok2')
    else
    begin
     // tmpstr := AppendPathDelim(ExtractFileDir(Application.ExeName))+ pathtoicon;
      Picture.LoadFromFile(pathtoicon);
    end;
    SrcBmp := TBitmap.Create;
    SrcBmp.Assign(Picture.Graphic);
    ImgList.Add(SrcBmp, nil);
  finally
    Picture.Free;
  end;
    btn.Align:= alRight;
    btn.Width:= 33;
    btn.Images := ImgList;
    btn.ImageIndex:= 0;
    cbox.Align:= alClient;
  end;
    btn.Repaint;
end;

destructor TComboButton.destroy;
begin
  FreeAndNil(ImgList);
  FreeAndNil(btn);
  FreeAndNil(cbox);
  FreeAndNil(panel);
  inherited;
end;

initialization
{$I combobutton.lrs}
end.

