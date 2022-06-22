unit OpsiLinuxInstaller_BaseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TOpsiLinuxInstallerBaseForm = class(TForm)
    BackgroundImage: TImage;
    BtnNext: TButton;
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormActivate(Sender: TObject); virtual;
    procedure BtnNextClick(Sender: TObject); virtual; abstract;
    // make Panel settings and load background and info images
    procedure SetBasics(Sender: TForm);
    // make InfoImage settings
    procedure setInfoBasics(InfoImage: TImage);
    // show hint on click of InfoImage
    // (used with '@' and therefore must be defined in TConfigedInstaller)
    procedure ShowHintOnClick(Sender: TObject);
  private
  public
  const
    // same width for all panels
    panelWidth = 460;
    // position Panels with twice as much space to the left than to the right of the form
    //panelLeft := Round((Width - panelWidth) * 2 / 3);
    panelLeft = 143;
    // same size for all info images (squares)
    infoSize = 22;
    // same background image for all forms
    BackgroundImageFileName = 'opsi.png';
    // same image for all infos
    InfoImageFileName = 'dialog-information.png';
  end;

implementation

// show hint on click of InfoImage
// (used with '@' and therefore must be defined in TConfigedInstaller)
procedure TOpsiLinuxInstallerBaseForm.ShowHintOnClick(Sender: TObject);
begin
  Application.ActivateHint(TWinControl(Sender).ClientToScreen(Point(1, 1)), True);
end;

// make InfoImage settings
procedure TOpsiLinuxInstallerBaseForm.setInfoBasics(InfoImage: TImage);
begin
  InfoImage.Width := infoSize;
  InfoImage.Height := infoSize;
  // set info image
  InfoImage.Picture.LoadFromFile(
    ExtractFilePath(ParamStr(0)) + InfoImageFileName);
  InfoImage.BorderSpacing.Left := 5;
  // Show info hints also on click of image
  InfoImage.OnClick := @ShowHintOnClick;
end;

// make Panel settings and load background and info images
procedure TOpsiLinuxInstallerBaseForm.SetBasics(Sender: TForm);
var
  compIndex: integer;
  PanelBigLine, PanelSmallLine: TPanel;
begin
  for compIndex := 0 to Sender.ComponentCount - 1 do
  begin
    if (Sender.Components[compIndex].ClassName = 'TPanel') then
    begin
      (Sender.Components[compIndex] as TPanel).Left := panelLeft;
      (Sender.Components[compIndex] as TPanel).Width := panelWidth;
      (Sender.Components[compIndex] as TPanel).Color := clForm;
    end
    else
    if (Sender.Components[compIndex].ClassName = 'TImage') and
      // load info icon
      (Pos('Info', Sender.Components[compIndex].Name) = 1) then
      setInfoBasics(Sender.Components[compIndex] as TImage);
  end;

  // big decoration line at bottom in opsi-blue
  PanelBigLine := TPanel.Create(Sender);
  PanelBigLine.Parent := Sender;
  PanelBigLine.ParentColor := False;
  PanelBigLine.BevelOuter := bvNone;
  PanelBigLine.Height := 5;
  PanelBigLine.Top := Sender.Height - PanelBigLine.Height;
  PanelBigLine.Left := 0;
  PanelBigLine.Width := Sender.Width;
  // Opsi-Blau: #006599
  PanelBigLine.Color := TColor($00996500);
  PanelBigLine.BevelColor := TColor($00996500);

  // thin decoration line above the big one in opsi-red
  PanelSmallLine := TPanel.Create(Sender);
  PanelSmallLine.Parent := Sender;
  PanelSmallLine.ParentColor := False;
  PanelSmallLine.BevelOuter := bvNone;
  PanelSmallLine.Height := 3;
  PanelSmallLine.Top := Sender.Height - 11;
  PanelSmallLine.Left := 0;
  PanelSmallLine.Width := Sender.Width;
  // Opsi-Rot: #B42554
  PanelSmallLine.Color := TColor($005425B4);
  PanelSmallLine.BevelColor := TColor($005425B4);
end;

procedure TOpsiLinuxInstallerBaseForm.FormCreate(Sender: TObject);
begin
  // set constant form size
  Height := 450;
  //Width := 730;
  Width := 675;
end;

procedure TOpsiLinuxInstallerBaseForm.FormActivate(Sender: TObject);
begin
  // set background image
  BackgroundImage.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + BackgroundImageFileName);
  BackgroundImage.BorderSpacing.Top := 10;

  SetBasics(Sender as TForm);
end;

end.
