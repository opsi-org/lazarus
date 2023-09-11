unit OpsiLinuxInstaller_BaseForm;

// Base form for the welcome and query forms of the installer.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LayoutFunctions;

type
  TOpsiLinuxInstallerBaseForm = class(TForm)
    BackgroundImage: TImage;
    BtnNext: TButton;
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormActivate(Sender: TObject); virtual;
    procedure BtnNextClick(Sender: TObject); virtual; abstract;
    procedure SetInfoImageLayout(InfoImage: TImage);
  public
  const
    // position Panels with twice as much space to the left than to the right of the form
    //panelLeft := Round((Width - panelWidth) * 2 / 3);
    panelLeft = 143;
  private
    procedure SetConstantFormSize;
    procedure SetBackgroundImageLayout;
    procedure SetPanelLayout(Panel: TPanel);
    procedure SetLayout(Sender: TForm);
    // show hint on click of InfoImage
    procedure ShowHintOnClick(Sender: TObject);
  const
    // same width for all panels
    panelWidth = 440;
    // same size for all info images (squares)
    InfoImageSize = 22;
    // same background image for all forms
    BackgroundImageFileName = 'opsi.png';
    // same image for all infos
    InfoImageFileName = 'dialog-information.png';
  end;

implementation

// show hint on click of InfoImage
procedure TOpsiLinuxInstallerBaseForm.ShowHintOnClick(Sender: TObject);
begin
  Application.ActivateHint(TWinControl(Sender).ClientToScreen(Point(1, 1)), True);
end;

procedure TOpsiLinuxInstallerBaseForm.SetBackgroundImageLayout;
begin
  BackgroundImage.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) +
    BackgroundImageFileName);
  BackgroundImage.BorderSpacing.Top := 10;
end;

procedure TOpsiLinuxInstallerBaseForm.SetPanelLayout(Panel: TPanel);
begin
  Panel.Left := panelLeft;
  Panel.Width := panelWidth;
  Panel.Color := clForm;
end;

procedure TOpsiLinuxInstallerBaseForm.SetInfoImageLayout(InfoImage: TImage);
begin
  InfoImage.Width := InfoImageSize;
  InfoImage.Height := InfoImageSize;
  // set info image
  InfoImage.Picture.LoadFromFile(
    ExtractFilePath(ParamStr(0)) + InfoImageFileName);
  InfoImage.BorderSpacing.Left := 5;
  // Show info hints also on click of image
  InfoImage.OnClick := @ShowHintOnClick;
end;

procedure TOpsiLinuxInstallerBaseForm.SetLayout(Sender: TForm);
var
  compIndex: integer;
begin
  SetBackgroundImageLayout;

  for compIndex := 0 to Sender.ComponentCount - 1 do
  begin
    if (Sender.Components[compIndex].ClassName = 'TPanel') then
      SetPanelLayout(Sender.Components[compIndex] as TPanel)
    else
    if (Sender.Components[compIndex].ClassName = 'TImage') and
      (Pos('Info', Sender.Components[compIndex].Name) = 1) then
      SetInfoImageLayout(Sender.Components[compIndex] as TImage); // load info icon
  end;

  DecorateForm(Sender);
end;

procedure TOpsiLinuxInstallerBaseForm.SetConstantFormSize;
begin
  Height := 450;
  //Width := 730;
  Width := 675;
end;

procedure TOpsiLinuxInstallerBaseForm.FormCreate(Sender: TObject);
begin
  SetConstantFormSize;
end;

procedure TOpsiLinuxInstallerBaseForm.FormActivate(Sender: TObject);
begin
  SetLayout(Sender as TForm);
end;

end.
