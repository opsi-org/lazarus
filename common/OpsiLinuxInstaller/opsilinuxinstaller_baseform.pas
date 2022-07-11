unit OpsiLinuxInstaller_BaseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LayoutFunctions;

type
  TOpsiLinuxInstallerBaseForm = class(TForm)
    BackgroundImage: TImage;
    BtnNext: TButton;
    procedure SetConstantFormSize;
    procedure FormCreate(Sender: TObject); virtual;
    procedure SetBackgroundImage;
    procedure FormActivate(Sender: TObject); virtual;
    procedure BtnNextClick(Sender: TObject); virtual; abstract;
    // make Panel settings and load background and info images
    procedure SetLayoutBasics(Sender: TForm);
    // make InfoImage settings
    procedure SetInfoBasics(InfoImage: TImage);
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
procedure TOpsiLinuxInstallerBaseForm.SetInfoBasics(InfoImage: TImage);
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
procedure TOpsiLinuxInstallerBaseForm.SetLayoutBasics(Sender: TForm);
var
  compIndex: integer;
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
      (Pos('Info', Sender.Components[compIndex].Name) = 1) then
      SetInfoBasics(Sender.Components[compIndex] as TImage); // load info icon
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

procedure TOpsiLinuxInstallerBaseForm.SetBackgroundImage;
begin
  BackgroundImage.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) +
    BackgroundImageFileName);
  BackgroundImage.BorderSpacing.Top := 10;
end;

procedure TOpsiLinuxInstallerBaseForm.FormActivate(Sender: TObject);
begin
  SetBackgroundImage;
  SetLayoutBasics(Sender as TForm);
end;

end.
