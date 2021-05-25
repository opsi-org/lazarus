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
  LazFileutils;

type


  (*
  TConfiguration = class(TPersistent)
  private
    Fconfig_version: string;
    { help to detect and handle changes of config file structure }
    //Fworkbench_share: string;
    Fworkbench_Path: string;
    //Fworkbench_mounted: boolean;
    Fconfig_filled: boolean;
    FregisterInFilemanager: boolean;
    Femail_address: string;
    FFullName: string;
    Fimport_libraries: TStrings;
    FpreInstallLines: TStrings;
    FpostInstallLines: TStrings;
    FpreUninstallLines: TStrings;
    FpostUninstallLines: TStrings;
    FPathToOpsiPackageBuilder: string;
    FCreateRadioIndex: integer;  // Create mode
    FBuildRadioIndex: integer;  // Build mode
    //FCreateQuiet: boolean;
    //FCreateBuild: boolean;
    //FCreateInstall: boolean;
    FUsePropDesktopicon: boolean;
    FUsePropLicenseOrPool: boolean;
    FProperties: TPProperties;
    FReadme_txt_templ: string;
    FShowCheckEntryWarning :boolean;
    FShow2StepMacSeletionWarn : boolean;
    //FtargetOS : TTargetOS;
    procedure SetLibraryLines(const AValue: TStrings);
    procedure SetPreInstallLines(const AValue: TStrings);
    procedure SetPostInstallLines(const AValue: TStrings);
    procedure SetPreUninstallLines(const AValue: TStrings);
    procedure SetPostUninstallLines(const AValue: TStrings);
    procedure SetProperties(const AValue: TPProperties);
  published
    property config_version: string read Fconfig_version write Fconfig_version;
    //property workbench_share: string read Fworkbench_share write Fworkbench_share;
    property workbench_Path: string read Fworkbench_Path write Fworkbench_Path;
    //property workbench_mounted: boolean read Fworkbench_mounted write Fworkbench_mounted;
    property config_filled: boolean read Fconfig_filled write Fconfig_filled;
    property registerInFilemanager: boolean
      read FregisterInFilemanager write FregisterInFilemanager;
    property email_address: string read Femail_address write Femail_address;
    property fullName: string read FFullName write FFullName;
    property PathToOpsiPackageBuilder: string
      read FPathToOpsiPackageBuilder write FPathToOpsiPackageBuilder;
    property import_libraries: TStrings read Fimport_libraries write SetLibraryLines;
    property preInstallLines: TStrings read FpreInstallLines write SetPreInstallLines;
    property postInstallLines: TStrings read FpostInstallLines write SetPostInstallLines;
    property preUninstallLines: TStrings read FpreUninstallLines
      write SetPreUninstallLines;
    property postUninstallLines: TStrings read FpostUninstallLines
      write SetPostUninstallLines;
    property CreateRadioIndex: integer read FCreateRadioIndex write FCreateRadioIndex;
    property BuildRadioIndex: integer read FBuildRadioIndex write FBuildRadioIndex;
    //property CreateQuiet: boolean read FCreateQuiet write FCreateQuiet;
    //property CreateBuild: boolean read FCreateBuild write FCreateBuild;
    //property CreateInstall: boolean read FCreateInstall write FCreateInstall;
    property UsePropDesktopicon: boolean read FUsePropDesktopicon
      write FUsePropDesktopicon;
    property UsePropLicenseOrPool: boolean read FUsePropLicenseOrPool
      write FUsePropLicenseOrPool;
    //property Properties: TPProperties read FProperties  write SetProperties;
    property Readme_txt_templ: string read FReadme_txt_templ write FReadme_txt_templ;
    property ShowCheckEntryWarning: boolean read FShowCheckEntryWarning
      write FShowCheckEntryWarning;
    property Show2StepMacSeletionWarn: boolean read FShow2StepMacSeletionWarn
      write FShow2StepMacSeletionWarn;

    procedure writeconfig;
    procedure readconfig;
  public
    { public declarations }
    constructor Create;
    destructor Destroy;
  end;
 *)

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
    constructor Create(AOwner: TComponent; btnonly : boolean = true);
    destructor Destroy;
  end;

implementation

constructor TComboButton.Create(AOwner: TComponent; btnonly : boolean = true);
var
  Picture : TPicture;
  SrcBmp : TBitmap;
begin
  panel := TPanel.Create(AOwner);
  cbox := TComboBox.Create(panel);
  btn := TSpeedButton.Create(panel);
  ImgList:= TImageList.Create(panel);
  button_only := btnonly;
  //inherited.
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
    Picture.LoadFromFile(AppendPathDelim(ExtractFileDir(Application.ExeName))+ 'ok2.png');
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

end.

