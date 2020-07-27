unit opsi_quick_install_unit_query;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, StrUtils;

type

  { TQuery }

  TQuery = class(TForm)
    BackgrImage: TImage;
    BtnNext: TButton;
    BtnBack: TButton;
    EditProxy: TEdit;
    EditRepo: TEdit;
    EditNoCache: TEdit;
    LabelNoCache: TLabel;
    LabelProxy: TLabel;
    LabelRepo: TLabel;
    PanelNoCache: TPanel;
    PanelProxy: TPanel;
    PanelRepo: TPanel;
    RadioBtnOpsi41: TRadioButton;
    RadioBtnNone: TRadioButton;
    RadioBtnOpsi42: TRadioButton;
    RadioBtnOtherRepo: TRadioButton;
    RadioBtnOtherProxy: TRadioButton;
    RadioBtnOtherNoCache: TRadioButton;
    RadioBtnOpsi41NoCache: TRadioButton;
    RadioBtnMyProxy: TRadioButton;
    RadioBtnOpsi42NoCache: TRadioButton;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  const
    baseURLOpsi41 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    baseURLOpsi42 = 'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';
  public

  end;

var
  Query: TQuery;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2, oslog, osfunclin, osLinuxRepository;

{$R *.lfm}

{ TQuery }

procedure TQuery.FormActivate(Sender: TObject);
var
  distroName, distroRelease: string;
begin
  // bring all panels to the same position (QuickInstall.panelLeft)
  AdjustPanelPosition(self);
  // always the same background (as in QuickInstall)
  BackgrImage.Picture.LoadFromFile(QuickInstall.BackgrImageFileName);

  // .../lazarus/common/oslog.pas
  LogDatei := TLogInfo.Create;
  LogDatei.CreateTheLogfile('opsi_quickinstall.log');

  // like function GetDefaultURL in osLinuxRepository
  RadioBtnOpsi41.Caption := baseURLOpsi41 + 'stable/';
  RadioBtnOpsi42.Caption := baseURLOpsi42 + 'stable/';
  //ShowMessage(IntToStr(Pos('7.', '127.8')));
  distroName := getLinuxDistroName;
  distroRelease := getLinuxDistroRelease;
  //ShowMessage(distroName);
  //ShowMessage(distroRelease);

  if (distroName = 'CentOS') and (Pos('7', distroRelease) = 1) then
  begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'CentOS_7/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'CentOS_7/';
  end
  else
  if distroName = 'Debian' then
  begin
    if Pos('8', distroRelease) = 1 then
    begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'Debian_8/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'Debian_8/';
    end
    else
    if Pos('9', distroRelease) = 1 then
    begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'Debian_9/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'Debian_9/';
    end
    else
    if Pos('10', distroRelease) = 1 then
    begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'Debian_10/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'Debian_10/';
    end;
  end
  else
  if distroName = 'openSUSE project' then
  begin
    if distroRelease = '15.1' then
    begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'openSUSE_Leap_15.1/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'openSUSE_Leap_15.1/';
    end
    else if distroRelease = '42.3' then
    begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'openSUSE_Leap_42.3/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'openSUSE_Leap_42.3/';
    end;
  end
  else
  if (distroName = 'RedHatEnterpriseServer') and (Pos('7', distroRelease) = 1) then
  begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'RHEL_7/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'RHEL_7/';
  end
  else
  if distroName = 'Ubuntu' then
  begin
    if distroRelease = '16.04' then
    begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'xUbuntu_16.04/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'xUbuntu_16.04/';
    end
    else if distroRelease = '18.04' then
    begin
      RadioBtnOpsi41.Caption := RadioBtnOpsi41.Caption + 'xUbuntu_18.04/';
      RadioBtnOpsi42.Caption := RadioBtnOpsi42.Caption + 'xUbuntu_18.04/';
    end;
  end;

  RadioBtnOpsi41NoCache.Caption := RadioBtnOpsi41.Caption;
  RadioBtnOpsi42NoCache.Caption := RadioBtnOpsi42.Caption;
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  QuickInstall.Close;
end;

procedure TQuery.BtnNextClick(Sender: TObject);
begin
  showForm(Query2, self);
  Query2.BtnBack.Left := BtnBack.Left;
  Query2.BtnBack.Top := BtnBack.Top;
  Query2.BtnNext.Left := BtnNext.Left;
  Query2.BtnNext.Top := BtnNext.Top;
end;

procedure TQuery.BtnBackClick(Sender: TObject);
begin
  showForm(QuickInstall, self);
end;

end.

