unit OckImagestoDepot;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  //jwawinbase,
  OckWindows,
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  OckLinux,
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  OckMacOS,
  {$ENDIF DARWIN}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Process, oslog, FileUtil, LazFileUtils, opsiconnection, //LazProgInfo,
  LCLTranslator, ExtCtrls, ComCtrls

  {add more units if nedded};

type

  { TFormSaveImagesOnDepot }

  TFormSaveImagesOnDepot = class(TForm)
    ButtonClose: TButton;
    ButtonCopy: TButton;
    CheckBoxMountDepot: TCheckBox;
    DirectoryEditPathToDepot: TDirectoryEdit;
    EditPassword: TEdit;
    EditUser: TEdit;
    GroupBoxProgress: TGroupBox;
    GroupBoxPath: TGroupBox;
    GroupBoxMountDepot: TGroupBox;
    LabelInfo: TLabel;
    LabelPassword: TLabel;
    LabelUser: TLabel;
    PanelInfo: TPanel;
    ProgressBar: TProgressBar;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure CheckBoxMountDepotChange(Sender: TObject);
    procedure DirectoryEditPathToDepotChange(Sender: TObject);
    procedure DirectoryEditPathToDepotEnter(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    //procedure FormDestroy(Sender: TObject);
  private
    //function InitLogging(const LogFileName: String; MyLogLevel: integer): boolean;
    function SaveImagesOnDepot(const PathToDepot: String):boolean;
    //function SetRights(Path:String):boolean;
  public

  end;

var
  FormSaveImagesOnDepot: TFormSaveImagesOnDepot;


implementation

{$R *.lfm}

const
  PathDepotOnShare = '\var\lib\opsi\depot';
  {$IFDEF KIOSK_IN_AGENT} //if the kiosk is in the opsi-client-agent product
    PathKioskAppOnShare = '\opsi-client-agent\files\opsi\opsiclientkiosk\app';
  {$ELSE} //if the kiosk is a standalone opsi product
    PathKioskAppOnShare = '\opsi-client-kiosk\files\app';
  {$ENDIF KIOSK_IN_AGENT}
  CustomFolder = '\ock_custom';

resourcestring
  rsCouldNotSaveIcons = 'Could not save icons on depot.';
  rsCouldNotSaveScreenshots = 'Could not save screenshots on depot.';
  rsCouldNotUnmount = 'Could not unmount depot.';
  rsIsAnInvalidDir = '"%s" is an invalid directory.%sPlease set "%s" to a '
    +'valid directory.';
  rsImagesNotSaved = 'Images could not be saved on opsi depot. %sPlease'
    +' check if depot is mounted with write privileges.';
  rsImagesSaved = 'Images saved on';
  rsSettingRights = 'Setting rights...';
  rsDone = 'done';
  rsCopyIcons = 'Copy icons and screenshots...';
  rsMounting = 'Mounting';
  rsFinished = 'Copy process finished. Closing window...';

{ TFormSaveImagesOnDepot }

procedure TFormSaveImagesOnDepot.ButtonCopyClick(Sender: TObject);
var
  PathToDepot : String;
  User: String;
  AlreadyMounted :boolean;
  CopySuccess: boolean;
  i,LineNumber:integer;
begin
  CopySuccess := False;
  PathToDepot :=  SwitchPathDelims(Trim(DirectoryEditPathToDepot.Text),pdsSystem);
  {Mount opsi depot}
  if CheckBoxMountDepot.Checked then
  begin
    if {$IFDEF WINDOWS} IsDepotMounted(PathToDepot) {$ENDIF WINDOWS}
       {$IFDEF LINUX} IsDepotMounted(MountPoint) {$ENDIF LINUX}
       {$IFDEF DARWIN} IsDepotMounted(MountPoint) {$ENDIF DARWIN}
      then AlreadyMounted := True
    else
    begin
      AlreadyMounted := False;
      User := EditUser.Text;
      MountDepot(User, EditPassword.Text, PathToDepot);
      LabelInfo.Caption := rsMounting + ' ' + rsDone;
      Application.ProcessMessages;
    end;
  end;

  if {$IFDEF WINDOWS} IsDepotMounted(PathToDepot) {$ENDIF WINDOWS}
     {$IFDEF LINUX} IsDepotMounted(MountPoint) {$ENDIF LINUX}
     {$IFDEF DARWIN} IsDepotMounted(MountPoint) {$ENDIF DARWIN}
  then
  begin
    ProgressBar.Visible := True;
    LabelInfo.Caption := rsCopyIcons;
    ProgressBar.Position:= 20;
    Application.ProcessMessages;
    if {$IFDEF WINDOWS} SaveImagesOnDepot(PathToDepot) {$ENDIF WINDOWS}
       {$IFDEF LINUX} SaveImagesOnDepot(MountPoint) {$ENDIF LINUX}
       {$IFDEF DARWIN} SaveImagesOnDepot(MountPoint) {$ENDIF DARWIN}
    then
    begin
      LabelInfo.Caption := rsCopyIcons + ' ' + rsDone;
      Application.ProcessMessages;
      sleep(1000);
      CopySuccess := True;
      //LogDatei.log(SwitchPathDelims(PathDepotOnShare + PathKioskAppOnShare + CustomFolder, pdsUnix), LLDebug);
      //SetRights(SwitchPathDelims(PathDepotOnShare + PathKioskAppOnShare + CustomFolder, pdsUnix));
      ProgressBar.Position:= 80;
      Application.ProcessMessages;
    end;
  end
  else
  begin
    ShowMessage(Format(rsIsAnInvalidDir, [DirectoryEditPathToDepot.Text,
      LineEnding, GroupBoxPath.Caption]));
    CopySuccess := False;
    DirectoryEditPathToDepot.Font.Color:= clRed;
  end;

  if CheckBoxMountDepot.Checked and (not AlreadyMounted) then
  begin
    {$IFDEF WINDOWS}
    UmountDepot(PathToDepot);
    {$ENDIF WINDOWS}
    {$IFDEF LINUX}
    UmountDepot(MountPoint);
    {$ENDIF LINUX}
    {$IFDEF DARWIN}
    UmountDepot(MountPoint);
    {$ENDIF DARWIN}
     //LabelInfo.Caption := rsCouldNotUnmount;
  end;

  if CopySuccess then
  begin
   ProgressBar.Position:= 100;
   LabelInfo.Caption := rsFinished;
   Application.ProcessMessages;
   sleep(2000); //chance to see that copy process was successful before window is closed
   {for i := 5 downto 1 do
    begin
      LabelInfo.Caption := 'Window will be automatically closed in ' + IntToStr(i) + ' sec';
      Application.ProcessMessages;
      sleep(1000);
    end;}
   Close;
  end;

end;

procedure TFormSaveImagesOnDepot.CheckBoxMountDepotChange(Sender: TObject);
begin
  if CheckBoxMountDepot.Checked then GroupBoxMountDepot.Enabled := True
    else GroupBoxMountDepot.Enabled := False;
end;

procedure TFormSaveImagesOnDepot.DirectoryEditPathToDepotChange(Sender: TObject
  );
begin
  DirectoryEditPathToDepot.Font.Color:= clDefault;
end;

procedure TFormSaveImagesOnDepot.DirectoryEditPathToDepotEnter(Sender: TObject);
begin
  DirectoryEditPathToDepot.Font.Color:= clDefault;
end;

procedure TFormSaveImagesOnDepot.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  LabelInfo.Caption := '';
  ProgressBar.Position:= 0;
end;

procedure TFormSaveImagesOnDepot.FormCreate(Sender: TObject);
begin
   //InitLogging('images_to_depot ' + GetUserName_ +'.log', LLDebug);
   SetDefaultLang(GetDefaultLang);
end;

(*
procedure TFormSaveImagesOnDepot.FormDestroy(Sender: TObject);
begin
  FeeAndNil(LogDatei);
end;
*)

(*
function TFormSaveImagesOnDepot.SetRights(Path:String): boolean;
var
  OpsiConnection: TOpsiConnection;
  LineNumber: integer;
begin
  //LineNumber := MemoInfo.Lines.Add('Please wait while setting rights...');
  ProgressBar.Position := 50;
  LabelInfo.Caption := rsSettingRights;
  //Refresh;
  Application.ProcessMessages;
  //Refresh;
  //MemoInfo.Lines.Text := MemoInfo.Lines.Text+ ('Please wait while setting rights...');
  Result := False;
  try
    try
      OpsiConnection := TOpsiConnection.Create(False);
      LogDatei.log('ClientID :' + OpsiConnection.MyClientID +
      ' Service_URL :' + OpsiConnection.MyService_URL +
      ' Hostkey :' + OpsiConnection.MyHostkey +
      ' Error :' + OpsiConnection.MyError,LLDebug);
      ProgressBar.Position := 80;
      Application.ProcessMessages;
      OpsiConnection.OpsiData.initOpsiConf(OpsiConnection.MyService_URL,'jan','jan123');
      OpsiConnection.SetRights(path);
      Result := True;
      ProgressBar.Position := 100;
      LabelInfo.Caption := rsSettingRights + ' '+ rsDone;;
      Application.ProcessMessages;
      sleep(1000);
      LogDatei.log('SetRights done', LLInfo);
    except
      Result := False;
    end;
  finally
    OpsiConnection.Free;
  end;
end;
*)


function TFormSaveImagesOnDepot.SaveImagesOnDepot(const PathToDepot: String):boolean;
begin
  Result := False;
  LogDatei.log('Copy ' + OckPaths.FOnClient.FCustomSettings + ' to ' + OckPaths.FOnDepot.FCustomSettings, LLInfo);
  //if CopyDirTree(PathToIconsOnClient, PathToIconsOnDepot,[cffOverwriteFile, cffCreateDestDirectory]) then
  if Copy(OckPaths.FOnClient.FCustomSettings, OckPaths.FOnDepot.FCustomSettings) then
  begin
    LogDatei.log('Copy done', LLInfo);
    Result := True;
    Refresh;
  end
  else
  begin
    LogDatei.log('Images could not be saved on opsi depot. ' + PathToDepot +
      ' Possible solution: mount depot with write privileges.' ,LLDebug);
    ShowMessage(Format(rsImagesNotSaved, [LineEnding]));
    Result := False;
  end;
end;

procedure TFormSaveImagesOnDepot.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

(*function TFormSaveImagesOnDepot.InitLogging(const LogFileName:String; MyLogLevel:integer): boolean;
begin
  try
    //LogDatei.free;
    LogDatei := TLogInfo.Create;
    LogDatei.WritePartLog := False;
    LogDatei.WriteErrFile:= False;
    LogDatei.WriteHistFile:= False;
    LogDatei.CreateTheLogfile(LogFileName, False);
    LogDatei.LogLevel := MylogLevel;
    LogDatei.log(' ' + 'Version('+')'
      + ' starting at ' + DateTimeToStr(now), LLEssential);
    LogDatei.log('Initialize Logging', LLNotice);
    InitLogging := True;
  except
    InitLogging := False;
    LogDatei.log('Error while initialising logging. Calling Method: TFormOpsiClientKiosk.InitLogging',LLDebug);
  end;
end;*)


end.

