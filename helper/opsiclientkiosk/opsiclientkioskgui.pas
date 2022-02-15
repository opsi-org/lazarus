// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel, Jan Werner
// credits: http://www.opsi.org/credits/



unit opsiclientkioskgui;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DB, ExtendedNotebook, Forms, Controls, Process,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, Grids, DBGrids,
  DBCtrls,
  datadb,
  typinfo, installdlg, lcltranslator,
  ActnList, Menus, oslog, inifiles, Variants, Lazfileutils, Types,
  opsiconnection,
  osprocesses,
  OckUnique,
  progresswindow,
  ExtDlgs,
  lazproginfo,
  helpinfo,
  {$IFDEF WINDOWS}
    //CommCtrl,
    //jwawinbase,
    //DSiWin32,
    OckWindows,
    //ShellApi
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
   {add Linux specific units here}
   OckLinux,
   OckPasswordQuery,
  {$ENDIF LINUX}
  {$IFDEF DARWIN}
  {add MacOS specific units here}
   OckMacOS,
   OckPasswordQuery,
  {$ENDIF DARWIN}

  {more units if nedded}
  OckImagestoDepot;

type

  { TProductPanel }

  (* Class for creating the Tiles *)
  TProductPanel = class(TPanel)
    ShapeRoundSquare : TShape; //just for a nice look
    LabelAction: TLabel; //ProductID
    LabelName: TLabel; //name of product
    LabelState: TLabel; //Status of product e.g. installed, not installed, update
    ImageIcon : TImage; //Icon of the product
    procedure ProductPanelClick(Sender:TObject);
    procedure ProductPanelMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Scroll(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint; var Handled: boolean);
    procedure ProductPanelMouseEnter(Sender :TObject);
    procedure ProductPanelMouseLeave(Sender :TObject);
  private
    { private declarations }
    //ShortName:string;
    procedure LoadSkinPanel(SkinPath:string);
    procedure LoadSkinLabelAction(SkinPath:string);
    procedure SetIcon(ProductPanel: TProductPanel);
    procedure TrimLabelCaption(fLabel:TLabel);
  public
    { public declarations }
    ProductID : String;
    constructor Create(TheOwner: TWinControl);overload;
    destructor Destroy;override;
  end;

  { TPanels }

  TPanels = array of TProductPanel;

  { TFormOpsiClientKiosk }

  TFormOpsiClientKiosk = class(TForm)
     BitBtnSaveImages: TBitBtn;
     ButtonSoftwareReinstall: TButton;
 (*----------------------------*)
 (*         Attributes         *)
 (*----------------------------*)
    (* DataSources *)
    DataSourceProductDependencies: TDataSource;
    DataSourceProductData: TDataSource;
    DBMemoProductDescription: TDBMemo;
    DBMemoProductDescription1: TDBMemo;
    DBTextActionRequest: TDBText;
    ImageListButtons: TImageList;
    ImageLogo: TImage;
    LabelSoftwareActionRequest: TLabel;
    LabelPleaseWait: TLabel;
    OpenPictureDialogSetIcon: TOpenPictureDialog;
    PagePleaseWait: TPage;
    PanelSearchMask: TPanel;
    (* Head *)
    PanelTopImage: TPanel;//container for header components
    ImageHeader: TImage;
    LabelTitle: TLabel;
    (* ToolBar *)
    PanelToolbar: TPanel;//container for toolbar components
    ProgressBarPleaseWait: TProgressBar;
    SpeedButtonSearch: TSpeedButton;
     { Buttons to filter products }
    SpeedButtonAll: TSpeedButton;//show all products
    SpeedButtonUpdates: TSpeedButton;// show only products where an update is available
    SpeedButtonNotInstalled: TSpeedButton;//show products not installed
    SpeedButtonActions: TSpeedButton;//show products with action requests
     { Panel search }
    PanelSearch: TPanel;//container for search components
    EditSearch: TEdit;
    ImageViewmag: TImage;
    SpeedButtonClearSearchEdit: TSpeedButton;
     { Info }
    BitBtnHelp: TBitBtn;//show info about opsiclientkiosk
    (* Expert mode *)
    PanelExpertMode: TPanel;//container for expert mode components
    RadioGroupView: TRadioGroup;//toggle between list and tiles view
    BitBtnInstallNow: TBitBtn; //Set action requests
    SpeedButtonReload: TSpeedButton;//reload data from server
    (* Views *)
    PanelProducts: TPanel;//container for components showing product infos
    NotebookProducts: TNotebook;//container for toggling between views (eg. list or tiles)
     { Tile view }
    PageAllProductTiles: TPage;//showing product tiles
    ScrollBoxAllTiles: TScrollBox;
    FlowPanelAllTiles: TFlowPanel;//container for ProductPanels
     { detailed software view tiles }
    ButtonSoftwareBack: TButton;
    ButtonSoftwareInstall: TButton;
    ButtonSoftwareRemoveAction: TButton;
    ButtonSoftwareUninstall: TButton;
    ButtonSoftwareUpdate: TButton;
    DBTextSoftwareClientVerStr: TDBText;
    DBTextSoftwareVerStr: TDBText;
    ImageIconSoftware: TImage;
    ImageScreenShot: TImage;
    LabelSoftwareAdvice: TLabel;
    LabelSoftwareDescription: TLabel;
    LabelSoftwareInstalledVersion: TLabel;
    LabelSoftwareName: TLabel;
    LabelSoftwareRecentVersion: TLabel;
    PageSoftwareDetails: TPage;//showing details on products in tiles view mode
    PanelSoftwareHead: TPanel;
    PanelSoftwareScreenshot: TPanel;
    ScrollBoxSoftwarePage: TScrollBox;
     { List view }
    PageList: TPage;//showing products in list view
    DBGrid1: TDBGrid;
    DBComboBox1: TDBComboBox;
     { detailed view list}
    PanelProductDetail: TPanel;// showing product details in list view mode
    ExtendedNotebook1: TExtendedNotebook;
     {Tab details }
    TabSheet1: TTabSheet;
    PanelDetailsLables: TPanel;
    PanelDetailsDBText: TPanel;
    DBMemoAdvice: TDBMemo;
    DBMemoDescription: TDBMemo;
    DBTextInstalledVerStr: TDBText;
    DBTextVerStr: TDBText;
    LabelVerstr: TLabel;
    LabelDescription: TLabel;
    LabelAdvice: TLabel;
    LabelClientVerstr: TLabel;
     {Tab dependencies and priorities}
    TabSheet2: TTabSheet;
     {dependencies}
    PanelDependencies: TPanel;
    DBGrid2: TDBGrid;
      {priorities}
    PanelPriority: TPanel;
    LabelPriority: TLabel;
    DBTextPriority: TDBText;
    (* StatusBars *)
    StatusBar1: TStatusBar;

  (*----------------------------*)
  (*         Methods            *)
  (*----------------------------*)
    { BItBtn }
    procedure BitBtnHelpClick(Sender: TObject);
    procedure BitBtnSaveImagesClick(Sender: TObject);
    procedure BitBtnShowActionClick(Sender: TObject);
    procedure BitBtnInstallNowClick(Sender: TObject);
    procedure BitBtnToggleViewClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    { ButtonSoftware }
    procedure ButtonSoftwareBackClick(Sender: TObject);
    procedure ButtonSoftwareInstallClick(Sender: TObject);
    procedure ButtonSoftwareReinstallClick(Sender: TObject);
    procedure ButtonSoftwareRemoveActionClick(Sender: TObject);
    procedure ButtonSoftwareUninstallClick(Sender: TObject);
    procedure ButtonSoftwareUpdateClick(Sender: TObject);
    { DBComboBox1 }
    procedure DBComboBox1Change(Sender: TObject);
    procedure DBComboBox1Click(Sender: TObject);
    procedure DBComboBox1Enter(Sender: TObject);
    procedure DBComboBox1Exit(Sender: TObject);
    procedure DBComboBox1MouseEnter(Sender: TObject);
    { DBGrid1 }
    procedure DBGrid1CellClick(Column: TColumn);
    procedure DBGrid1ColExit(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1Enter(Sender: TObject);
    procedure DBGrid1Exit(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    { Form }
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    { RadioGroup }
    procedure GrouplistEnter(Sender: TObject);
    procedure ImageIconSoftwareMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageScreenShotMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RadioGroupViewSelectionChanged(Sender: TObject);
    { ScrollBoxAllTiles }
    procedure ScrollBoxAllTilesMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    { SpeedButtons }
    procedure SpeedButtonClearSearchEditClick(Sender: TObject);
    procedure SpeedButtonSearchClick(Sender: TObject);
    procedure SpeedButtonActionsClick(Sender: TObject);
    procedure SpeedButtonReloadClick(Sender: TObject);
    procedure SpeedButtonNotInstalledClick(Sender: TObject);
    procedure SpeedButtonExpertModeClick(Sender: TObject);
    procedure SpeedButtonUpdatesClick(Sender: TObject);
    procedure SpeedButtonAllClick(Sender: TObject);
    procedure SpeedButtonViewListClick(Sender: TObject);
    procedure SpeedButtonViewStoreClick(Sender: TObject);
    { EditSearch }
    procedure EditSearchChange(Sender: TObject);
    procedure EditSearchEnter(Sender: TObject);
   private
    { private declarations }
    InTileRebuild: boolean; //= False;
    SoftwareOnDemand : boolean;
    AdminMode : boolean;
    DisableTilesView: boolean;
    ShowingFirstTime : boolean;
    SelectedPanelIndex : integer;  //TileIndex e.g. Tag
    SelectedProduct : String; //ProductID
    FilteredProductIDs : TStringList;
    StringListDefaultIcons : TStringList;
    StringListCustomIcons :TStringList;
    StringListScreenshots :TStringList;
    StartUpDone : boolean;
    ArrayProductPanels: TPanels;
    ClientdMode : boolean;
    ClientID : string;
    SpeedButtonLastClicked : TSpeedButton;
    LastFilter : String;
    MinWidthStandardMode : Integer;
    MinWidthExpertMode   : Integer;
    procedure DeleteFormerImage(ImagePath:String);
    procedure InitSkin;
    function GetClientID(PathToConfigFile:string): string;
    //function RunAsAdmin(const Handle: DWord; const Path, Params: string
    //  ): Boolean;
    procedure SaveIconsAndScreenshotsLists;
    //function SaveImagesOnDepotNT: String;
    procedure SetPositionButtonsOnPanelToolbar;
    { Inits at Start }
    function InitLogging(const LogFileName, CallingMethod: string; MyLogLevel:integer): boolean;
    procedure InitDBGrids;
    function InstallNow(var aErrorMessage: string):boolean;
    procedure LoadSkinForTitle(SkinPath: string);
    procedure BuildProductTiles(var fArrayProductPanels:TPanels; const OwnerName:string);
    procedure LoadDataFromServer;
    procedure InitConnectionToServer;
    //procedure LoadDataFromServer;
    procedure InitDatabase;
     {Save StringList}
    procedure SaveStringListToFile(StringList: TStringList; Path: String);
    procedure LoadStringListFromFile(StringList:TstringList; Path: String);
    { Set views: List and Tiles }
    procedure SetView;
    procedure SetListView;
    procedure SetTilesView;
    { Show }
    procedure ShowSoftwareButtonsDependendOnState(const ProductPanel: TProductPanel);
    procedure ShowPagePleaseWait;
    { Product Details}
    procedure ShowProductDetails(ProductPanel: TProductPanel);
    { Search }
    procedure SearchProducts;
    { Set actions}
    procedure SetActionRequestTilesView(Request:String; Message:String;
      OnDemand:boolean);
    procedure SetActionRequestListView(Request: String; Message: String;
      OnDemand: boolean);
     function GetProductPanelByProductID(const ProductID:String):TProductPanel;
    { Reload data}
    procedure ReloadDataFromServer;
    procedure Terminate;
    { Icons }
    //procedure DeleteNotUsedImage(ImagePath:String; ImageList:TStrings);
    //procedure IconToProductID()
   public
    { public declarations }
  end;


var
  FormOpsiClientKiosk: TFormOpsiClientKiosk;
  //StartupDone: boolean;
  //ArrayAllProductTiles: TPanels;
  //ArraySearchPanelTiles: TPanels;
  //InTileRebuild: boolean = False;
  //LastOrderDirAsc: boolean = True;
  //LastOrderCol: string;
  //detail_visible: boolean = False;
  //skinpath: string;
  //preLogfileLogList: TStringList;


resourcestring
  {General Buttons and Labels}
  rsActRequest = 'Action request';
  rsActSetup = 'Setup';
  rsActUninstall = 'Uninstall';
  rsActNone = 'None';
  rsAction = 'Action';
  rsActions = 'Actions';
  rsDisabled = 'disabled';
  rsInstall = 'Install';
  rsInstalled = 'Installed';
  rsInstallNow = 'Install now';
  rsNotInstalled = 'Not installed';
  rsStateUnknown = 'Unknown';
  rsUpdate = 'Update';
  rsUpdates = 'Updates';
  rsViewList = 'List';
  rsViewTiles = 'Tiles';
  rsConnectedTo = 'Connected to';
  rsAs = 'as';
  rsON = 'on';
  rsDepot = 'depot';
  rsStoreActions = 'Store actions';
  rsBack = '<-- Back';
  rsAll = 'All';

  rsReload = 'Reload';
  //rsRequestDone = 'Request done. %s';
  rsLabelInfoLoadData = 'Please wait while communicating with OPSI web server ...';
  rsLabelInfoTiles = 'Please wait while building tiles.';
  rsLabelDataLoadTiles = 'Fill tiles';
  rsLabelDataLoadLoading ='Loading product data from server';
  rsLabelDataLoadCommunicating = 'Communicating with server ...';
  rsLabelDetailConnecting = 'Connecting to server';
  {Hints}
  rsInstallNowHint = 'Start the installation/update (uninstallation) of the selected products';
  rsStoreActionsHint =
    'Send the action requests to the server, show the resulting installations and ask for installation start.';
  {DBGrid}
  rsRequest = 'Request';
  rsStatus = 'Status';
  rsProductID = 'ID';
  rsProductName = 'Software';

  {Dialogs }
  rsNoActionsFound = 'No action requests found.';
  rsNoUpdatesFound = 'No updates found.';
  rsAllProductsInstalled = 'All available products installed!';
  rsNow = 'Now';
  rsNextEvent ='Next standard event (e.g. reboot)';
   {Install Dialog}
  rsInstallNowOrNextEvent = 'Install product now or at next standard event (e.g. reboot)?';
  rsWillInstallNow = ' will be installed now.';
  rsWillInstallNextEvent = ' will be installed at next standard event (e.g. reboot).';
  rsDoYouWantInstall = 'Do you want to install ';
   {Update Dialog}
  rsUpdateNowOrNextEvent = 'Update product now or at next standard event (e.g. reboot)?';
  rsWillUpdateNow = ' will be updated now.';
  rsWillUpdateNextEvent = ' will be updated at next standard event (e.g. reboot).';
  rsDoYouWantUpdate = 'Do you want to update ';
  {Uninstall Dialog}
 rsUninstallNowOrNextEvent = 'Uninstall product now or at next standard event (e.g. reboot)?';
 rsWillUninstallNow = ' will be uninstalled now.';
 rsWillUninstallNextEvent = ' will be uninstalled at next standard event (e.g. reboot).';
 rsDoYouWantUninstall = 'Do you want to uninstall ';
 rsNo = 'No';
 rsYes = 'Yes';
 rsCancel = 'Cancel';
 rsInstallationFinished = 'Installation/update finished.';
 rsUninstallationFinished = 'Uninstallation finished.';
 rsActionRemovedFor = 'Action removed for';
 rsCurrentUserNoAdmin = 'Current user has no admin privileges. Admin mode '
   +'disabled.%sSolution: Start opsi Client Kiosk (as user) with admin privileges.';
 rsAdminMode = 'Admin mode';
 rsErrorOnDemand = 'opsi is bussy please try it again later. Detailed error message: ';


implementation

{$R *.lfm}

const
  //color for StatusLabel
  clInstalled = clTeal;
  clUpdate = $000080FF;
  clNotInstalled = clGray; //$00FF8000;
  clUnknown = clRed;

function ActionRequestToLocale(actionRequest: string): string;
begin
  if actionRequest = 'setup' then
    Result := rsActSetup
  else if actionRequest = 'uninstall' then
    Result := rsActUninstall
  else if actionRequest = 'none' then
    Result := rsActNone
  else
    Result := 'unknown';
end;

function LocaleToActionRequest(localestr: string): string;
begin
  if localestr = rsActSetup then
    Result := 'setup'
  else if localestr = rsActUninstall then
    Result := 'uninstall'
  else if localestr = rsActNone then
    Result := 'none'
  else
    Result := 'unknown';
end;



constructor TProductPanel.Create(TheOwner: TWinControl);
begin
  try
    inherited Create(TheOwner);
    parent := theOwner;
    ShowHint := False;
    Width := 140;
    Height := 140;
    //FlowStyle := fsTopBottomLeftRight;
    //self.OnClick := ProductTileClick;
    //BorderStyle := bsSingle;
    BorderStyle := bsNone;
    //BorderWidth := 4;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    BorderSpacing.Around := 5;
    Color := clDefault;
    Font.Name := 'Arial';
    Font.Size := 10;
    Font.Color := clBlack;
    Font.Bold := True;
    Font.Italic := False;
    Font.Underline := False;
    self.OnMouseWheel := scroll;
    //LoadSkinPanel(Application.Location + 'default'+ PathDelim + 'skin' + PathDelim);


    //Shape (just for looking nice)
    ShapeRoundSquare := TShape.Create(self);
    with ShapeRoundSquare do begin
       Parent := self;
       Align := alNone;
       Height:= self.ClientHeight;
       Width:= self.ClientWidth;
       Shape:= stRoundSquare;
       Top:= 0;
       Left:= 0;
       OnClick := ProductPanelClick;//ProductTileChildClick;
       OnMouseWheel := scroll;
       OnMouseEnter := ProductPanelMouseEnter;
       OnMouseLeave := ProductPanelMouseLeave;
       //Brush.Color:= clWhite;
       //Brush.Style := bsSolid;
       //Visible:= True;
    end;


    //label Name
    LabelName := TLabel.Create(self);
    with LabelName do begin
      Parent := self;
      font.Style := [fsBold];
      font.Italic := self.Font.Italic;
      font.Underline := self.Font.Underline;
      Caption := 'name';
      ShowHint := True;
      WordWrap := False;
      AutoSize := False;
      Width := self.ClientWidth-10;
      Alignment := taCenter;
      Align := alNone;
      Top := 100;
      Left := 5;
      BorderSpacing.Around := 3;
      //BorderSpacing.Left := 10;
      //BorderSpacing.Right := 10;
      //BorderSpacing.InnerBorder := 5;
      OnClick := ProductPanelClick;//ProductTileChildClick;
      OnMouseWheel := scroll;
      OnMouseEnter := ProductPanelMouseEnter;
      OnMouseLeave := ProductPanelMouseLeave;
    end;


    //Label Action
    LabelAction := TLabel.Create(self);
    with LabelAction do begin
      Parent := self;
      Caption := 'Action: none';
      Color := clNone;
      Font.Name := 'Arial';
      Font.Size:= 9;
      Font.Color:= clBlack;
      Font.Italic := True;
      Font.Bold := False;
      Font.Underline := False;
      AutoSize:= False;
      Alignment := taCenter;
      Width := self.ClientWidth;
      Align := alNone;
      Top := LabelName.Top + LabelName.Height + 3;
      Left := 0;
      //LabelAction.BorderSpacing.Around := 3;
      OnClick := ProductPanelClick;//ProductTileChildClick;
      OnMouseWheel := scroll;
      OnMouseEnter := ProductPanelMouseEnter;
      OnMouseLeave := ProductPanelMouseLeave;
    end;
    //LoadSkinLabelAction(Application.Location + 'default'+ PathDelim + 'skin' +
      //PathDelim);


    //program icon
    ImageIcon := TImage.Create(self);
    with ImageIcon do
    begin
      Parent := self;
      //Picture.LoadFromFile(IconPath);
      AutoSize := False;
      Proportional := True;
      Width := 64;
      Height := 64;
      Center := True;
      Align:= alNone;
      Top:=35;
      Left:=38;
      Picture.LoadFromFile(PathsOnClient.FDefaultIcons + PathDelim + 'opsi-logo.png');
      //BorderSpacing.Around := 0;
      OnClick := ProductPanelClick;//ProductTileChildClick;
      OnMouseUp := ProductPanelMouseUp;
      OnMouseWheel := scroll;
      OnMouseEnter := ProductPanelMouseEnter;
      OnMouseLeave := ProductPanelMouseLeave;
    end;

    //label LabelState
    LabelState := TLabel.Create(self);
    with LabelState do begin
      Parent := self;
      Align := alNone;
      Caption := 'state';
      WordWrap := False;
      AutoSize:=False;
      Alignment := taCenter;
      Width := self.Width-20;
      Top := 15;
      Left:=20-ShapeRoundSquare.Pen.Width;
      Font.Color := clWhite;
      Name := 'LabelState';
      //BorderSpacing.Around := 3;
      OnClick := ProductPanelClick;//ProductTileChildClick;
      OnMouseWheel := scroll;
      OnMouseEnter := ProductPanelMouseEnter;
      OnMouseLeave := ProductPanelMouseLeave;
    end;

  except
    on e: Exception do
    begin
      logdatei.log('Exception TProductPanel.Create', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E, LLError);
    end;
  end;
end;

destructor TProductPanel.Destroy;
begin
  try
    try
      FreeAndNil(LabelAction);
      FreeAndNil(LabelName);
      FreeAndNil(ImageIcon);
      FreeAndNil(ShapeRoundSquare);
      FreeAndNil(LabelState);
    except
      on e: Exception do
      begin
        logdatei.log('Exception TProductPanel.Destroy', LLError);
        logdatei.log('Exception: ' + E.message, LLError);
        logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
        logdatei.log_exception(E, LLError);
      end;
    end;
  finally
    //LogDatei.log('TProductPanel.Destroy', LLDebug2);
    inherited Destroy;
  end;

end;


procedure TProductPanel.Scroll(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  oldpos: integer;
begin
  oldpos := FormOpsiClientKiosk.ScrollBoxAllTiles.VertScrollBar.Position;
  FormOpsiClientKiosk.ScrollBoxAllTiles.VertScrollBar.Position :=
    oldpos + ((WheelDelta * -1) div 10);
  logdatei.log('Scroll WheelDelta: ' + IntToStr(WheelDelta div 10), LLDebug2);
end;

procedure TProductPanel.ProductPanelMouseEnter(Sender: TObject);
var
  ProductPanel : TProductPanel;
begin
  if not FormOpsiClientKiosk.InTileRebuild then
  begin
    //tileindex := TControl(Sender).Parent.Tag; //type convertion to TControl (all visual components are of this type), parent is the Panel
    //Case TControl(Sender).Parent.GetNamePath of
    //  'FlowPanelAllTiles':
    //end;
    //ArrayAllProductTiles[tileindex].ShapeRoundSquare.Brush.Color:=$00FEEFD3;
    ProductPanel := TControl(Sender).Parent as TProductPanel;
    ProductPanel.ShapeRoundSquare.Brush.Color:=$00FEEFD3;
    //former_selected_tile := tileindex;
  end;
end;


procedure TProductPanel.ProductPanelMouseLeave(Sender: TObject);
var
  ProductPanel : TProductPanel;
begin
  if not FormOpsiClientKiosk.InTileRebuild then
  begin
    //tileindex := TControl(Sender).Parent.Tag; //type convertion to TControl (all visual components are of this type), parent is the Panel
    //ArrayAllProductTiles[tileindex].ShapeRoundSquare.Brush.Color:=clWhite;
    ProductPanel := TControl(Sender).Parent as TProductPanel;
    ProductPanel.ShapeRoundSquare.Brush.Color:=clWhite;
    //former_selected_tile := tileindex;
  end;
end;

procedure TProductPanel.LoadSkinPanel(SkinPath: string);
var
  myini: TInifile;
begin
  if FileExists(skinpath + 'opsiclientkiosk.ini') then
  begin
    LogDatei.Log('loading skin for ' + ProductID + ' from: ' + skinpath + 'opsiclientkiosk.ini', LLDebug);
    myini := TIniFile.Create(skinpath + 'opsiclientkiosk.ini');
    Color := StringToColor(myini.ReadString('Tile', 'Color', 'clDefault'));
    Font.Name := myini.ReadString('Tile', 'FontName', 'Arial');
    Font.Size := myini.ReadInteger('Tile', 'FontSize', 10);
    Font.Color := StringToColor(myini.ReadString('Tile', 'FontColor', 'clBlack'));
    Font.Bold := StrToBool(myini.ReadString('Tile', 'FontBold','False'));
    Font.Italic := StrToBool(myini.ReadString('Tile', 'FontItalic','False'));
    Font.Underline := StrToBool(myini.ReadString('Tile','FontUnderline', 'False'));
    Width := myini.ReadInteger('Tile', 'Width', 140);
    Height := myini.ReadInteger('Tile', 'Height', 140);
    myini.Free;
  end;
end;

procedure TProductPanel.LoadSkinLabelAction(SkinPath: string);
var
  myini: TInifile;
begin
  if FileExists(skinpath + 'opsiclientkiosk.ini') then
  begin
    myini := TIniFile.Create(skinpath + 'opsiclientkiosk.ini');
    with LabelAction do
    begin
      Color := StringToColor(myini.ReadString('LabelAction', 'Color', 'clNone'));
      Font.Name := myini.ReadString('LabelAction', 'FontName', 'Arial');
      Font.Size := myini.ReadInteger('LabelAction', 'FontSize', 10);
      Font.Color := StringToColor(myini.ReadString('LabelAction', 'FontColor', 'clBlack'));
      Font.Bold := StrToBool(myini.ReadString('LabelAction', 'FontBold','False'));
      Font.Italic := StrToBool(myini.ReadString('LabelAction', 'FontItalic','True'));
      Font.Underline := StrToBool(myini.ReadString('LabelAction','FontUnderline', 'False'));
      Top := LabelName.Top + LabelName.Height + 3;
      Left := 0;
      Width := self.Width;
      Caption := 'LabelAction';
      //Width := self.Height-4;
    end;
    myini.Free;
  end;
end;

procedure TFormOpsiClientKiosk.ShowProductDetails(ProductPanel: TProductPanel);
begin
  try
    //ProductPanel := TControl(Sender).Parent as TProductPanel;
    //TileIndex := TControl(Sender).Parent.Tag;
    //FormOpsiClientKiosk.SelectedPanelIndex := ProductPanel.Tag;
    //pid := ArrayAllProductTiles[TileIndex].ProductID;
    //FormOpsiClientKiosk.SelectedProduct := ProductPanel.ProductID;
    //DataModuleOCK.SQLTransaction.StartTransaction;
    //sqltext := DataModuleOCK.SQLQueryProductData.SQL.Text;
    //DataModuleOCK.SQLQueryProductData.Open;
    DataModuleOCK.SQLQueryProductData.First;
    //gefunden := DataModuleOCK.SQLQueryProductData.Locate('ProductID', VarArrayOf([pid]),[loCaseInsensitive]);
    if DataModuleOCK.SQLQueryProductData.Locate('ProductID',
       VarArrayOf([ProductPanel.ProductID]), [loCaseInsensitive]) then
    begin
      PanelProductDetail.Height := 0;
      NotebookProducts.PageIndex := 2;
      PanelExpertMode.Visible := False;
      PanelToolbar.Visible := False;
      LabelSoftwareName.Caption := DataModuleOCK.SQLQueryProductData.FieldByName('ProductName').AsString; //ArrayAllProductTiles[SelectedProductIndex].LabelName.Caption;
      ImageIconSoftware.Picture:= ProductPanel.ImageIcon.Picture; //ArrayAllProductTiles[SelectedProductIndex].ImageIcon.Picture;
      if FileExists(PathsOnClient.FCustomScreenShots + StringListScreenshots.Values[ProductPanel.ProductID]) then
       ImageScreenShot.Picture.LoadFromFile
         (PathsOnClient.FCustomScreenShots + StringListScreenshots.Values[ProductPanel.ProductID]);
       { View Buttons dependend on state}
      ShowSoftwareButtonsDependendOnState(ProductPanel);
    end;
  finally
    //DataModuleOCK.SQLQueryProductData.Close;
  end;
end;

procedure TProductPanel.SetIcon(ProductPanel:TProductPanel);
var
  FilePath:String;
  Index : integer;
  gefunden :boolean;
  FormerImageName:string;
begin
  gefunden := False;
  if FormOpsiClientKiosk.OpenPictureDialogSetIcon.Execute then
  begin
    with FormOpsiClientKiosk do
    begin
      { former image }
      FormerImageName := StringListCustomIcons.Values[ProductPanel.ProductID];
      { load and save current image }
      FilePath := OpenPictureDialogSetIcon.FileName;
      ProductPanel.ImageIcon.Picture.LoadFromFile(FilePath);
      ProductPanel.ImageIcon.Picture.SaveToFile(PathsOnClient.FCustomIcons + PathDelim + ExtractFileName(FilePath));
      StringListCustomIcons.Values[ProductPanel.ProductID] := ExtractFileName(FilePath);
      { delete former image if not used for other products }
      if not ContainsStr(StringListCustomIcons.Text, FormerImageName) then
        DeleteFormerImage(PathsOnClient.FCustomIcons+PathDelim+FormerImageName);
    end;
  end;
end;

procedure TProductPanel.TrimLabelCaption(fLabel:TLabel);
var
  sText: String;
begin
  sText := fLabel.Caption;
  if fLabel.Canvas.TextWidth(sText) >= fLabel.ClientWidth-6 then
  begin
    { Get maximal length of string which fits in Sender}
    //s := '';
    while ((fLabel.Canvas.TextWidth(sText) +
      fLabel.Canvas.TextWidth('...')) > (fLabel.ClientWidth-6)) do
        Delete(sText, Length(sText), 1);
    fLabel.Caption := sText+('...');
  end;
end;

procedure TProductPanel.ProductPanelClick(Sender: TObject);
var
  //TileIndex :integer;
  //pid : String;
  gefunden : boolean;
  sqltext : String;
  ProductPanel: TProductPanel;
begin
  ProductPanel := TControl(Sender).Parent as TProductPanel;
    //TileIndex := TControl(Sender).Parent.Tag;
  FormOpsiClientKiosk.SelectedPanelIndex := ProductPanel.Tag;
    //pid := ArrayAllProductTiles[TileIndex].ProductID;
  FormOpsiClientKiosk.SelectedProduct := ProductPanel.ProductID;
  FormOpsiClientKiosk.ShowProductDetails(ProductPanel);
end;

procedure TProductPanel.ProductPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ProductPanel: TProductPanel;
begin
  if (FormOpsiClientKiosk.AdminMode and (Button = mbRight)) then
  begin
    ProductPanel := TControl(Sender).Parent as TProductPanel;
    FormOpsiClientKiosk.SelectedPanelIndex := ProductPanel.Tag;
    FormOpsiClientKiosk.SelectedProduct := ProductPanel.ProductID;
    SetIcon(ProductPanel);
  end;
end;


procedure TFormOpsiClientKiosk.BuildProductTiles(var fArrayProductPanels:TPanels; const OwnerName:string);
var
  counter, i : integer;
  state, ProductID: string;
begin
  try
    inTileRebuild := True;
    logdatei.log('BuildProductTiles start', LLDebug2);

    { ProgressBar Init}
    with FormProgressWindow do
    begin
      ProgressbarDetail.Position := 0;
      LabelInfo.Caption:= rsLabelInfoTiles;
      LabelDataload.Caption := rsLabelDataLoadTiles;
      ProgressBarPleaseWait.Step := 1;
      ProgressBarPleaseWait.Max := 2*ProgressBarPleaseWait.Position + DataModuleOCK.SQLQueryProductData.RecordCount;
      ProgressBarDetail.Step := 1;
      ProgressBarDetail.Max := ProgressBarPleaseWait.Position + DataModuleOCK.SQLQueryProductData.RecordCount;
    end;
    Application.ProcessMessages;

    { if array exists free content and set array length 0 }
    try
      counter := length(fArrayProductPanels);
      if counter > 0 then
        for i := 0 to counter - 1 do
        begin
          fArrayProductPanels[i].Visible := False;
          fArrayProductPanels[i].Free;
          fArrayProductPanels[i] := nil;
        end;
      SetLength(fArrayProductPanels, 0);
    except
      on e: Exception do
      begin
        logdatei.log('Exception BuildProductTiles: ProductTilesArray[i].Free;',
          LLError);
        logdatei.log('Exception: ' + E.message, LLError);
        logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
        logdatei.log_exception(E, LLError);
      end;
    end;

    logdatei.log('BuildProductTiles from db start', LLDebug2);
    counter := 0;
    while not (DataModuleOCK.SQLQueryProductData.EOF) and (counter < 30) do
    begin
      ProductID := DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString;
      { Progressbar progress}
      FormProgressWindow.LabelDataLoadDetail.Caption := ProductID;
      FormProgressWindow.ProgressbarDetail.StepIt;
      FormProgressWindow.ProgressBar1.StepIt;
      Application.ProcessMessages;
      {Fill ArrayProductPanels }
      FilteredProductIDs.Add(ProductID);
      SetLength(fArrayProductPanels, counter + 1);
      //FlowPanelAllTiles.InsertControl(TProductPanel.Create(
        //FormOpsiClientKiosk.FindComponent(OwnerName) as TFlowPanel));
      fArrayProductPanels[counter] := TProductPanel.Create(
        FormOpsiClientKiosk.FindComponent(OwnerName) as TFlowPanel);
      fArrayProductPanels[counter].Name:= 'Panel' + IntToStr(counter);
      fArrayProductPanels[counter].ProductID := ProductID;
      if DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString
        <> '' then fArrayProductPanels[counter].LabelAction.Caption := rsAction +': '
          + DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString
      else fArrayProductPanels[counter].LabelAction.Caption :=
             DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString;
      fArrayProductPanels[counter].LabelName.Caption :=
        DataModuleOCK.SQLQueryProductData.FieldByName('ProductName').AsString;
      fArrayProductPanels[counter].TrimLabelCaption(fArrayProductPanels[counter].LabelName);
      fArrayProductPanels[counter].LabelName.Hint :=
        DataModuleOCK.SQLQueryProductData.FieldByName('ProductName').AsString;
      fArrayProductPanels[counter].Hint :=
        DataModuleOCK.SQLQueryProductData.FieldByName('ProductName').AsString;
      {Set Product Icons}
      if FileExists(PathsOnClient.FCustomIcons + PathDelim + StringListCustomIcons.Values[ProductID]) then
        fArrayProductPanels[counter].ImageIcon.Picture.LoadFromFile(
          PathsOnClient.FCustomIcons + PathDelim + StringListCustomIcons.Values[ProductID])
      else
        if FileExists(PathsOnClient.FDefaultIcons + PathDelim + StringListDefaultIcons.Values[ProductID]) then
          fArrayProductPanels[counter].ImageIcon.Picture.LoadFromFile(
            PathsOnClient.FDefaultIcons + PathDelim + StringListDefaultIcons.Values[ProductID]);
       state := DataModuleOCK.SQLQueryProductData.FieldByName(
        'InstallationStatus').AsString;
      if state = 'installed' then
      begin
        if DataModuleOCK.SQLQueryProductData.FieldByName('UpdatePossible').AsBoolean then
        begin
          fArrayProductPanels[counter].LabelState.Caption := rsUpdate;
          fArrayProductPanels[counter].LabelState.Color := clUpdate;
        end
        else
        begin
          fArrayProductPanels[counter].LabelState.Caption := rsInstalled;
          fArrayProductPanels[counter].LabelState.Color := clInstalled;
        end;
      end
      else if (state = 'not_installed') or (state = 'not installed') or (state =
       '') then
      begin
        fArrayProductPanels[counter].LabelState.Caption := rsNotInstalled;
        fArrayProductPanels[counter].LabelState.Color := clNotInstalled;
      end
      else if state = 'unknown' then
      begin
        fArrayProductPanels[counter].LabelState.Caption := rsStateUnknown;
        fArrayProductPanels[counter].LabelState.Color := clUnknown;
      end;
      fArrayProductPanels[counter].Tag := counter;
      Inc(counter);
      DataModuleOCK.SQLQueryProductData.Next;
    end;
    DataModuleOCK.SQLQueryProductData.First;
  finally
    //DataModuleOCK.SQLQueryProductData.Close;
    //DataModuleOCK.SQLTransaction.Commit;
    inTileRebuild := False;
    logdatei.log('BuildProductTiles stop', LLDebug2);
  end;
end;

procedure TFormOpsiClientKiosk.LoadDataFromServer;
var
    ConfigState:TStringList;
begin
    { Load data from server }
    LogDatei.log('Load data from server.', LLNotice);
    FormProgressWindow.LabelDataLoadDetail.Caption := rsLabelDataLoadLoading;
    FormProgressWindow.ProgressBar1.StepIt;
    FormProgressWindow.ProgressBarDetail.Position := 100;
    //FormProgressWindow.Repaint;
    Application.ProcessMessages;//FormProgressWindow.ProcessMess;

    OCKOpsiConnection.GetProductInfosFromServer;
    //ConfigState := TSTringList.Create;
    ConfigState := OCKOpsiConnection.GetConfigState('software-on-demand.installation-now-button');
    //ShowMessage(ConfigState.Text);
    SoftwareOnDemand := StrToBool(ConfigState.Strings[0]);
    ConfigState.Free;
    if not SoftwareOnDemand then
    begin
      if FormHelpInfo.LabelModeInfo.Caption = '' then
        FormHelpInfo.LabelModeInfo.Caption := rsInstallNow + ': ' + rsDisabled
      else
        FormHelpInfo.LabelModeInfo.Caption := FormHelpInfo.LabelModeInfo.Caption + ', ' + rsInstallNow + ': ' + rsDisabled
    end;
    if SoftwareOnDemand then
    begin
      BitBtnInstallNow.Caption := rsInstallNow;
      BitBtnInstallNow.Hint := rsInstallNowHint;
      BitBtnInstallNow.Visible:= True;
    end
    else BitBtnInstallNow.Visible:= False;


    ConfigState := OCKOpsiConnection.GetConfigState('software-on-demand.admin-mode');
    //ShowMessage(ConfigState.Text);
    AdminMode := StrToBool(ConfigState.Strings[0]);
    ConfigState.Free;
    if AdminMode and IsAdmin then
    begin
      if FormHelpInfo.LabelModeInfo.Caption = '' then
        FormHelpInfo.LabelModeInfo.Caption := rsAdminMode
      else
        FormHelpInfo.LabelModeInfo.Caption := FormHelpInfo.LabelModeInfo.Caption + ', ' + rsAdminMode;
      BitBtnSaveImages.Visible := True;
    end
    else
    begin
      BitBtnSaveImages.Visible := False;
      if AdminMode and not IsAdmin then
      begin
        ShowMessage(Format(rsCurrentUserNoAdmin, [LineEnding]));
        AdminMode := false;
      end;
    end;
    ConfigState := OCKOpsiConnection.GetConfigState('software-on-demand.disable-tilesview');
    //ShowMessage(ConfigState.Text);
    DisableTilesView := StrToBool(ConfigState.Strings[0]);
    ConfigState.Free;
    //FormProgressWindow.ProgressBarDetail.Position := 100;
    //Application.ProcessMessages;
end;

procedure TFormOpsiClientKiosk.InitDatabase;
begin
   { Initialize database and tables and copy opsi product data to database }
    LogDatei.log('Initialize database and tables and copy opsi product data to database.', LLNotice);
    DataModuleOCK := TDataModuleOCK.Create(nil);
    DataModuleOCK.CreateDatabaseAndTables;
    //if DataModuleOCK.SQLTransaction.Active then ShowMessage('Transaction active after Initdatabse!');
    //LogDatei.log('start OpsiProductsToDataset', LLNotice);
    DataModuleOCK.OpsiProductsToDataset;
    DataModuleOCK.LoadTableProductsIntoMemory;
    //if DataModuleOCK.SQLTransaction.Active then ShowMessage('Transaction active after OpsiProducts!');
    //DBGrid1.DataSource := DataSourceProductData;
    //DBGrid2.DataSource := DataSourceProductDependencies;
end;


procedure TFormOpsiClientKiosk.SaveStringListToFile(StringList:TStringList;Path:String);
begin
  try
    StringList.SaveToFile(Path);
  except
    LogDatei.log('Error while saving StringList to '
      + Path, LLDebug);
  end;
end;

procedure TFormOpsiClientKiosk.LoadStringListFromFile(StringList: TstringList;
  Path: String);
begin
  try
    StringList.LoadFromFile(Path);
  except
    LogDatei.log('Error while loading StringList from '
      + Path, LLDebug);
  end;
end;


procedure TFormOpsiClientKiosk.InitConnectionToServer;
begin
   { Init connection to Opsi-Server }
    LogDatei.log('Create connection to Opsi.', LLNotice);
    FormProgressWindow.LabelInfo.Caption := rsLabelInfoLoadData;
    FormProgressWindow.LabelDataLoad.Caption := rsLabelDataLoadCommunicating;
    FormProgressWindow.LabelDataLoadDetail.Caption := rsLabelDetailConnecting;
    FormProgressWindow.ProgressBar1.StepIt;
    FormProgressWindow.ProgressBarDetail.Position := 50;
    Application.ProcessMessages;
    try
      OCKOpsiConnection := TOpsiConnection.Create(ClientdMode,ClientID,'opsi-client-kiosk-' + ProgramInfo.Version);
    except
      LogDatei.log('Error no connection to Opsi.', LLError);
    end;
    FormProgressWindow.LabelDataLoad.Caption := rsConnectedTo + ' ' +
    OCKOpsiConnection.myservice_url + ' ' + rsAS + ' ' + OCKOpsiConnection.myclientid;
    StatusBar1.Panels[0].Text := rsConnectedTo + ' ' +
        OCKOpsiConnection.myservice_url + ' '+ rsAS+ ' ' + OCKOpsiConnection.myclientid
        + ' ' + rsON + ' ' + OCKOpsiConnection.MyDepotID;
    //if FormHelpInfo.CheckBoxShowDepot.Checked then
    //begin
    //   StatusBar1.Panels[0].Text := StatusBar1.Panels[0].Text + ' ' +
    //    rsON + ' ' + rsDepot + ' ' + OCKOpsiConnection.MyDepotID;
    //end;
    FormProgressWindow.ProgressBar1.StepIt;
    Application.ProcessMessages; //FormProgressWindow.ProcessMess;
end;

procedure TFormOpsiClientKiosk.SetActionRequestTilesView(Request:String; Message:String; OnDemand:boolean);
var
    ErrorMessage: string;
begin
  Screen.Cursor := crHourGlass;
  OCKOpsiConnection.SetActionRequest(SelectedProduct,Request); //to opsi server
  DataModuleOCK.SQLQueryProductData.Locate('ProductID',VarArrayOf([SelectedProduct]),[loCaseInsensitive]);
  //DataModuleOCK.SQLQueryProductData.Edit;
  DataSourceProductData.Edit;
  if OnDemand then
  begin
    { On Demand }
    ShowPagePleaseWait;
    if InstallNow(ErrorMessage) then
    begin
      DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString := '';
      { install or update }
      if Request = 'setup' then
      begin
        ButtonSoftwareInstall.Visible:= False;
        ButtonSoftwareReinstall.Visible:= True;
        ButtonSoftwareUninstall.Visible:= True;
        ArrayProductPanels[SelectedPanelIndex].LabelState.Caption := rsInstalled;
        ArrayProductPanels[SelectedPanelIndex].LabelState.Color := clInstalled;
        DataModuleOCK.SQLQueryProductData.FieldByName('InstallationStatus').AsString := 'installed';
        DataModuleOCK.SQLQueryProductData.FieldByName('InstalledVerStr').AsString :=
          DataModuleOCK.SQLQueryProductData.FieldByName('VersionStr').AsString;
         ShowMessage(rsInstallationFinished);
        //SQLProductData[] =
      end;
      { uninstall }
      if Request = 'uninstall' then
      begin
        ButtonSoftwareUninstall.Visible:= False;
        ButtonSoftwareReinstall.Visible:= False;
        ButtonSoftwareInstall.Visible:= True;
        DataModuleOCK.SQLQueryProductData.FieldByName('InstallationStatus').AsString := '';
        DataModuleOCK.SQLQueryProductData.FieldByName('InstalledVerStr').AsString := '';
        ArrayProductPanels[SelectedPanelIndex].LabelState.Caption := rsNotInstalled;
        ArrayProductPanels[SelectedPanelIndex].LabelState.Color := clNotInstalled;
        ShowMessage(rsUninstallationFinished);
      end;
      ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := '';
      NotebookProducts.PageIndex := 2;
    end
    else
    begin
     { error occured in onDemand }
     //DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString := '';
      ShowMessage(rsErrorOnDemand + ErrorMessage);
    end;
  end //onDemand = True
  else
  begin
    { next standard event }
    DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString := Request;// to local database
    ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := rsAction+': ' + Request;
    {$IFDEF DARWIN}
      NotebookProducts.PageIndex := 2;
    {$ENDIF DARWIN}
    ShowMessage(ArrayProductPanels[SelectedPanelIndex].LabelName.Caption + Message);
    //ShowSoftwareButtonsDependendOnState(ArrayProductPanels[SelectedPanelIndex]);
    ButtonSoftwareUninstall.Visible:= False;
    ButtonSoftwareReinstall.Visible:= False;
    ButtonSoftwareInstall.Visible:= False;
    ButtonSoftwareUpdate.Visible:= False;
    ButtonSoftwareRemoveAction.Visible := True;
  end;
  DataModuleOCK.SQLQueryProductData.Post;
  DataModuleOCK.SQLQueryProductData.Open;
  Screen.Cursor := crDefault;
end;

procedure TFormOpsiClientKiosk.SetActionRequestListView(Request:String; Message:String; OnDemand:boolean);
begin
  Screen.Cursor := crHourGlass;
  OCKOpsiConnection.SetActionRequest(SelectedProduct,Request); //to opsi server
  DataSourceProductData.Edit;
  if Request = 'none' then
  begin
    DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString := '';// to local database
    if not DisableTilesView then ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := '';
  end
  else
  begin
    DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString := Request;// to local database
    if not DisableTilesView then ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := rsAction+': ' + Request;
  end;
  if not DisableTilesView then ShowMessage(ArrayProductPanels[SelectedPanelIndex].LabelName.Caption + Message);
  DataModuleOCK.SQLQueryProductData.Post;
  DataModuleOCK.SQLQueryProductData.Open;
  Screen.Cursor := crDefault;
end;


function TFormOpsiClientKiosk.GetProductPanelByProductID(const ProductID: String
  ): TProductPanel;
var
  i : integer;
  found : boolean;
begin
  Result := nil;
  i := 0;
  while (ArrayProductPanels[i].ProductID <> ProductID)
   and (i < length(ArrayProductPanels)) do inc(i);
  if (i = length(ArrayProductPanels)-1)
    and (ArrayProductPanels[i].ProductID <> ProductID) then
  begin
    LogDatei.log('Product not found by ProductID: ' + ProductID, LLDebug);
    ShowMessage('Product not found by ProductID: ' + ProductID);
  end
  else
  begin
    SelectedProduct := ArrayProductPanels[i].ProductID;
    SelectedPanelIndex := i;
    Result := ArrayProductPanels[i];
  end;
end;

procedure TFormOpsiClientKiosk.SetView;
begin
  DataModuleOCK.SQLQueryProductData.Open;
  if (FormHelpInfo.CheckBoxExpertMode.Checked
    and (RadioGroupview.ItemIndex = RadioGroupView.Items.IndexOf(rsViewList))) or DisableTilesView
  then SetListView
  else SetTilesView;
  FormOpsiClientKiosk.Repaint;
  Application.ProcessMessages;
end;


procedure TFormOpsiClientKiosk.SetListView;
begin
  if not (DataModuleOCK.SQLQueryProductData.EOF and
    DataModuleOCK.SQLQueryProductData.BOF) then
  begin
    DBGrid1.Enabled:= True;
    DBComboBox1.Enabled := True;
    DataSourceProductData.Edit;
    DataModuleOCK.SQLQueryProductData.First;
  end;
  NotebookProducts.PageIndex:= 0;
end;

procedure TFormOpsiClientKiosk.SetTilesView;
var
  i: integer;
  //ProductPanel: TProductPanel;
begin
  //DataModuleOCK.SQLQueryProductData.Open;
  if not (DataModuleOCK.SQLQueryProductData.EOF and DataModuleOCK.SQLQueryProductData.BOF) then
  begin
    i := 0; //initialize counter for loop

    if DataModuleOCK.SQLQueryProductData.Filtered then
    { Set filtered products to visible }
    begin
      FilteredProductIDs.Clear;
      DataModuleOCK.SQLQueryProductData.First;
      while not DataModuleOCK.SQLQueryProductData.EOF do
      begin
        FilteredProductIDs.Add(DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString);
        DataModuleOCK.SQLQueryProductData.Next;
      end;
      FilteredProductIDs.Sort;
      for i := 0 to Length(ArrayProductPanels)-1 do
      begin
        If FilteredProductIDs.IndexOf(ArrayProductPanels[i].ProductID) <> -1  //ProductID within FilteredProductIDs?
         then ArrayProductPanels[i].Visible := True
        else ArrayProductPanels[i].Visible := False;
      end;
      //Alternative:
      {for i := 0 to FlowPanelAllTiles.ControlCount -1 do
      begin
        if FlowPanelAllTiles.ControlList.Items[i].Control is TProductPanel then
         with FlowPanelAllTiles.ControlList.Items[i].Control as TProductPanel do
         begin
           if FilteredProductIDs.IndexOf(ProductID) <> -1
            then Visible := True
            else Visible := False;
         end;
      end;}
    end
    else
    { Set all products to visible }
    begin
      for i := 0 to Length(ArrayProductPanels)-1 do
      begin
        ArrayProductPanels[i].Visible := True;
        ArrayProductPanels[i].Refresh;
      end;
      //Alternative:
      {for i := 0 to FlowPanelAllTiles.ControlCount -1 do
        if FlowPanelAllTiles.ControlList.Items[i].Control is TProductPanel then
          TProductPanel(FlowPanelAllTiles.ControlList.Items[i].Control).Visible := True;}
    end;
    DataModuleOCK.SQLQueryProductData.First;
    PanelProductDetail.Height:= 0;
    ScrollBoxAllTiles.VertScrollBar.Position:=0;
    //ScrollBoxAllTiles.VertScrollBar.Range:= FlowPanelAllTiles.ClientWidth;
    //ScrollBoxAllTiles.AutoScroll := False;
    //ScrollBoxAllTiles.AutoScroll := True;
    //ScrollBoxAllTiles.Repaint;
    NotebookProducts.PageIndex:= 1;
  end;
end;


procedure TFormOpsiClientKiosk.ShowSoftwareButtonsDependendOnState(
  const ProductPanel: TProductPanel);
begin
  if ProductPanel.LabelAction.Caption <> '' then
  begin
    ButtonSoftwareInstall.Visible := False;
    ButtonSoftwareReinstall.Visible := False;
    ButtonSoftwareUninstall.Visible := False;
    ButtonSoftwareRemoveAction.Visible:= True;
    //if SoftwareOnDemand then ButtonSoftwareUpdate.Visible := True
    // else ButtonSoftwareUpdate.Visible := False;
    ButtonSoftwareUpdate.Visible := False;
  end
  else
  begin
    if ProductPanel.LabelState.Caption = rsInstalled then
    begin
      ButtonSoftwareInstall.Visible := False;
      ButtonSoftwareUninstall.Visible := True;
      ButtonSoftwareRemoveAction.Visible:= False;
      ButtonSoftwareUpdate.Visible := False;
      ButtonSoftwareReinstall.Visible := True;
    end
    else
      if ProductPanel.LabelState.Caption = rsNotInstalled then
      begin
        ButtonSoftwareInstall.Visible := True;
        ButtonSoftwareReinstall.Visible := False;
        ButtonSoftwareUninstall.Visible := False;
        ButtonSoftwareRemoveAction.Visible:= False;
        ButtonSoftwareUpdate.Visible := False;
      end
      else
        if ProductPanel.LabelState.Caption = rsUpdate then
        begin
          ButtonSoftwareInstall.Visible := False;
          ButtonSoftwareReinstall.Visible := False;
          ButtonSoftwareUninstall.Visible := True;
          ButtonSoftwareRemoveAction.Visible:= False;
          ButtonSoftwareUpdate.Visible := True;
        end;
  end;
end;

procedure TFormOpsiClientKiosk.ShowPagePleaseWait;
begin
  NotebookProducts.PageIndex := 3;
  Refresh;
end;


{ TFormOpsiClientKiosk }


procedure TFormOpsiClientKiosk.RadioGroupViewSelectionChanged(Sender: TObject);
begin
  SetView;
end;

procedure TFormOpsiClientKiosk.ScrollBoxAllTilesMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  logdatei.log('ScrollBox1MouseWheel WheelDelta: ' + IntToStr(WheelDelta), LLDebug2);
end;

procedure TFormOpsiClientKiosk.SpeedButtonClearSearchEditClick(Sender: TObject);
begin
  EditSearch.Clear;
end;

procedure TFormOpsiClientKiosk.SpeedButtonSearchClick(Sender: TObject);
begin
   if PanelSearchMask.Visible then
   begin
     PanelSearchMask.Hide;
     EditSearch.Clear;
     //SetView;
   end
   else PanelSearchMask.Show;
end;

procedure TFormOpsiClientKiosk.SpeedButtonActionsClick(Sender: TObject);
begin
  EditSearch.Clear;
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  //DataModuleOCK.SQLQueryProductData.Filter := ' not ((ActionRequest = "") and (ActionRequest = "none"))';
  DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
  DataModuleOCK.SQLQueryProductData.Filter := 'ActionRequest <> ""';
  DataModuleOCK.SQLQueryProductData.Filtered := True;
  if (DataModuleOCK.SQLQueryProductData.EOF and DataModuleOCK.SQLQueryProductData.BOF) then
  begin
    DataModuleOCK.SQLQueryProductData.Filtered := False;
    //DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
    //DataModuleOCK.SQLQueryProductData.Filter := LastFilter;
    //DataModuleOCK.SQLQueryProductData.Filtered := True;
    //DBComboBox1.Enabled := False;
    //DBGrid1.Enabled:= False;
    ShowMessage(rsNoActionsFound);
    //SpeedButtonLastClicked.Down := True;
    SpeedButtonAll.Down := True;
  end;
  {else
  begin
    SpeedButtonActions.Down:= True;
    SetView;
    SpeedButtonLastClicked := SpeedButtonActions;
    LastFilter := DataModuleOCK.SQLQueryProductData.Filter;
  end;}
  SetView;
end;

procedure TFormOpsiClientKiosk.SpeedButtonReloadClick(Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    //if DataModuleOCK.SQLTransaction.Active then ShowMessage('Transaction active Button reload click!');
    ReloadDataFromServer;
    //FilterOnSearch;
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFormOpsiClientKiosk.Terminate;
begin
  Application.Terminate;
end;

{procedure TFormOpsiClientKiosk.DeleteNotUsedImage(ImagePath: String;
  ImageList: TStrings);
begin
  if ImageList.Values[ExtractFileName(ImagePath)] = '' then DeleteFileUTF8(ImagePath);
end;}

procedure TFormOpsiClientKiosk.DBGrid1TitleClick(Column: TColumn);
var
  direction: string;
begin
 { try
    direction := ' ASC';
    if LowerCase(lastOrderCol) = LowerCase(Column.FieldName) then
    begin
      if lastOrderDirAsc then
      begin
        direction := ' DESC';
        lastOrderDirAsc := False;
      end
      else
      begin
        direction := ' ASC';
        lastOrderDirAsc := True;
      end;
    end
    else
      lastOrderCol := Column.FieldName;
    DataModuleOCK.SQLQueryProductData.SQL.Clear;
    DataModuleOCK.SQLQueryProductData.SQL.Add('select * from products order by ' +
      Column.FieldName + direction);
    DataModuleOCK.SQLQueryProductData.Open;
  except
  end;}
end;

procedure TFormOpsiClientKiosk.FormDestroy(Sender: TObject);
var
  counter, i: integer;
begin
  logDatei.log('Closing ' + ProgramInfo.InternalName,LLNotice);
  FilteredProductIDs.Free;
  StringListDefaultIcons.Free;
  if AdminMode and IsAdmin then SaveIconsAndScreenshotsLists;
  StringListCustomIcons.Free;
  StringListScreenshots.Free;
  DataModuleOCK.Free;
  OCKOpsiConnection.Free;
  try
    counter := length(ArrayProductPanels);
    if counter > 0 then
      for i := 0 to counter - 1 do
      begin
        ArrayProductPanels[i].Free;
        ArrayProductPanels[i] := nil;
        logdatei.log('FreeAndNil(ProductTilesArray)', LLDebug2);
      end;
    SetLength(ArrayProductPanels, 0);
    ArrayProductPanels := nil;
  except
    on e: Exception do
    begin
      logdatei.log('Exception FormClose: ProductTilesArray[i].Free;', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E, LLError);
    end;
  end;
  LogDatei.log(ProgramInfo.InternalName + ' terminates', LLEssential);
  LogDatei.Close;
  LogDatei.Free;
end;

procedure TFormOpsiClientKiosk.GrouplistEnter(Sender: TObject);
begin
  PanelProductDetail.Height := 0;
end;

procedure TFormOpsiClientKiosk.ImageIconSoftwareMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  FilePath:String;
  FormerImageName:String;
begin
  if (AdminMode and (Button = mbRight)) then
  begin
    if OpenPictureDialogSetIcon.Execute then
    begin
      FormerImageName := StringListCustomIcons.Values[SelectedProduct];
      {load and save current image}
      FilePath := OpenPictureDialogSetIcon.FileName;
      LogDatei.log('Save image ' + PathsOnClient.FCustomIcons + ExtractFileName(FilePath), LLDebug);
      ImageIconSoftware.Picture.LoadFromFile(FilePath);
      ImageIconSoftware.Picture.SaveToFile(PathsOnClient.FCustomIcons+ExtractFileName(FilePath));
      StringListCustomIcons.Values[SelectedProduct] := ExtractFileName(FilePath);
      {delete former image if not in StringList hence is not used by another product}
      if not ContainsStr(StringListCustomIcons.Text, FormerImageName) then
        DeleteFormerImage(PathsOnClient.FCustomIcons+FormerImageName);
    end;
  end;
end;


procedure TFormOpsiClientKiosk.ImageScreenShotMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  FilePath:String;
  FormerImageName:String;
begin
  if (AdminMode and (Button = mbRight)) then
    if OpenPictureDialogSetIcon.Execute then
    begin
      { former image }
      FormerImageName := StringListScreenShots.Values[SelectedProduct];
      {load and save image}
      FilePath := OpenPictureDialogSetIcon.FileName;
      ImageScreenShot.Picture.LoadFromFile(FilePath);
      ImageScreenShot.Picture.SaveToFile(PathsOnClient.FCustomScreenShots+ExtractFileName(FilePath));
      StringListScreenShots.Values[SelectedProduct] := ExtractFileName(FilePath);
      if not ContainsStr(StringListScreenShots.Text, FormerImageName) then
        DeleteFormerImage(PathsOnClient.FCustomScreenShots+FormerImageName);
    end;
end;

procedure TFormOpsiClientKiosk.DBGrid1Exit(Sender: TObject);
begin
  //PanelProductDetail.Height := 0;
end;

procedure TFormOpsiClientKiosk.DBGrid1Enter(Sender: TObject);
begin
  //PanelProductDetail.Height := 0;
  //PanelProductDetail.Height:=185;
  //Showmessage('OnEnter');
end;


procedure TFormOpsiClientKiosk.SpeedButtonUpdatesClick(Sender: TObject);
begin
  EditSearch.Clear;
  //SpeedButtonUpdates.Down:= True;
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
  DataModuleOCK.SQLQueryProductData.Filter := 'UpdatePossible and InstallationStatus <> ""';
  DataModuleOCK.SQLQueryProductData.Filtered := True;
  if (DataModuleOCK.SQLQueryProductData.EOF and DataModuleOCK.SQLQueryProductData.BOF) then
  begin
    DataModuleOCK.SQLQueryProductData.Filtered := False;
    //DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
    //DataModuleOCK.SQLQueryProductData.Filter := LastFilter;
    //DataModuleOCK.SQLQueryProductData.Filtered := True;

    //DBComboBox1.Enabled := False;
    //DBGrid1.Enabled:= False;
    ShowMessage(rsNoUpdatesFound);
    SpeedButtonAll.Down:= True;
    //SetView;
  end;
  {else
  begin
    //SpeedButtonUpdates.Down:= True;
    SetView;
    //SpeedButtonLastClicked := SpeedButtonUpdates;
    //LastFilter := DataModuleOCK.SQLQueryProductData.Filter;
  end;}
  SetView;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareBackClick(Sender: TObject);
begin
  //DataModuleOCK.SQLQueryProductData.Close;
  //DataModuleOCK.SQLTransaction.Commit;
  ImageScreenShot.Picture.Clear;
  ArrayProductPanels[SelectedPanelIndex].ImageIcon.Picture := ImageIconSoftware.Picture;
  if FormHelpInfo.CheckBoxExpertMode.Checked then FormOpsiClientKiosk.PanelExpertMode.Visible := True;
  FormOpsiClientKiosk.PanelToolbar.Visible := True;
  FormOpsiClientKiosk.NotebookProducts.PageIndex:=1;
  //if (DataModuleOCK.SQLQueryProductData.EOF and DataModuleOCK.SQLQueryProductData.BOF) then
    //begin
      //DataModuleOCK.SQLQueryProductData.Filtered := False;
      //SpeedButtonAll.Down := True;
      //SetView;
    //end;
  SetView;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareInstallClick(Sender: TObject);
begin
  if SoftWareOnDemand then
  begin
    case QuestionDLG(rsInstall, rsInstallNowOrNextEvent,mtConfirmation,
                     [mrYes, rsNow, mrNo, rsNextEvent, mrCancel, rsCancel], 0)
    of
      mrYes: begin
               SetActionRequestTilesView('setup', rsWillInstallNow, True);
               //ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := 'Action: setup';
             end;
      mrNo: begin
              //ShowMessage('Please wait while sending request to server...');
              SetActionRequestTilesView('setup', rsWillInstallNextEvent, False);
              //ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := 'Action: setup';
            end;
    end;//case
  end//if SoftwareOndemand
  else
   if QuestionDLG(rsInstall,rsDoYouWantInstall + LabelSoftwareName.Caption + '?',
               mtConfirmation,[mrYes, rsYes, mrNo, rsNo], 0) = mrYes then
   begin
     SetActionRequestTilesView('setup', rsWillInstallNextEvent, False);
     //ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := 'Action: setup';
   end;
  {$IFDEF DARWIN}
    NotebookProducts.PageIndex := 2;//PanelToolbar.Visible:=True;
  {$ENDIF DARWIN}
  //PanelToolbar.Repaint;
  //Application.ProcessMessages;
  //FormOpsiClientKiosk.Repaint;
  //Application.ProcessMessages;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareReinstallClick(Sender: TObject);
begin
  ButtonSoftwareInstallClick(Sender);
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareRemoveActionClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  OCKOpsiConnection.SetActionRequest(SelectedProduct,'none'); //to opsi server
  DataSourceProductData.Edit;
  DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString := '';// to local database
  ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := '';
  ShowSoftwareButtonsDependendOnState(ArrayProductPanels[SelectedPanelIndex]);
  ShowMessage(rsActionRemovedFor + ' '
    + ArrayProductPanels[SelectedPanelIndex].LabelName.Caption);
  DataModuleOCK.SQLQueryProductData.Post;
  DataModuleOCK.SQLQueryProductData.Open;
  if (DataModuleOCK.SQLQueryProductData.EOF and
    DataModuleOCK.SQLQueryProductData.BOF) then
    begin
      DataModuleOCK.SQLQueryProductData.Filtered := False;
      DataModuleOCK.SQLQueryProductData.Locate('ProductID',
       VarArrayOf([SelectedProduct]), [loCaseInsensitive]);
      SpeedButtonAll.Down := True;
    end;
  Screen.Cursor := crDefault;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareUninstallClick(Sender: TObject);
begin
  if SoftWareOnDemand then
  begin
    case QuestionDLG(rsActUninstall,rsUninstallNowOrNextEvent, mtConfirmation,
                     [mrYes, rsNow, mrNo, rsNextEvent, mrCancel, rsCancel], 0)
    of
      mrYes: begin
               SetActionRequestTilesView('uninstall', rsWillUninstallNow, True);
               //ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := 'Action: uninstall';
             end;
      mrNo: begin
              //ShowMessage('Please wait while sending request to server...');
              SetActionRequestTilesView('uninstall', rsWillUninstallNextEvent, False);
              //ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := 'Action: uninstall';
            end;
    end;//case
  end//if SoftwareOndemand
  else
   if QuestionDLG(rsActUninstall, rsDoYouWantUninstall + LabelSoftwareName.Caption + '?',
               mtConfirmation, [mrYes, rsYes, mrNo, rsNo ], 0) = mrYes then
   begin
     SetActionRequestTilesView('uninstall', rsWillUninstallNextEvent, False);
     //ArrayProductPanels[SelectedPanelIndex].LabelAction.Caption := 'Action: uninstall';
   end;
  {$IFDEF DARWIN}
    NotebookProducts.PageIndex := 2;
  {$ENDIF DARWIN}
end;//procedure ButtonSoftwareUninstallClick

procedure TFormOpsiClientKiosk.ButtonSoftwareUpdateClick(Sender: TObject);
begin
  if SoftWareOnDemand then
  begin
    case QuestionDLG(rsUpdate,rsUpdateNowOrNextEvent,mtConfirmation,
                     [mrYes, rsNow, mrNo, rsNextEvent, mrCancel, rsCancel], 0)
    of
      mrYes: begin
               SetActionRequestTilesView('setup', rsWillUpdateNow, True);
             end;
      mrNo: begin
              //ShowMessage('Please wait while sending request to server...');
              SetActionRequestTilesView('setup', rsWillUpdateNextEvent,False);
            end;
    end;//case
  end//if SoftwareOndemand
  else
   if QuestionDLG(rsUpdate, rsDoYouWAntUpdate + LabelSoftwareName.Caption + '?',
               mtConfirmation,[mrYes, rsYes, mrNo, rsNo ], 0) = mrYes then
   begin
     SetActionRequestTilesView('setup', rsWillUpdateNextEvent, False);
   end;
  {$IFDEF DARWIN}
    NotebookProducts.PageIndex := 2;
  {$ENDIF DARWIN}
end;



procedure TFormOpsiClientKiosk.DBComboBox1Change(Sender: TObject);
var
  Product : TProductPanel;
begin
  //DataModuleOCK.SQLQueryProductData.Edit;
  if (DBComboBox1.Text <> '') and not (DataModuleOCK.SQLQueryProductData.EOF and
    DataModuleOCK.SQLQueryProductData.BOF) then
  begin
    if not DisableTilesView then Product := GetProductPanelByProductID(DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString)
    else SelectedProduct := DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString;

    if DBComboBox1.Text = 'setup' then SetActionRequestListView('setup', rsWillInstallNextEvent, False);
    //if DBComboBox1.Text = 'update' then SetActionRequest('setup', rsWillUpdateNextEvent, False);
    if DBComboBox1.Text = 'uninstall' then SetActionRequestListView('uninstall', rsWillUninstallNextEvent, False);
    if DBComboBox1.Text = 'none' then SetActionRequestListView('none', rsWillUpdateNextEvent, False);

    {DataModuleOCK.SQLQueryProductData.Post;
    //if DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString
      //<> '' then
      Product := GetProductPanelByProductID(DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString);
      Product.LabelAction.Caption := rsAction+': '
         + DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString;
    //DataModuleOCK.SQLQueryProductData.FieldByName('actionrequest').AsString := 'none';
    //DataModuleOCK.SQLQueryProductData.Edit;
    //DataSourceProductData.Edit; }
  end
  else if (DataModuleOCK.SQLQueryProductData.EOF and DataModuleOCK.SQLQueryProductData.BOF) then
   ShowMessage('Action could not be done because no product is available.');
  //DataSourceProductData.Edit;
end;

procedure TFormOpsiClientKiosk.DBComboBox1Click(Sender: TObject);
var
  action : string;
begin
  //DataModuleOCK.SQLQueryProductData.Edit;
  {action := DataModuleOCK.SQLQueryProductData.FieldByName('possibleAction').AsString;
  DBComboBox1.Items.Clear;
  DBComboBox1.Items.Add('none');
  DBComboBox1.Items.Add('setup');
  if (action <> 'setup') and (action <> '') then
    DBComboBox1.Items.Add(action);}
  //DataSourceProductData.Edit;
end;


procedure TFormOpsiClientKiosk.DBComboBox1Enter(Sender: TObject);
begin
  //DataModuleOCK.SQLQueryProductData.Edit;
  DataSourceProductData.Edit;
end;

procedure TFormOpsiClientKiosk.DBComboBox1Exit(Sender: TObject);
begin
  //DBGrid1.Repaint;
  if (DBComboBox1.Text <> '') and (not DataModuleOCK.SQLQueryProductData.EOF)
  then
  begin
    DataModuleOCK.SQLQueryProductData.Post;
  //DataModuleOCK.SQLQueryProductData.FieldByName('actionrequest').AsString := 'none';
    //DataModuleOCK.SQLQueryProductData.Edit;
    //DataSourceProductData.Edit;
  end
  else if DataModuleOCK.SQLQueryProductData.EOF
  then ShowMessage('Action could not be done because no product is available.');
  //DBComboBox1.Items.Clear;
end;

procedure TFormOpsiClientKiosk.DBComboBox1MouseEnter(Sender: TObject);
var
  action : string;
begin
  //DataModuleOCK.SQLQueryProductData.Edit;
  action := DataModuleOCK.SQLQueryProductData.FieldByName('possibleAction').AsString;
  DBComboBox1.Items.Clear;
  DBComboBox1.Items.Add('none');
  DBComboBox1.Items.Add('setup');
  if (action <> 'setup') and (action <> '') then
    DBComboBox1.Items.Add(action);
  //DataSourceProductData.Edit;
end;


procedure TFormOpsiClientKiosk.BitBtnShowActionClick(Sender: TObject);
begin
  EditSearch.Clear;
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  //DataModuleOCK.SQLQueryProductData.Filter := ' not ((ActionRequest = "") and (ActionRequest = "none"))';
  DataModuleOCK.SQLQueryProductData.Filter := 'ActionRequest <> ""';
  DataModuleOCK.SQLQueryProductData.Filtered := True;
  if DataModuleOCK.SQLQueryProductData.EOF then
  begin
    DBComboBox1.Enabled := False;
    DBGrid1.Enabled:= False;
  end
  else SetView;
end;

procedure TFormOpsiClientKiosk.BitBtnHelpClick(Sender: TObject);
begin
  FormHelpInfo.Show;
end;

procedure TFormOpsiClientKiosk.BitBtnSaveImagesClick(Sender: TObject);
var
  Output: string;
  PathToExe: string;
begin
  SaveIconsAndScreenshotsLists;
  //PathToExeFolder := TrimFilename(Application.Location + 'images_to_depot\images_to_depot.exe');
 {$IFDEF WINDOWS}
  FormSaveImagesOnDepot.Visible := True;
  //PathToExe := TrimFilename(Application.Location + 'images_to_depot\images_to_depot.exe');
  //RunCommand('cmd.exe',['/c', PathToExe],Output,[], swoHIDE);
  //if SystemAPI is TWindowsAPI then
    //(SystemAPI as TWindowsAPI).SaveImagesOnDepot(Application.Location, FormOpsiClientKiosk.Handle);
 {$ENDIF WINDOS}
 {$IFDEF LINUX}
 //if ShowingFirstTime then
   FormPasswordQuery.Visible := True
 //else
   //FormSaveImagesOnDepot.Visible := True;
 //PathToExe := TrimFilename(Application.Location + 'images_to_depot\images_to_depot');
 //RunCommand('/bin/sh',['-c', './' + PathToExe],Output,[], swoHIDE);
 {$ENDIF LINUX}
 {$IFDEF DARWIN}
 //if ShowingFirstTime then
   FormPasswordQuery.Visible := True
 //else
   //FormSaveImagesOnDepot.Visible := True;
 //PathToExe := TrimFilename(Application.Location + 'images_to_depot\images_to_depot');
 //RunCommand('/bin/sh',['-c', './' + PathToExe],Output,[], swoHIDE);
 {$ENDIF DARWIN}
 end;

procedure TFormOpsiClientKiosk.BitBtnInstallNowClick(Sender: TObject);
begin
  screen.Cursor := crHourGlass;
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  DataModuleOCK.SQLQueryProductData.Filter := 'ActionRequest  <> ""';
  DataModuleOCK.SQLQueryProductData.Filtered := True;
  DataModuleOCK.SQLQueryProductData.First;
  if not (DataModuleOCK.SQLQueryProductData.EOF and DataModuleOCK.SQLQueryProductData.BOF) then
  begin
    try
      installdlg.Finstalldlg.Memo1.Clear;
      while not DataModuleOCK.SQLQueryProductData.EOF do
      begin
        //OCKOpsiConnection.setActionrequest(DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString,
          //DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString);
        installdlg.Finstalldlg.Memo1.Append(
          DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString
          + ': ' +
          DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString);

        DataModuleOCK.SQLQueryProductData.Next;
      end;
      installdlg.Finstalldlg.SoftwareOnDemand:= SoftwareOnDemand;
      //installdlg.Finstalldlg.Memo1.Text := OCKOpsiConnection.getActionrequests.Text;
      if installdlg.Finstalldlg.Memo1.Text = '' then
         installdlg.Finstalldlg.Memo1.Text := rsNoActionsFound;
      installdlg.Finstalldlg.Show;
    finally
      SpeedButtonActions.Down:= True;
      SetView;
      Screen.Cursor := crDefault;
    end;
  end
  else
  begin
    DataModuleOCK.SQLQueryProductData.Filtered := False;
    ShowMessage(rsNoActionsFound);
    SpeedButtonAll.Down := True;
    SetView;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormOpsiClientKiosk.BitBtnToggleViewClick(Sender: TObject);
begin
  if NotebookProducts.PageIndex = 1 then NotebookProducts.PageIndex := 0
    else if NotebookProducts.PageIndex = 0 then NotebookProducts.PageIndex := 1;
  FormOpsiClientKiosk.PanelProductDetail.height := 0;
end;


{*procedure TFormOpsiClientKiosk.SpeedButtonNotInstalledClick(Sender: TObject);
begin
  FormOpsiClientKiosk.NotebookProducts.PageIndex:= 4;
  EditSearch.Text := '';
  TimerSearchEdit.Enabled := False;
  ockdata.DataModuleOCK.SQLQueryProductData.Filtered := False;
  ockdata.DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
  ockdata.DataModuleOCK.SQLQueryProductData.Filter := 'installationStatus = ""';
  ockdata.DataModuleOCK.SQLQueryProductData.Filtered := True;
  FormProgressWindow.ProcessMess;
  while inTileRebuild do
    Sleep(10);
  if RadioGroupView.ItemIndex = 1 then
    BuildProductTiles(ArrayUpdateProductTiles,'FlowPanelNotInstalledTiles');
end;*}


procedure TFormOpsiClientKiosk.SpeedButtonNotInstalledClick(Sender: TObject);
begin
  EditSearch.Clear;
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
  DataModuleOCK.SQLQueryProductData.Filter := 'InstallationStatus = ""';
  DataModuleOCK.SQLQueryProductData.Filtered := True;
  DataModuleOCK.SQLQueryProductData.First;
  if (DataModuleOCK.SQLQueryProductData.EOF and DataModuleOCK.SQLQueryProductData.BOF) then
  begin
    DataModuleOCK.SQLQueryProductData.Filtered := False;
    //DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
    //DataModuleOCK.SQLQueryProductData.Filter := LastFilter;
    //DataModuleOCK.SQLQueryProductData.Filtered := True;
    //DBComboBox1.Enabled := False;
    //DBGrid1.Enabled:= False;
    ShowMessage(rsAllProductsInstalled);
    //SpeedButtonLastClicked.Down:= True;
    SpeedButtonAll.Down:= True;
  end;
  {else
  begin
    SpeedButtonNotInstalled.Down:= True;
    SetView;
    SpeedButtonLastClicked := SpeedButtonNotInstalled;
    LastFilter := DataModuleOCK.SQLQueryProductData.Filter;
  end;}
  SetView;
end;

procedure TFormOpsiClientKiosk.SpeedButtonExpertModeClick(Sender: TObject);
begin
  { Expert mode }
  if FormHelpInfo.CheckBoxExpertMode.Checked then
  begin
    { Expert view }
    PanelExpertMode.Visible := True;
    SetView;
    //NotebookProducts.PageIndex := RadioGroupView.ItemIndex;
  end
  else
  { Standard mode }
  begin
    PanelExpertMode.Visible := False;
    PanelProductDetail.Height := 0;
    SetTilesView;
  end;
end;

procedure TFormOpsiClientKiosk.DBGrid1CellClick(Column: TColumn);
var
  action: string;
begin
  PanelProductDetail.Height := 185;
  {action := DataModuleOCK.SQLQueryProductData.FieldByName('possibleAction').AsString;
  DBComboBox1.Items.Clear;
  DBComboBox1.Items.Add('none');
  DBComboBox1.Items.Add('setup');
  if (action <> 'setup') and (action <> '') then
    DBComboBox1.Items.Add(action);}
  //ShowMessage(DataModuleOCK.SQLQueryProductData.FieldByName('VersionStr').AsString);
   //DBTextVerStr.DataField := 'VersionStr';
  //ShowMessage('OnCellClick');
end;

procedure TFormOpsiClientKiosk.DBGrid1ColExit(Sender: TObject);
begin
  if (DBGrid1.SelectedField.FieldName = DBComboBox1.DataField) then
  begin
    DBComboBox1.Visible := False;
  end;
end;

procedure TFormOpsiClientKiosk.DBGrid1DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
  if (gdFocused in State) then
  begin
    if (DBGrid1.SelectedField.FieldName = DBComboBox1.DataField) then
    begin
      DBComboBox1.Left := Rect.Left + DBGrid1.Left;
      DBComboBox1.Top := Rect.Top + DBGrid1.top;
      DBComboBox1.Width := Rect.Right - Rect.Left;
      DBComboBox1.Visible := True;
    end
    else
    begin
      DBComboBox1.Top := Rect.Top + DBGrid1.top;
    end;
  end;
end;

procedure TFormOpsiClientKiosk.FormActivate(Sender: TObject);
begin
  InitSkin;
  LogDatei.log('LabelTitle.Caption: ' + LabelTitle.Caption, LLDebug);
  if not StartupDone then
  begin
    //ShowMessage('Width of SpeedButtonUpdates:'+ IntToStr(SpeedButtonUpdates.Width));
    try
      FormProgressWindow.ProgressBar1.Position := 0;
      FormProgressWindow.ProgressBarDetail.Position := 0;
      FormProgressWindow.Visible := True;
      try
        InitConnectionToServer;
        { Load data from server }
        LoadDataFromServer;
        { Set Paths for AdminMode}
        PathsOnClient.AdminMode := AdminMode;
        { Set MinWidth and Width of Form}
        MinWidthStandardMode := SpeedButtonAll.Width + SpeedButtonUpdates.Width +
          SpeedButtonNotInstalled.Width + SpeedButtonActions.Width +
          SpeedButtonSearch.Width + BitBtnHelp.Width + 30;
        if AdminMode then MinWidthStandardMode := MinWidthStandardMode + BitBtnSaveImages.Width;
        Constraints.MinWidth := MinWidthStandardMode;
        if Width < MinWidthStandardMode then Width := MinWidthStandardMode;
        SetPositionButtonsOnPanelToolbar;
        { InitDatabase }
        InitDatabase;
        { Initialize GUI }
        if not DisableTilesView then BuildProductTiles(ArrayProductPanels, 'FlowPanelAllTiles');
      except
        LogDatei.log('Error during startup.',LLError);
      end;
    finally
      FormProgressWindow.Close;
      StartupDone := True;
    end;
  end;//end of: if not StartupDone
  if FormHelpInfo.CheckBoxExpertMode.Checked or DisableTilesView then
  begin
    { Expert mode }
    PanelExpertMode.Visible := True;
    MinWidthExpertMode := RadioGroupView.Width +
      BitBtnInstallNow.Width + SpeedButtonReload.Width + 50;
    //ShowMessage('Width of ButtonSaveImagesOnDepot:'+ IntToStr(ButtonSaveImagesOnDepot.Width));
    if MinWidthExpertMode > MinWidthStandardMode then
    begin
      Constraints.MinWidth := MinWidthExpertMode;
      if Width < MinWidthExpertMode then Width := MinWidthExpertMode;
    end;
    if DisableTilesView then RadioGroupView.Visible := false;
    SetView;
    //NotebookProducts.PageIndex := RadioGroupView.ItemIndex;
  end
  else
  { Standard mode }
  begin
    PanelExpertMode.Visible := False;
    PanelProductDetail.Height := 0;
    Constraints.MinWidth := MinWidthStandardMode;
    if Width < MinWidthStandardMode then Width := MinWidthStandardMode;
    SetTilesView;
  end;
  Caption := 'opsi-client-kiosk' + ' (Version '+ ProgramInfo.Version + ')';
end;

procedure TFormOpsiClientKiosk.FormResize(Sender: TObject);
begin
  SetPositionButtonsOnPanelToolbar;
end;



procedure TFormOpsiClientKiosk.ReloadDataFromServer;
var
  i, counter: integer;
begin
  LogDatei.log('Starting ReloadDataFromServer ...', LLNotice);
  //SaveImagesAndScreenshotsLists;
  DataModuleOCK.RemoveTableProductsFromMemory;
  FilteredProductIDs.Clear;
  DataModuleOCK.Free;
  OCKOpsiConnection.Free;
  try
    counter := length(ArrayProductPanels);
    i := 0;
    if counter > 0 then
      //for i:=0 to counter -1 do ArrayProductPanels[i].Destroy;
      for i := 0 to counter - 1 do
      begin
        ArrayProductPanels[i].Free;
        ArrayProductPanels[i] := nil;
        logdatei.log('FreeAndNil(ProductTilesArray', LLDebug2);
      end;
    SetLength(ArrayProductPanels, 0);
    ArrayProductPanels := nil;
  except
    on e: Exception do
    begin
      logdatei.log('Exception FormClose: ProductTilesArray[i].Free;', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E, LLError);
    end;
  end;
  LogDatei.log('Removing former objects done.', LLInfo);
  try
    FormProgressWindow.ProgressBar1.Position := 0;
    FormProgressWindow.ProgressBarDetail.Position := 0;
    FormProgressWindow.Visible := True;
    try
      InitConnectionToServer;
      { Load data from server }
      LoadDataFromServer;
      InitDatabase;
      { Initialize GUI }
      if not DisableTilesView then BuildProductTiles(ArrayProductPanels, 'FlowPanelAllTiles');
    except
      LogDatei.log('Error reloading data.',LLInfo);
    end;
  finally
    FormProgressWindow.Close;
  end;
  LogDatei.log('Finished ReloadDataFromServer', LLNotice);
end;

procedure TFormOpsiClientKiosk.EditSearchChange(Sender: TObject);
begin
  if EditSearch.Text <> '' then
  begin
    SearchProducts;
    //SetView;
  end
  else
  begin
    if SpeedButtonAll.Down then
    begin
      DataModuleOCK.SQLQueryProductData.Filtered := False;
      //SetView;
    end;
    if SpeedButtonUpdates.Down then
    begin
      DataModuleOCK.SQLQueryProductData.Filtered := False;
      DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
      DataModuleOCK.SQLQueryProductData.Filter := 'UpdatePossible and InstallationStatus <> ""';
      DataModuleOCK.SQLQueryProductData.Filtered := True;
      //SetView;
    end;
    if SpeedButtonNotInstalled.Down then
    begin
      DataModuleOCK.SQLQueryProductData.Filtered := False;
      DataModuleOCK.SQLQueryProductData.Filter := 'InstallationStatus = ""';
      DataModuleOCK.SQLQueryProductData.Filtered := True;
      //SetView;
    end;
  end;
  SetView;
end;

procedure TFormOpsiClientKiosk.LoadSkinForTitle(SkinPath: string);
var
  myini: TInifile;
begin
  if FileExists(SkinPath) then
  begin
    LogDatei.Log('loading skin for title from: ' + SkinPath, LLInfo);
    myini := TIniFile.Create(SkinPath);
    //title
    //LogDatei.LogLevel:=9;//for testing -> set loglevel to 9
    LabelTitle.Caption := myini.ReadString('TitleLabel', 'Text', 'Opsi Client Kiosk');
    LogDatei.log('LabelTitle.Caption: ' + LabelTitle.Caption, LLDebug3);
    LabelTitle.Font.Name := myini.ReadString('TitleLabel', 'FontName', 'Arial');
    LogDatei.log('LabelTitle.Font.Name: ' + LabelTitle.Font.Name, LLDebug3);
    LabelTitle.Font.Size := myini.ReadInteger('TitleLabel', 'FontSize', 20);
    LogDatei.log('LabelTitle.Font.Size: ' + IntToStr(LabelTitle.Font.Size), LLDebug3);
    LabelTitle.Font.Color := StringToColor(myini.ReadString('TitleLabel', 'FontColor', 'clBlack'));
    LogDatei.log('LabelTitle.Font.Color: ' + ColorToString(LabelTitle.Font.Color), LLDebug3);
    LabelTitle.Font.Bold := StrToBool(myini.ReadString('TitleLabel','FontBold','True'));
    LogDatei.log('LabelTitle.Font.Bold: ' + BoolToStr(LabelTitle.Font.Bold, True), LLDebug3);
    LabelTitle.Font.Italic := StrToBool(myini.ReadString('TitleLabel', 'FontItalic','False'));
    LogDatei.log('LabelTitle.Font.Italic: ' + BoolToStr(LabelTitle.Font.Italic, True), LLDebug3);
    LabelTitle.Font.Underline := StrToBool(myini.ReadString('TitleLabel', 'FontUnderline', 'False'));
    LogDatei.log('LabelTitle.Font.Underline: ' + BoolToStr(LabelTitle.Font.Underline, True), LLDebug3);
    //LogDatei.LogLevel:=7;//for testing -> set loglevel back to 7
    myini.Free;
  end;
end;

procedure TFormOpsiClientKiosk.FormCreate(Sender: TObject);
var
  InfoText    : String;
  ListOptions : TStringList;
  ErrorMsg    : String;
  Spacing     : String;
begin
  InTileRebuild := False;
  InitLogging('kiosk-' + GetUserName_ +'.log', self.Name + '.FormCreate', LLDebug);
  LogDatei.log('Initialize Opsi Client Kiosk', LLNotice);
  { is opsiclientd or another instance running? }
  //{$IFNDEF DARWIN}
  if not CheckUnique(InfoText) then
  begin
    ShowMessage(InfoText);
    halt(1);
  end;
  //{$ENDIF DARWIN}
  //ShowMessage('Form Create');

  { Init Variables }
  //Height := 640;
  //Width := 640;
  StartUpDone := False;
  ClientdMode := True;
  FilteredProductIDs := TStringList.Create;
  StringListDefaultIcons := TStringList.Create;
  StringListCustomIcons := TStringList.Create;
  StringListScreenshots := TStringList.Create;
  SpeedButtonAll.Down := True;
  SpeedButtonLastClicked := SpeedButtonAll;
  LastFilter := '';
  NotebookProducts.PageIndex := 1;  //tiles
  PanelProductDetail.Height := 0;
  //detail_visible := False;
  LogDatei.log('Default icon path: ' + PathsOnClient.FDefaultIcons, LLinfo);
  LogDatei.log('Custom icon path: ' + PathsOnClient.FCustomIcons, LLinfo);
  LogDatei.log('Screenshot path: ' + PathsOnClient.FCustomScreenShots, LLinfo);
  { Load assignment of image paths to product IDs }
  try
    if FileExists(PathsOnClient.FDefaultIcons + PathDelim + 'IconsList.txt') then
      LoadStringListFromFile(StringListDefaultIcons, PathsOnClient.FDefaultIcons + PathDelim + 'IconsList.txt');
    if FileExists(PathsOnClient.FCustomIcons  + PathDelim + 'IconsList.txt') then
      LoadStringListFromFile(StringListCustomIcons, PathsOnClient.FCustomIcons + PathDelim + 'IconsList.txt');
    if FileExists(PathsOnClient.FCustomScreenShots + PathDelim + 'ScreenshotsList.txt') then
      LoadStringListFromFile(StringListScreenshots, PathsOnClient.FCustomScreenShots + PathDelim + 'ScreenshotsList.txt');
  except
    LogDatei.log('Error while loading Images (Icons and/or Screenshots)',LLError);
  end;


  try
    { quick check parameters }
    ListOptions := TStringList.Create;
    ListOptions.Append('clientdmode:');
    ListOptions.Append('fqdn:');
    ListOptions.Append('lang:');
    ErrorMsg := Application.CheckOptions('', ListOptions);
    LogDatei.log('Options: ' + ListOptions.Text + ', ErrorMsg: ' + ErrorMsg, LLDebug);
    if ErrorMsg <> '' then
    begin
      Application.ShowException(Exception.Create(ErrorMsg));
      Application.Terminate;
      Exit;
    end;
    { parse parameters }
    if Application.HasOption('clientdmode') then
    begin
      ClientdMode := StrToBool(Application.GetOptionValue('clientdmode'));
      LogDatei.log('ClientdMode (Option = clientdmode): ' + BoolToStr(ClientdMode), LLDebug);
    end;
    if Application.HasOption('fqdn') then
    begin
      ClientID := Application.GetOptionValue('fqdn');
      LogDatei.log('ClientID (option = fqdn): ' + ClientID, LLDebug);
    end
    else
    begin
      ClientID := GetClientID(PathsOnClient.FCustomSettings + 'opsiclientkiosk.conf');
    end;
    if Application.HasOption('lang') then
    begin
      SetDefaultLang(Application.GetOptionValue('lang'));
    end
    else
    begin
      GetDefaultLang;
      LogDatei.log('GetDefaultLang: ' + GetDefaultLang, LLEssential);
    end;
  finally
    ListOptions.free;
  end;
  //grouplist.Clear;
  Caption := Caption + ' (Version ' + ProgramInfo.Version + ') ';
  InitDBGrids;

  (*****************)
  (* Localizations *)
  (*****************)
  Spacing := '  ';
  { RadioGroupView }
  RadioGroupView.Items[0] := rsViewList;
  RadioGroupView.Items[1] := rsViewTiles;
  { Expert Mode Buttons }
  BitBtnInstallNow.Caption := rsInstallNow;
  BitBtnInstallNow.Hint := rsInstallNowHint;
  { ButtonSoftware on PageSoftware}
  ButtonSoftwareInstall.Caption := rsInstall;
  ButtonSoftwareUninstall.Caption := rsActUninstall;
  ButtonSoftwareUpdate.Caption := rsUpdate;
  ButtonSoftwareBack.Caption:= rsBack;

  { SpeedButtons on Toolpanel}
  SpeedButtonAll.Caption := Spacing + rsAll + Spacing;
  SpeedButtonUpdates.Caption:= Spacing + rsUpdates + Spacing;
  SpeedButtonNotInstalled.Caption:= Spacing + rsNotInstalled + Spacing;
  SpeedButtonActions.Caption := Spacing + rsActions + Spacing;
  { Set MinWidth, Width and Center Buttons on ToolPanel}

  //ShowMessage('Width of SpeedButtonUpdates:'+ IntToStr(SpeedButtonUpdates.Width));
   { Load skin }
  { Set skin for LabelTitle }
  {skinpath := Application.Location + 'opsiclientkioskskin' + PathDelim;
  if FileExistsUTF8(skinpath + 'opsiclientkiosk.png') then
  begin
    ImageHeader.Picture.LoadFromFile(skinpath + 'opsiclientkiosk.png');
  end;
  if FileExists(skinpath + 'opsiclientkiosk.ini') then
  begin
    LoadSkinForTitle(skinpath);
  end;}
end;

procedure TFormOpsiClientKiosk.EditSearchEnter(Sender: TObject);
begin
  PanelProductDetail.Height := 0;
end;

{ Searching Products}
procedure TFormOpsiClientKiosk.SearchProducts;
var
  Filtercond, Filterstr: string;
begin
  try
    Filtercond := '"*' + EditSearch.Text + '*"';
    LogDatei.log('Search for: ' + EditSearch.Text + ' Filter for: ' +
      Filtercond, LLinfo);
    Filterstr := '(ProductId =' + Filtercond;
    Filterstr := Filterstr + 'or ProductName =' + Filtercond;
    Filterstr := Filterstr + 'or Description =' + Filtercond;
    Filterstr := Filterstr + 'or Advice =' + Filtercond;
    if SpeedButtonAll.Down then
      Filterstr := Filterstr + 'or InstallationStatus =' + Filtercond +')';
    if SpeedButtonNotInstalled.Down then
      Filterstr := Filterstr + ') and InstallationStatus = ""';
    if SpeedButtonUpdates.Down then
       Filterstr := Filterstr + ' ) and UpdatePossible and InstallationStatus <> ""';
    if SpeedButtonActions.Down then
       Filterstr := Filterstr + ' ) and ActionRequest <> ""';
    DataModuleOCK.SQLQueryProductData.Filtered := False;
    DataModuleOCK.SQLQueryProductData.Filter := Filterstr;
    DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
    DataModuleOCK.SQLQueryProductData.Filtered := True;
    DataModuleOCK.SQLQueryProductData.First;
    //LastFilter := DataModuleOCK.SQLQueryProductData.Filter;
  except
    LogDatei.log('Error while seraching for products.',LLError);
    ShowMessage('Error while searching for products.');
  end;
end;

procedure TFormOpsiClientKiosk.SpeedButtonAllClick(Sender: TObject);
begin
  EditSearch.Clear;
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  //LastFilter := '';
  //SpeedButtonAll.Down:= True;
  SetView;
  //SpeedButtonLastClicked := SpeedButtonAll;
end;


procedure TFormOpsiClientKiosk.SpeedButtonViewListClick(Sender: TObject);
begin
  NotebookProducts.PageIndex := 0;
end;

procedure TFormOpsiClientKiosk.SpeedButtonViewStoreClick(Sender: TObject);
begin
  NotebookProducts.PageIndex := 1;
end;


procedure TFormOpsiClientKiosk.BitBtnCancelClick(Sender: TObject);
var
  counter, i: integer;
begin
  if OCKOpsiConnection.OpsiData <> nil then
  begin
    // do not send log
    //opsidata.finishOpsiConf;
    OCKOpsiConnection.OpsiData.Free;
  end;
  try
    counter := length(ArrayProductPanels);
    if counter > 0 then
      //for i:=0 to counter -1 do ArrayProductPanels[i].Destroy;
      for i := 0 to counter - 1 do
      begin
        ArrayProductPanels[i] := nil;
        ArrayProductPanels[i].Free;
        logdatei.log('FreeAndNil(ProductTilesArray', LLDebug2);
      end;
    SetLength(ArrayProductPanels, 0);

  except
    on e: Exception do
    begin
      logdatei.log('Exception BitBtnCancelClick: ProductTilesArray[i].Free;', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E, LLError);
    end;
  end;

  Application.Terminate;
  halt;
end;


procedure TFormOpsiClientKiosk.SetPositionButtonsOnPanelToolbar;
var
  testWidth :integer;
begin
  //testWidth := SpeedButtonUpdates.Width;
  if AdminMode then
    SpeedButtonAll.BorderSpacing.Left := round((BitBtnSaveImages.Left -
    (SpeedButtonAll.Width + SpeedButtonUpdates.Width +
     SpeedButtonNotInstalled.Width + SpeedButtonActions.Width))/2)
  else
    SpeedButtonAll.BorderSpacing.Left := round((SpeedButtonSearch.Left -
    (SpeedButtonAll.Width + SpeedButtonUpdates.Width +
     SpeedButtonNotInstalled.Width + SpeedButtonActions.Width))/2);
end;

procedure TFormOpsiClientKiosk.SaveIconsAndScreenshotsLists;
begin
  { Save Custom changes on client }
  logDatei.log('Saving IconsList and ScreenshotsList' , LLNotice);
  logDatei.log('Saving StringListCustomIcons to ' +  PathsOnClient.FCustomIcons + PathDelim +'IconsLis'
    +'t.txt', LLInfo);
  SaveStringListToFile(StringListCustomIcons, PathsOnClient.FCustomIcons + PathDelim +'IconsList.txt');
  logDatei.log('Saving StringListScreenshots to ' +  PathsOnClient.FCustomScreenShots + PathDelim +'Screensh'
    +'otsList.txt', LLInfo);
  SaveStringListToFile(StringListScreenshots, PathsOnClient.FCustomScreenShots + PathDelim +'ScreenshotsList'
    +'.txt');
end;

procedure TFormOpsiClientKiosk.DeleteFormerImage(ImagePath:String);
begin
  if FileExistsUTF8(ImagePath) then
  begin
    LogDatei.log('Delete image ' + ImagePath, LLDebug);
    DeleteFileUTF8(ImagePath);
  end;
end;

procedure TFormOpsiClientKiosk.InitSkin;
begin
  {Loading header image}
  if FileExists(PathsOnClient.FCustomSkin + PathDelim +  'header.png') then
  begin
    ImageHeader.Picture.LoadFromFile(PathsOnClient.FCustomSkin + PathDelim + 'header.png');
  end
  else
    if FileExists(PathsOnClient.FDefaultSkin + PathDelim + 'header.png') then
    begin
      ImageHeader.Picture.LoadFromFile(PathsOnClient.FDefaultSkin + PathDelim + 'header.png');
    end;
  {Loading logo}
  if FileExists(PathsOnClient.FCustomSkin + PathDelim + 'logo.png') then
  begin
    ImageLogo.Picture.LoadFromFile(PathsOnClient.FCustomSkin + PathDelim + 'logo.png');
  end
  else
    if FileExists(PathsOnClient.FDefaultSkin + PathDelim + 'logo.png') then
    begin
      ImageLogo.Picture.LoadFromFile(PathsOnClient.FDefaultSkin + PathDelim + 'logo.png');
    end;
  {Loading label text and font style}
  if FileExists(PathsOnClient.FCustomSkin + PathDelim + 'opsiclientkiosk.ini') then
  begin
    LoadSkinForTitle(PathsOnClient.FCustomSkin + PathDelim + 'opsiclientkiosk.ini');
  end
  else
    if FileExists(PathsOnClient.FDefaultSkin + PathDelim + 'opsiclientkiosk.ini') then
    begin
      LoadSkinForTitle(PathsOnClient.FDefaultSkin + PathDelim + 'opsiclientkiosk.ini');
    end;
end;

function TFormOpsiClientKiosk.GetClientID(PathToConfigFile:string): string;
var
  KioskConfig:TIniFile;
begin
  KioskConfig :=  TIniFile.Create(PathToConfigFile);
  Result := KioskConfig.ReadString('webservice','ClientID','');
  FreeAndNil(KioskConfig);
end;


function TFormOpsiClientKiosk.InitLogging(const LogFileName, CallingMethod: string;
  MyLogLevel:integer): boolean;
begin
  try
    //LogDatei.free;
    LogDatei := TLogInfo.Create;
    LogDatei.WritePartLog := False;
    LogDatei.WriteErrFile:= False;
    LogDatei.WriteHistFile:= False;
    LogDatei.CreateTheLogfile(LogFileName, False);
    LogDatei.LogLevel := MylogLevel;
    LogDatei.log(ProgramInfo.InternalName+ ' ' + 'Version('+ProgramInfo.Version+')'
      + ' starting at ' + DateTimeToStr(now), LLEssential);
    LogDatei.log('Initialize Logging', LLNotice);
    InitLogging := True;
  except
    InitLogging := False;
    LogDatei.log('Error while initialising logging. Calling Method: TFormOpsiClientKiosk.InitLogging',LLDebug);
  end;
end;

procedure TFormOpsiClientKiosk.InitDBGrids;
begin
  { DBGrid1 }
  //DBGrid1.Columns.Add.FieldName := 'ProductId';
  //DBGrid1.Columns.Items[0].Title.Caption := rsProductID;//'ProductId';
  //DBGrid1.Columns.Items[0].Width := 150;
  DBGrid1.Columns.Add.FieldName := 'ProductName';
  DBGrid1.Columns.Items[0].Title.Caption := rsProductName;
  DBGrid1.Columns.Items[0].Width := 300;
  DBGrid1.Columns.Add.FieldName := 'InstallationStatus';
  DBGrid1.Columns.Items[1].Title.Caption := rsStatus;//'InstallationStatus';
  DBGrid1.Columns.Items[1].Width := 100;
  DBGrid1.Columns.Add.FieldName := 'actionrequest';
  DBGrid1.Columns.Items[2].Title.Caption := rsRequest;//'ActionRequest';
  DBGrid1.Columns.Items[2].Width := 100;
  //DBGrid1.Columns.Add.FieldName := 'updatePossible';
  //DBGrid1.Columns.Items[4].Title.Caption := 'updatePossible';
  //DBGrid1.Columns.Items[4].Width := 100;

  { DBGrid2 }
  DBGrid2.Columns.Add.FieldName := 'requiredProductId';
  DBGrid2.Columns.Items[0].Title.Caption := 'requiredProductId';
  DBGrid2.Columns.Items[0].Width := 300;
  DBGrid2.Columns.Add.FieldName := 'required';
  DBGrid2.Columns.Items[1].Title.Caption := 'required';
  DBGrid2.Columns.Items[1].Width := 100;
  DBGrid2.Columns.Add.FieldName := 'prerequired';
  DBGrid2.Columns.Items[2].Title.Caption := 'pre-required';
  DBGrid2.Columns.Items[2].Width := 100;
  DBGrid2.Columns.Add.FieldName := 'postrequired';
  DBGrid2.Columns.Items[3].Title.Caption := 'post-required';
  DBGrid2.Columns.Items[3].Width := 100;
end;

function TFormOpsiClientKiosk.InstallNow(var aErrorMessage:string ): boolean;
var
  i : integer;
begin
  //Test loop to generate error message
  //for i:= 0 to 6 do
  //begin
  //  OCKOpsiConnection.DoActionsOnDemand(aErrorMessage);
  //  sleep(100);
  //end;
  OCKOpsiConnection.DoActionsOnDemand(aErrorMessage);
  if aErrorMessage  = '' then
  begin
    sleep(10000);
    while osprocesses.numberOfProcessInstances('notifier') > 0 do
    begin
      Application.ProcessMessages;
      //Instances := ockunique.numberOfProcessInstances('notifier');
      sleep(100);
      //Instances := ockunique.numberOfProcessInstances('notifier');
    end;
    Result := True;
  end else Result:= False;
end;


(*function TFormOpsiClientKiosk.RunAsAdmin(const Handle: DWord; const Path, Params: string): Boolean;
var
  ShellExecuteInfoA: TShellExecuteInfoA;
begin
  FillChar(ShellExecuteInfoA, SizeOf(ShellExecuteInfoA), 0);
  with ShellExecuteInfoA do
  begin
    cbSize := SizeOf(ShellExecuteInfoA);
    Wnd := Handle;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    lpVerb := 'runas';
    lpFile := PAnsiChar(Path);
    lpParameters := PAnsiChar(Params);
    nShow := 1;
  end;
  Result := ShellExecuteExA(@ShellExecuteInfoA);
end;*)


(*function TFormOpsiClientKiosk.SaveImagesOnDepotNT: String;
var
  Shell,
  ShellOptions,
  ShellCommand,
  ShellOutput: String;
  PathToExe: String;
begin
  try
    Result := '';
    PathToExe := TrimFilename(Application.Location + 'images_to_depot\images_to_depot.exe');
    LogDatei.log('Saving images on depot...',LLInfo);
    {set shell and options}
    Shell := 'powershell.exe';//PathToExe;//'powershell.exe';
    ShellOptions := '/c'; //-Verb runAs  'Start-Process PowerShell -Verb RunAs | '
    ShellCommand := 'Start-Process ' + QuotedStr(PathToExe) + ' -Verb RunAs';
    //if RunCommand(Shell, ['Start-Process', QuotedStr(PathToExe), '-Verb RunAs'], ShellOutput) then
    if RunAsAdmin(FormOpsiClientKiosk.Handle, PathToExe,'') then
    begin
      //ShellCommand := '';
      Result := ShellOutput;
      LogDatei.log('Images saved on depot', LLInfo);
      LogDatei.log('Shell command: ' + ShellCommand, LLDebug);
      //ShowMessage(ShellOutput);
    end
    else
    begin
      Result := '';
      LogDatei.log('Error while trying to run command ' + ShellCommand
        + ' on ' + Shell + ' with options ' + ShellOptions, LLError);
    end;
  except
    Result := '';
    LogDatei.log('Exception during SaveImagesOnDepot', LLDebug);
  end;
end;*)


{procedure TFormOpsiClientKiosk.InitOpsiClientKiosk;
begin
  FormProgressWindow.LabelDataload.Caption := 'Init connection';
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;

  if true then //OCKOpsiConnection.initConnection(30, ConnectionInfo) then
  begin
    LogDatei.log('init connection done', LLNotice);
    DataModuleOCK := TDataModuleOCK.Create(nil);
    DataModuleOCK.InitDatabase;
    FormProgressWindow.LabelInfo.Caption := 'Please wait while gettting products';
    LogDatei.log('start OpsiProductsToDataset', LLNotice);
    DataModuleOCK.OpsiProductsToDataset(DataModuleOCK.SQLQueryProductData);
    LogDatei.log('Handle products done', LLNotice);
    FormProgressWindow.LabelDataload.Caption := 'Handle Products';
    FormProgressWindow.ProcessMess;
  end
  else
  begin
    LogDatei.log('init Connection failed - Aborting', LLError);
    if OCKOpsiConnection.OpsiData <> nil then
      OCKOpsiConnection.OpsiData.Free;
    FormOpsiClientKiosk.Terminate;
    halt(1);
  end;
  LogDatei.log('InitOPsiClientKiosk done', LLDebug2);
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;
end;}



end.
