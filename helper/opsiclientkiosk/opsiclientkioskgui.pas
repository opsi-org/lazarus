// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/



unit opsiclientkioskgui;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DB, ExtendedNotebook, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, Grids, DBGrids,
  DBCtrls,
  datadb,
  CommCtrl, BufDataset, typinfo, installdlg, lcltranslator,
  ActnList, Menus, oslog, inifiles, Variants, Lazfileutils, Types,
  opsiconnection,
  jwawinbase,
  osprocesses,
  pleasewaitwindow,
  progresswindow, proginfo;

type

  { TProductPanel }

  TProductPanel = class(TPanel)
    ShapeRoundSquare : TShape; //just for a nice look
    LabelAction: TLabel; //ProductID
    LabelName: TLabel; //name of product
    LabelState: TLabel; //Status of product e.g. installed, not installed, update
    ImageIcon : TImage; //Icon of the product
    procedure TileClick(Sender:TObject);
    procedure Scroll(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint; var Handled: boolean);
    procedure ProductTileMouseEnter(Sender :TObject);
    procedure ProductTileMouseLeave(Sender :TObject);
    procedure LoadSkin(SkinPath:string);
  private
    { private declarations }
  public
    { public declarations }
    ProductID : String;
    constructor Create(TheOwner: TWinControl);overload;
    destructor Destroy;override;
  end;

  TPanels = array of TProductPanel;


  { TFormOpsiClientKiosk }

  TFormOpsiClientKiosk = class(TForm)
    BitBtn1: TBitBtn;
    BitBtnInfo: TBitBtn;
    BitBtnStoreAction: TBitBtn;
    ButtonSoftwareUninstall: TButton;
    ButtonSoftwareUpdate: TButton;
    ButtonSoftwareInstall: TButton;
    ButtonSoftwareBack: TButton;
    DataSourceProductDependencies: TDataSource;
    DataSourceProductData: TDataSource;
    DBComboBox1: TDBComboBox;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBMemoAdvice: TDBMemo;
    DBMemoSoftwareAdvice: TDBMemo;
    DBMemoDescription: TDBMemo;
    DBMemoSoftwareDescription: TDBMemo;
    DBTextInstalledVerStr: TDBText;
    DBTextSoftwareClientVerStr: TDBText;
    DBTextPriority: TDBText;
    DBTextVerStr: TDBText;
    DBTextSoftwareVerStr: TDBText;
    ExtendedNotebook1: TExtendedNotebook;
    FlowPanelAllTiles: TFlowPanel;
    FlowPanelSearchTiles: TFlowPanel;
    ImageIconSoftware: TImage;
    ImageScreenShot: TImage;
    ImageViewmag: TImage;
    LabelSoftwareInstalledVersion: TLabel;
    LabelSoftwareAdvice: TLabel;
    LabelSoftwareDescription: TLabel;
    LabelSoftwareName: TLabel;
    LabelPriority: TLabel;
    LabelSoftwareRecentVersion: TLabel;
    LabelVerstr: TLabel;
    LabelDescription: TLabel;
    LabelAdvice: TLabel;
    LabelClientVerstr: TLabel;
    PageSearchTiles: TPage;
    PageSoftwareDetails: TPage;
    PanelSoftwareScreenshot: TPanel;
    PanelSoftwareHead: TPanel;
    PanelSearchEdit: TPanel;
    PanelExpertMode: TPanel;
    PanelDependencies: TPanel;
    PanelPriority: TPanel;
    PanelDetailsDBText: TPanel;
    PanelDetailsLables: TPanel;
    RadioGroupView: TRadioGroup;
    ScrollBoxAllTiles: TScrollBox;
    ScrollBoxSearchTiles: TScrollBox;
    ScrollBoxSoftwarePage: TScrollBox;
    EditSearch: TEdit;
    ImageHeader: TImage;
    SpeedButtonNotInstalled: TSpeedButton;
    SpeedButtonExpertMode: TSpeedButton;
    SpeedButtonActions: TSpeedButton;
    SpeedButtonUpdates: TSpeedButton;
    BtnClearSearchEdit: TSpeedButton;
    SpeedButtonReload: TSpeedButton;
    SpeedButtonAll: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TitleLabel: TLabel;
    NotebookProducts: TNotebook;
    PageList: TPage;
    PageAllTiles: TPage;
    PanelToolbar: TPanel;
    PanelProductList: TPanel;
    PanelProductDetail: TPanel;
    PanelTopImage: TPanel;
    StatusBar1: TStatusBar;
    procedure BitBtnInfoClick(Sender: TObject);
    procedure BitBtnShowActionClick(Sender: TObject);
    procedure BitBtnStoreActionClick(Sender: TObject);
    procedure BitBtnToggleViewClick(Sender: TObject);
    procedure ButtonReadConfigStateClick(Sender: TObject);
    procedure ButtonSoftwareBackClick(Sender: TObject);
    procedure ButtonSoftwareInstallClick(Sender: TObject);
    procedure ButtonSoftwareUninstallClick(Sender: TObject);
    procedure DBComboBox1Change(Sender: TObject);
    procedure DBComboBox1Click(Sender: TObject);
    procedure DBComboBox1Enter(Sender: TObject);
    procedure DBComboBox1Exit(Sender: TObject);
    procedure DBComboBox1MouseEnter(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure DBGrid1ColExit(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1Enter(Sender: TObject);
    procedure DBGrid1Exit(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GrouplistEnter(Sender: TObject);
    procedure RadioGroupViewSelectionChanged(Sender: TObject);
    procedure ScrollBoxAllTilesMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure BtnClearSearchEditClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButtonActionsClick(Sender: TObject);
    procedure SpeedButtonReloadClick(Sender: TObject);
    procedure SpeedButtonNotInstalledClick(Sender: TObject);
    procedure SpeedButtonExpertModeClick(Sender: TObject);
    procedure SpeedButtonUpdatesClick(Sender: TObject);
    procedure Terminate;
    procedure BitBtnCancelClick(Sender: TObject);
    //procedure grouplistSelectionChange(Sender: TObject; User: boolean);
    procedure ReloadDataFromServer;
    procedure EditSearchChange(Sender: TObject);
    //procedure RadioGroup1Click(Sender: TObject);
    procedure EditSearchEnter(Sender: TObject);
    procedure SpeedButtonAllClick(Sender: TObject);
    procedure SpeedButtonViewListClick(Sender: TObject);
    procedure SpeedButtonViewStoreClick(Sender: TObject);
    function GetUserName_:string;
    function InitLogging(const LogFileName, CallingMethod: string; MyLogLevel:integer): boolean;
    procedure LoadSkin(SkinPath: string);
    //procedure InitOpsiClientKiosk;
    procedure SearchProducts;
    procedure BuildProductTiles(var ArrayProductTiles:TPanels; const OwnerName:string);
    procedure SetActionRequest(Request:String; Message:String; OnDemand:boolean);
    //function GetTileIDbyProductID(const ProductID:String):integer;
    procedure SetView;
    //procedure SetTileIDbyProductID(const TileID, ProductID:String);
    procedure SetListView;
    procedure SetTilesView;
  private
    { private declarations }
    //StringListTileIDs:TStringList;
    SoftwareOnDemand : boolean;
    SelectedTile : integer;  //TileIndex e.g. Tag
    SelectedProduct : String; //ProductID
    FilteredProductIDs : TStringList;
    StartUpDone : boolean;
  public
    { public declarations }
  end;


var
  FormOpsiClientKiosk: TFormOpsiClientKiosk;
  //StartupDone: boolean;
  ArrayAllProductTiles: TPanels;
  ArraySearchPanelTiles: TPanels;
  InTileRebuild: boolean = False;
  LastOrderDirAsc: boolean = True;
  LastOrderCol: string;
  detail_visible: boolean = False;
  skinpath: string;
  //preLogfileLogList: TStringList;


resourcestring
  rsNoActionsFound = 'No action requests found.';
  rsActRequest = 'Action Request';
  rsActSetup = 'Setup';
  rsActUninstall = 'Uninstall';
  rsActNone = 'None';
  rsInstalled = 'Installed';
  rsNotInstalled = 'Not installed';
  rsStateUnknown = 'Unknown';
  rsUpdate = 'Update';
  rsViewList = 'List';
  rsViewTiles = 'Tiles';
  rsInstallNow = 'Install now';
  rsStoreActions = 'Store Actions';
  rsInstallNowHint = 'Start the installation (deinstallation) of the selected products';
  rsStoreActionsHint =
    'Send the action requests to the server, show the resulting installations and ask for installation start.';


implementation

{$R *.lfm}

const
  //color for StatusLabel
  clInstalled = clTeal;
  clUpdate = $000080FF;
  clNotInstalled = $00FF8000;
  clUnknown = clRed;

  //Path to programm icons
  IconPathCustom = '.'+PathDelim + 'progam_icons' + PathDelim + 'custom' + PathDelim;//Application.Location + PathDelim + 'progam_icons' + PathDelim + 'custom' + PathDelim;
  IconPathDefault = '.'+PathDelim + 'progam_icons' + PathDelim + 'default' + PathDelim;

  //Path to screenshots
  ScreenshotPath = '.'+PathDelim + 'screenshots' + PathDelim;


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
var
  SkinPath : String;
begin
  try
    inherited Create(TheOwner);
    parent := theOwner;
    //Width := tile_width;
    //Height := tile_height;
    //FlowStyle := fsTopBottomLeftRight;
    //self.OnClick := ProductTileClick;
    //BorderStyle := bsSingle;
    BorderStyle := bsNone;
    //BorderWidth := 4;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    BorderSpacing.Around := 5;
    //Color := StringToColor(tile_color);
    //Font.Name := tile_Font_Name;
    //font.Size := tile_Font_Size;
    //font.Color := StringToColor(tile_Font_Color);
    //font.Bold := tile_Font_Bold;
    //font.Italic := tile_Font_Italic;
    //font.Underline := tile_Font_Underline;
    self.OnMouseWheel := scroll;
    LoadSkin(Application.Location + 'opsiclientkioskskin' + PathDelim);

    //Shape (just for looking nice)
    ShapeRoundSquare := TShape.Create(self);
    with ShapeRoundSquare do begin
       Parent := self;
       Align := alNone;
       Height:= self.width;
       Width:= self.width;
       Shape:= stRoundSquare;
       Top:= 0;
       Left:= 0;
       OnClick := TileClick;//ProductTileChildClick;
       OnMouseWheel := scroll;
       OnMouseEnter := ProductTileMouseEnter;
       OnMouseLeave := ProductTileMouseLeave;
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
      WordWrap := True;
      AutoSize := False;
      Width := self.Width;
      Alignment := taCenter;
      Align := alNone;
      Top := 100;
      Left := 0;
      BorderSpacing.Around := 3;
      OnClick := TileClick;//ProductTileChildClick;
      OnMouseWheel := scroll;
      OnMouseEnter := ProductTileMouseEnter;
      OnMouseLeave := ProductTileMouseLeave;
    end;


    //label ID
    LabelAction := TLabel.Create(self);
    with LabelAction do begin
      Parent := self;
      Caption := 'Action: none';
      Font.Bold := self.Font.Bold;
      AutoSize:= False;
      Alignment := taCenter;
      Width := self.Width;
      Align := alNone;
      Top := LabelName.Top + LabelName.Height + 3;
      Left := 0;
      //labelId.BorderSpacing.Around := 3;
      OnClick := TileClick;//ProductTileChildClick;
      OnMouseWheel := scroll;
      OnMouseEnter := ProductTileMouseEnter;
      OnMouseLeave := ProductTileMouseLeave;
    end;

    //program icon
    ImageIcon := TImage.Create(self);
    with ImageIcon do
    begin
      Parent := self;
      //Picture.LoadFromFile(IconPath);
      AutoSize := False;
      Proportional := True;
      Width := 60;
      Height := 60;
      Center := True;
      Align:= alNone;
      Top:=35;
      Left:=40;
      //BorderSpacing.Around := 0;
      OnClick := TileClick;//ProductTileChildClick;
      OnMouseWheel := scroll;
      OnMouseEnter := ProductTileMouseEnter;
      OnMouseLeave := ProductTileMouseLeave;
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
      OnClick := TileClick;//ProductTileChildClick;
      OnMouseWheel := scroll;
      OnMouseEnter := ProductTileMouseEnter;
      OnMouseLeave := ProductTileMouseLeave;
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

procedure TProductPanel.ProductTileMouseEnter(Sender: TObject);
var
  tileindex: integer;
begin
  if not inTileRebuild then
  begin
    tileindex := TControl(Sender).Parent.Tag; //type convertion to TControl (all visual components are of this type), parent is the Panel
    //Case TControl(Sender).Parent.GetNamePath of
    //  'FlowPanelAllTiles':
    //end;
    ArrayAllProductTiles[tileindex].ShapeRoundSquare.Brush.Color:=$00FEEFD3;
    //former_selected_tile := tileindex;
  end;
end;

procedure TProductPanel.ProductTileMouseLeave(Sender: TObject);
var
  tileindex: integer;
begin
  if not inTileRebuild then
  begin
    tileindex := TControl(Sender).Parent.Tag; //type convertion to TControl (all visual components are of this type), parent is the Panel
    ArrayAllProductTiles[tileindex].ShapeRoundSquare.Brush.Color:=clWhite;
    //former_selected_tile := tileindex;
  end;
end;

procedure TProductPanel.LoadSkin(SkinPath: string);
var
  myini: TInifile;
begin
  if FileExists(skinpath + 'opsiclientkiosk.ini') then
  begin
    LogDatei.Log('loading skin for ' + ProductID + ' from: ' + skinpath + 'opsiclientkiosk.ini', LLEssential);
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

procedure TProductPanel.TileClick(Sender: TObject);
var
  TileIndex :integer;
  pid : String;
  gefunden : boolean;
  sqltext : String;
begin
  try
    TileIndex := TControl(Sender).Parent.Tag;
    FormOpsiClientKiosk.SelectedTile := TileIndex;
    pid := ArrayAllProductTiles[TileIndex].ProductID;
    FormOpsiClientKiosk.SelectedProduct := pid;
    //DataModuleOCK.SQLTransaction.StartTransaction;
    //sqltext := DataModuleOCK.SQLQueryProductData.SQL.Text;
    //DataModuleOCK.SQLQueryProductData.Open;
    DataModuleOCK.SQLQueryProductData.First;
    //gefunden := DataModuleOCK.SQLQueryProductData.Locate('ProductID', VarArrayOf([pid]),[loCaseInsensitive]);
    if DataModuleOCK.SQLQueryProductData.Locate('ProductID', VarArrayOf([pid]),[loCaseInsensitive]) then
    begin
      FormOpsiClientKiosk.PanelProductDetail.Height := 0;
      FormOpsiClientKiosk.NotebookProducts.PageIndex := 2;
      FormOpsiClientKiosk.PanelExpertMode.Visible := False;
      FormOpsiClientKiosk.PanelToolbar.Visible := False;
      FormOpsiClientKiosk.LabelSoftwareName.Caption := ArrayAllProductTiles[TileIndex].LabelName.Caption;
      FormOpsiClientKiosk.ImageIconSoftware.Picture:= ArrayAllProductTiles[TileIndex].ImageIcon.Picture;
      if FileExists(ScreenshotPath +'screenshot1_'+ ArrayAllProductTiles[TileIndex].ProductID +'.png') then
       FormOpsiClientKiosk.ImageScreenShot.Picture.LoadFromFile
         (ScreenshotPath +'screenshot1_'+ ArrayAllProductTiles[TileIndex].ProductID +'.png')
      else FormOpsiClientKiosk.ImageScreenShot.Picture.LoadFromFile
           (ScreenshotPath + 'no_screenshot.png');
      if ArrayAllProductTiles[TileIndex].LabelState.Caption = rsInstalled then
      begin
        FormOpsiClientKiosk.ButtonSoftwareInstall.Visible := False;
        FormOpsiClientKiosk.ButtonSoftwareUninstall.Visible := True;
        FormOpsiClientKiosk.ButtonSoftwareUpdate.Enabled := False;
      end
      else
      if ArrayAllProductTiles[TileIndex].LabelState.Caption = rsNotInstalled then
      begin
        FormOpsiClientKiosk.ButtonSoftwareUninstall.Visible := False;
        FormOpsiClientKiosk.ButtonSoftwareUpdate.Enabled := False;
        FormOpsiClientKiosk.ButtonSoftwareInstall.Visible := True;
        //FormOpsiClientKiosk.
      end
      else
      if ArrayAllProductTiles[TileIndex].LabelState.Caption = rsUpdate then
      begin
        FormOpsiClientKiosk.ButtonSoftwareInstall.Visible := False;
        FormOpsiClientKiosk.ButtonSoftwareUninstall.Visible := True;
        FormOpsiClientKiosk.ButtonSoftwareUpdate.Enabled := True;
      end
    end;
  finally
    //DataModuleOCK.SQLQueryProductData.Close;
  end;
end;

procedure TFormOpsiClientKiosk.BuildProductTiles(var ArrayProductTiles:TPanels; const OwnerName:string);
var
  counter, i, index: integer;
  action, state, update, ProductID: string;
begin
  try
    inTileRebuild := True;
    logdatei.log('BuildProductTiles start', LLDebug2);

    { ProgressBar Init}
    with FormProgressWindow do
    begin
      ProgressbarDetail.Position := 0;
      LabelInfo.Caption:= 'Please wait while building tiles';
      LabelDataload.Caption := 'Fill Tiles';
      ProgressBar1.Step := 1;
      ProgressBar1.Max := 2*ProgressBar1.Position + DataModuleOCK.SQLQueryProductData.RecordCount;
      ProgressBarDetail.Step := 1;
      ProgressBarDetail.Max := ProgressBar1.Position + DataModuleOCK.SQLQueryProductData.RecordCount;
    end;
    Application.ProcessMessages;

    { if array exists free content and set array length 0 }
    try
      counter := length(ArrayProductTiles);
      if counter > 0 then
        for i := 0 to counter - 1 do
        begin
          ArrayProductTiles[i].Visible := False;
          ArrayProductTiles[i].Free;
          ArrayProductTiles[i] := nil;
        end;
      SetLength(ArrayProductTiles, 0);
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
    while not DataModuleOCK.SQLQueryProductData.EOF do
    begin
      ProductID := DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString;
      { Progressbar progress}
      FormProgressWindow.LabelDataLoadDetail.Caption := ProductID;
      FormProgressWindow.ProgressbarDetail.StepIt;
      FormProgressWindow.ProgressBar1.StepIt;
      Application.ProcessMessages;
      {Fill ArrayProducttiles }
      FilteredProductIDs.Add(ProductID);
      SetLength(ArrayProductTiles, counter + 1);
      ArrayProductTiles[counter] := TProductPanel.Create(FormOpsiClientKiosk.FindComponent(OwnerName) as TFlowPanel);
      ArrayProductTiles[counter].ProductID := ProductID;
      ArrayProductTiles[counter].LabelAction.Caption := DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString;
      ArrayProductTiles[counter].LabelName.Caption := DataModuleOCK.SQLQueryProductData.FieldByName('ProductName').AsString;
      if FileExists(IconPathCustom + ProductID + '.png') then
        ArrayProductTiles[counter].ImageIcon.Picture.LoadFromFile(IconPathCustom + ProductID + '.png')
      else
        if FileExists(IconPathDefault + ProductID + '.png') then
          ArrayProductTiles[counter].ImageIcon.Picture.LoadFromFile(IconPathDefault + ProductID + '.png')
        else
          if FileExists(IconPathDefault + 'opsi-logo.png') then
            ArrayProductTiles[counter].ImageIcon.Picture.LoadFromFile(IconPathDefault + 'opsi-logo.png');
      state := DataModuleOCK.SQLQueryProductData.FieldByName('InstallationStatus').AsString;
      if state = 'installed' then
      begin
        if DataModuleOCK.SQLQueryProductData['updatePossible'] then
        begin
          ArrayProductTiles[counter].LabelState.Caption := rsUpdate;
          ArrayProductTiles[counter].LabelState.Color := clUpdate;
        end
        else
        begin
          ArrayProductTiles[counter].LabelState.Caption := rsInstalled;
          ArrayProductTiles[counter].LabelState.Color := clInstalled;
        end;
      end
      else if (state = 'not_installed') or (state = 'not installed') or (state = '') then
      begin
        ArrayProductTiles[counter].LabelState.Caption := rsNotInstalled;
        ArrayProductTiles[counter].LabelState.Color := clNotInstalled;
      end
      else if state = 'unknown' then
      begin
        ArrayProductTiles[counter].LabelState.Caption := rsStateUnknown;
        ArrayProductTiles[counter].LabelState.Color := clUnknown;
      end;
      ArrayProductTiles[counter].Tag := counter;
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

procedure TFormOpsiClientKiosk.SetActionRequest(Request:String; Message:String; OnDemand:boolean);
begin
  Screen.Cursor := crHourGlass;
  OCKOpsiConnection.SetActionRequest(SelectedProduct,Request); //to opsi server
  DataModuleOCK.SetActionRequestToDataset(SelectedProduct,Request);// to local database
  if OnDemand then OCKOpsiConnection.DoActionOnDemand;
  Screen.Cursor := crDefault;
  ShowMessage('Request done. ' + LabelSoftwareName.Caption + Message);
end;

procedure TFormOpsiClientKiosk.SetView;
begin
  if SpeedButtonExpertMode.Down
    and (RadioGroupview.ItemIndex = RadioGroupView.Items.IndexOf('List'))
  then SetListView
  else SetTilesView;
end;


procedure TFormOpsiClientKiosk.SetListView;
begin
  if not DataModuleOCK.SQLQueryProductData.EOF then
  begin
    DBGrid1.Enabled:= True;
    DBComboBox1.Enabled := True;
    DataSourceProductData.Edit;
    DataModuleOCK.SQLQueryProductData.First;
  end;
  FormOpsiClientKiosk.NotebookProducts.PageIndex:= 0
end;

procedure TFormOpsiClientKiosk.SetTilesView;
var
  i: integer;
begin
  i := 0; //initialize counter for loop
  DataModuleOCK.SQLQueryProductData.Open;
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
    for i := 0 to Length(ArrayAllProductTiles)-1 do
    begin
      If FilteredProductIDs.IndexOf(ArrayAllProductTiles[i].ProductID) <> -1
      then ArrayAllProductTiles[i].Visible := True
      else ArrayAllProductTiles[i].Visible := False;
    end;
  end
  else
  { Set all products to visible }
  begin
    for i := 0 to Length(ArrayAllProductTiles)-1 do
      ArrayAllProductTiles[i].Visible := True;
  end;
  DataModuleOCK.SQLQueryProductData.First;
  PanelProductDetail.Height:= 0;
  FormOpsiClientKiosk.NotebookProducts.PageIndex:= 1;
end;

{procedure Tmythread2.Execute;
begin
  // write back action requests
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  DataModuleOCK.SQLQueryProductData.Filter := 'ActionRequest  <> ""';
  DataModuleOCK.SQLQueryProductData.Filtered := True;
  DataModuleOCK.SQLQueryProductData.First;
  while not DataModuleOCK.SQLQueryProductData.EOF do
  begin
    OCKOpsiConnection.setActionrequest(DataModuleOCK.SQLQueryProductData.FieldByName('ProductId').AsString,
      DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString);
    DataModuleOCK.SQLQueryProductData.Next;
  end;
  Terminate;
end;}


{ TFormOpsiClientKiosk }

//procedure TFormOpsiClientKiosk.ProcessMess;
//begin
//  Application.ProcessMessages;
//end;

procedure TFormOpsiClientKiosk.RadioGroupViewSelectionChanged(Sender: TObject);
begin
  SetView;
end;

procedure TFormOpsiClientKiosk.ScrollBoxAllTilesMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  logdatei.log('ScrollBox1MouseWheel WheelDelta: ' + IntToStr(WheelDelta), LLDebug2);
end;

procedure TFormOpsiClientKiosk.BtnClearSearchEditClick(Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    EditSearch.Clear;
    //DataModuleOCK.SQLQueryProductData.Open;
    DataModuleOCK.SQLQueryProductData.Filtered := False;
    DataModuleOCK.SQLQueryProductData.First;
    SpeedButtonAll.AllowAllUp:= False;
    SpeedButtonAll.Click;
  finally
    //if DataModuleOCK.SQLTransaction.Active then
    //begin
      //DataModuleOCK.SQLQueryProductData.Close;
      //DataModuleOCK.SQLTransaction.Commit;
    //end;
    screen.Cursor := crDefault;
  end;
end;

procedure TFormOpsiClientKiosk.SpeedButton1Click(Sender: TObject);
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

procedure TFormOpsiClientKiosk.SpeedButtonActionsClick(Sender: TObject);
begin
  TSpeedButton(Sender).Down:= True;
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

procedure TFormOpsiClientKiosk.SpeedButtonReloadClick(Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
     if DataModuleOCK.SQLTransaction.Active then ShowMessage('Transaction active Button reload click!');

     //ReloadDataFromServer;
    //FilterOnSearch;
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFormOpsiClientKiosk.Terminate;
begin
  Application.Terminate;
end;

procedure TFormOpsiClientKiosk.DBGrid1TitleClick(Column: TColumn);
var
  direction: string;
begin
  try
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
  end;
end;

procedure TFormOpsiClientKiosk.FormDestroy(Sender: TObject);
var
  counter, i: integer;
begin
  FilteredProductIDs.Free;
  DataModuleOCK.Free;
  OCKOpsiConnection.Free;
  try
    counter := length(ArrayAllProductTiles);
    if counter > 0 then
      //for i:=0 to counter -1 do ArrayAllProductTiles[i].Destroy;
      for i := 0 to counter - 1 do
      begin
        ArrayAllProductTiles[i].Free;
        ArrayAllProductTiles[i] := nil;
        logdatei.log('FreeAndNil(ProductTilesArray', LLDebug2);
      end;
    SetLength(ArrayAllProductTiles, 0);
    ArrayAllProductTiles := nil;
  except
    on e: Exception do
    begin
      logdatei.log('Exception FormClose: ProductTilesArray[i].Free;', LLError);
      logdatei.log('Exception: ' + E.message, LLError);
      logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
      logdatei.log_exception(E, LLError);
    end;
  end;
  LogDatei.log('FopsiClientKiosk.FormDestroy: Application terminates', LLInfo);
  LogDatei.Close;
  LogDatei.Free;
end;

procedure TFormOpsiClientKiosk.GrouplistEnter(Sender: TObject);
begin
  PanelProductDetail.Height := 0;
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
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
  DataModuleOCK.SQLQueryProductData.Filter := 'UpdatePossible and InstallationStatus <> ""';
  DataModuleOCK.SQLQueryProductData.Filtered := True;
  TSpeedButton(Sender).Down:= True;
  SetView;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareBackClick(Sender: TObject);
begin
  //DataModuleOCK.SQLQueryProductData.Close;
  //DataModuleOCK.SQLTransaction.Commit;
  if SpeedButtonExpertMode.Down then FormOpsiClientKiosk.PanelExpertMode.Visible := True;
  FormOpsiClientKiosk.PanelToolbar.Visible := True;
  FormOpsiClientKiosk.NotebookProducts.PageIndex:=1;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareUninstallClick(Sender: TObject);
begin
  if SoftWareOnDemand then
  begin
    case QuestionDLG('Uninstall Dialog','Uninstall now, later or cancel action?',mtConfirmation,
                     [mrYes, 'Uninstall now', mrNo, 'Uninstall later', mrCancel], 0)
    of
      mrYes: begin
               SetActionRequest('uninstall', ' will be uninstalled now.', True);
             end;
      mrNo: begin
              //ShowMessage('Please wait while sending request to server...');
              SetActionRequest('uninstall',
                ' will be uninstalled at next standard event (e.g. reboot).', False);
            end;
    end;//case
  end//if SoftwareOndemand
  else
   if QuestionDLG('','Do you want to uninstall ' + LabelSoftwareName.Caption + '?',
               mtConfirmation,[mrYes, 'Yes', mrNo, 'No'], 0) = mrYes then
   begin
     SetActionRequest('uninstall',
       ' will be uninstalled at next standard event (e.g. reboot).', False);
   end;
end;//procedure ButtonSoftwareUninstallClick



procedure TFormOpsiClientKiosk.DBComboBox1Change(Sender: TObject);
begin
  //DataModuleOCK.SQLQueryProductData.Edit;
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

procedure TFormOpsiClientKiosk.BitBtnInfoClick(Sender: TObject);
begin
  ShowMessage('opsi-kiosk-client' + LineEnding + 'Display language: ' +
    GetDefaultLang + Lineending + 'Version: ' + ProgramInfo.Version + Lineending +
    'CopyRight: uib gmbh (http://uib.de) under AGPLv3' + LineEnding +
    'http://opsi.org' + Lineending + 'Credits to: Lazarus/FPC,indy,sqllite,superobject');
end;

procedure TFormOpsiClientKiosk.BitBtnStoreActionClick(Sender: TObject);
begin
  screen.Cursor := crHourGlass;
  try
    DataModuleOCK.SQLQueryProductData.Filtered := False;
    DataModuleOCK.SQLQueryProductData.Filter := 'ActionRequest  <> ""';
    DataModuleOCK.SQLQueryProductData.Filtered := True;

    DataModuleOCK.SQLQueryProductData.First;
    while not DataModuleOCK.SQLQueryProductData.EOF do
    begin
      OCKOpsiConnection.setActionrequest(DataModuleOCK.SQLQueryProductData.FieldByName('ProductID').AsString,
        DataModuleOCK.SQLQueryProductData.FieldByName('ActionRequest').AsString);
      DataModuleOCK.SQLQueryProductData.Next;
    end;
    installdlg.Finstalldlg.Memo1.Text := OCKOpsiConnection.getActionrequests.Text;
    if installdlg.Finstalldlg.Memo1.Text = '' then
       installdlg.Finstalldlg.Memo1.Text := rsNoActionsFound;
    installdlg.Finstalldlg.Show;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormOpsiClientKiosk.BitBtnToggleViewClick(Sender: TObject);
begin
  if NotebookProducts.PageIndex = 1 then NotebookProducts.PageIndex := 0
    else if NotebookProducts.PageIndex = 0 then NotebookProducts.PageIndex := 1;
  FormOpsiClientKiosk.PanelProductDetail.height := 0;
end;

procedure TFormOpsiClientKiosk.ButtonReadConfigStateClick(Sender: TObject);
var
  ConfigState,backendOptions: TStringList;
begin
  ConfigState := OCKOpsiConnection.getConfigState('software-on-demand.kiosk.allowed');
  ConfigState.free;
  //backendOptions := OCKOpsiConnection.getbackendOptions;
  //backendOptions.free;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareInstallClick(Sender: TObject);
begin
  if SoftWareOnDemand then
  begin
    case QuestionDLG('Install Dialog','Install now, install later at next standard event (e.g. reboot) or cancel action?',mtConfirmation,
                     [mrYes, 'Now', mrNo, 'Next standard event (e.g. reboot)', mrCancel], 0)
    of
      mrYes: begin
               SetActionRequest('setup', ' will be installed now.', True);
             end;
      mrNo: begin
              //ShowMessage('Please wait while sending request to server...');
              SetActionRequest('setup',
                ' will be installed at next standard event (e.g. reboot).',False);
            end;
    end;//case
  end//if SoftwareOndemand
  else
   if QuestionDLG('','Do you want to install ' + LabelSoftwareName.Caption + '?',
               mtConfirmation,[mrYes, 'Yes', mrNo, 'No'], 0) = mrYes then
   begin
     SetActionRequest('setup',
       ' will be installed at next standard event (e.g. reboot).', False);
   end;
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
  DataModuleOCK.SQLQueryProductData.Filter := 'InstallationStatus = ""';
  DataModuleOCK.SQLQueryProductData.Filtered := True;
  DataModuleOCK.SQLQueryProductData.First;
  TSpeedButton(Sender).Down:= True;
  SetView;
end;
procedure TFormOpsiClientKiosk.SpeedButtonExpertModeClick(Sender: TObject);
begin
  { Expert mode }
  if SpeedButtonExpertMode.Down then
  begin
    PanelExpertMode.Visible := True;
    //RadioGroupView.ItemIndex := 0;
    //SetListView;
    BitBtnStoreAction.Caption := rsStoreActions;
    BitBtnStoreAction.Hint := rsStoreActionsHint;
    // localize RadioGroupView
    RadioGroupView.Items[0] := rsViewList;
    RadioGroupView.Items[1] := rsViewTiles;
    // List view
    NotebookProducts.PageIndex := RadioGroupView.ItemIndex;
  end
  else
  { Standard mode }
  begin
    PanelExpertMode.Visible := False;
    PanelProductDetail.Height := 0;
    BitBtnStoreAction.Caption := rsInstallNow;
    BitBtnStoreAction.Hint := rsInstallNowHint;
    NotebookProducts.PageIndex := 1;
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
var
  ErrorMsg    : String;
  ListOptions,
  ConfigState : TStringList;
  MyClientID  :String;
begin
  if not StartupDone then
  begin
    StartupDone := True;

    { quick check parameters }
    try
      { quick check parameters }
      ListOptions := TStringList.Create;
      ListOptions.Append('fqdn::');
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

      if Application.HasOption('fqdn') then
      begin
        MyClientID := Application.GetOptionValue('fqdn');
        LogDatei.log('MyClientID (option = fqdn): ' + MyClientID, LLDebug);
      end;
      if Application.HasOption('lang') then
      begin
        SetDefaultLang(Application.GetOptionValue('lang'));
      end;

      { Create connection to Opsi }

      FormProgressWindow.LabelInfo.Caption := 'Please wait while communicating with OPSI web server ...';
      FormProgressWindow.LabelDataLoad.Caption := 'Communicating with server ...';
      FormProgressWindow.LabelDataLoadDetail.Caption := 'Connecting to server';
      FormProgressWindow.ProgressBar1.StepIt;
      FormProgressWindow.ProgressBarDetail.Position := 50;
      //FormProgressWindow.ProgressBarDetail.StepIt;
      //FormProgressWindow.ProcessMess;
      Application.ProcessMessages;
      OCKOpsiConnection := TOpsiConnection.Create(true, MyClientID);
      FormProgressWindow.LabelDataLoad.Caption := 'Connected to '+
        OCKOpsiConnection.myservice_url + ' as ' + OCKOpsiConnection.myclientid;
      StatusBar1.Panels[0].Text := 'Connected to '+
        OCKOpsiConnection.myservice_url + ' as ' + OCKOpsiConnection.myclientid;
      FormProgressWindow.ProgressBar1.StepIt;
      Application.ProcessMessages; //FormProgressWindow.ProcessMess;

      { Load data from server }

      FormProgressWindow.LabelDataLoadDetail.Caption := 'Loading product data from server';
      FormProgressWindow.ProgressBar1.StepIt;
      FormProgressWindow.ProgressBarDetail.Position := 100;
      //FormProgressWindow.Repaint;
      Application.ProcessMessages;//FormProgressWindow.ProcessMess;
      OCKOpsiConnection.GetJSONFromServer;
      ConfigState := TSTringList.Create;
      ConfigState := OCKOpsiConnection.GetConfigState('software-on-demand.kiosk.allowed');
      //ShowMessage(ConfigState.Text);
      SoftwareOnDemand := StrToBool(ConfigState.Strings[0]);
      //FormProgressWindow.ProgressBarDetail.Position := 100;
      //Application.ProcessMessages;

      { Initialize database and tables and copy opsi product data to database }

      DataModuleOCK := TDataModuleOCK.Create(nil);
      DataModuleOCK.CreateDatabaseAndTables;
      //if DataModuleOCK.SQLTransaction.Active then ShowMessage('Transaction active after Initdatabse!');
      LogDatei.log('start OpsiProductsToDataset', LLNotice);
      DataModuleOCK.OpsiProductsToDataset(DataModuleOCK.SQLQueryProductData);
      DataModuleOCK.LoadTableProductsIntoMemory;
      //if DataModuleOCK.SQLTransaction.Active then ShowMessage('Transaction active after OpsiProducts!');
      //DBGrid1.DataSource := DataSourceProductData;
      //DBGrid2.DataSource := DataSourceProductDependencies;

       { Initialize GUI }

       BuildProductTiles(ArrayAllproductTiles, 'FlowPanelAllTiles');
      //if DataModuleOCK.SQLTransaction.Active then ShowMessage('Transaction active after BuildProductTiles!');
      FormProgressWindow.Close;
      { log }
      LogDatei.log('rsActSetup is: ' + rsActSetup , LLDebug2);
      LogDatei.log('TitleLabel.Caption: ' + TitleLabel.Caption, LLDebug2);
    //end;
    finally
      ListOptions.Free;
      ConfigState.Free;
    end;
  end;
end;

procedure TFormOpsiClientKiosk.FormShow(Sender: TObject);
begin
  //FormProgressWindow.LabelDataload.Caption := 'Init';
  //StartupDone := False;
  //FormOpsiClientKiosk.Repaint;
  //Application.ProcessMessages;
end;


procedure TFormOpsiClientKiosk.ReloadDataFromServer;
var
  i: integer;
begin
  OCKOpsiConnection.GetJSONFromServer;
  DataModuleOCK.OpsiProductsToDataset(DataModuleOCK.SQLQueryProductData);
  //StartupDone := False;
  RadioGroupViewSelectionChanged(self);
  //StartupDone := True;
end;

procedure TFormOpsiClientKiosk.EditSearchChange(Sender: TObject);
begin
  if EditSearch.Text <> '' then
  begin
    SearchProducts;
    SetView;
  end
  else
  begin
    if SpeedButtonAll.Down then
    begin
      DataModuleOCK.SQLQueryProductData.Filtered := False;
      SetView;
    end;
    if SpeedButtonUpdates.Down then
    begin
      DataModuleOCK.SQLQueryProductData.Filtered := False;
      DataModuleOCK.SQLQueryProductData.Filter := 'InstallationStatus <> "" and UpdatePossible = "true"';
      DataModuleOCK.SQLQueryProductData.Filtered := True;
      SetView;
    end;
    if SpeedButtonNotInstalled.Down then
    begin
      DataModuleOCK.SQLQueryProductData.Filtered := False;
      DataModuleOCK.SQLQueryProductData.Filter := 'InstallationStatus = ""';
      DataModuleOCK.SQLQueryProductData.Filtered := True;
      SetView;
    end;
  end;
end;

procedure TFormOpsiClientKiosk.LoadSkin(SkinPath: string);
var
  myini: TInifile;
begin
  if FileExists(skinpath + 'opsiclientkiosk.ini') then
  begin
    LogDatei.Log('loading skin for title from: ' + skinpath + 'opsiclientkiosk.ini', LLEssential);
    myini := TIniFile.Create(skinpath + 'opsiclientkiosk.ini');
    //title
    TitleLabel.Caption := myini.ReadString('TitleLabel', 'Text', 'Opsi Client Kiosk');
    LogDatei.log('Title_Caption: ' + TitleLabel.Caption, LLDebug);
    TitleLabel.Font.Name := myini.ReadString('TitleLabel', 'FontName', 'Arial');
    TitleLabel.Font.Size := myini.ReadInteger('TitleLabel', 'FontSize', 20);
    TitleLabel.Font.Color := StringToColor(myini.ReadString('TitleLabel', 'FontColor', 'clBlack'));
    TitleLabel.Font.Bold := StrToBool(myini.ReadString('TitleLabel','FontBold','True'));
    TitleLabel.Font.Italic := StrToBool(myini.ReadString('TitleLabel', 'FontItalic','False'));
    TitleLabel.Font.Underline := StrToBool(myini.ReadString('TitleLabel', 'FontUnderline', 'False'));
    myini.Free;
  end;
end;

procedure TFormOpsiClientKiosk.FormCreate(Sender: TObject);
begin
  StartUpDone := False;
  InitLogging('kiosk-' + GetUserName_ +'.log', self.Name + '.FormCreate', LLDebug);
  LogDatei.log('Initialize Opsi Client Kiosk', LLNotice);
  // is an other instance running ?
  if numberOfProcessInstances(ExtractFileName(ParamStr(0))) > 1 then
  begin
    LogDatei.log('An other instance of this program is running - so we abort', LLCritical);
    LogDatei.Close;
    LogDatei.Free;
    ShowMessage('An other instance of this program is running - so we abort');
    halt(1);
  end;
  // is opsiclientd running ?
  if numberOfProcessInstances('opsiclientd') < 1 then
  begin
    LogDatei.log('opsiclientd is not running - so we abort', LLCritical);
    LogDatei.Close;
    LogDatei.Free;
    ShowMessage('opsiclientd is not running - so we abort');
    halt(1);
  end;
  //ShowMessage('Form Create');
  FilteredProductIDs := TStringList.Create;
  NotebookProducts.PageIndex := 1;  //tiles
  PanelProductDetail.Height := 0;
  detail_visible := False;
  // Load custom skin

  { Set skin }

  skinpath := Application.Location + 'opsiclientkioskskin' + PathDelim;
  if FileExistsUTF8(skinpath + 'opsiclientkiosk.png') then
  begin
    ImageHeader.Picture.LoadFromFile(skinpath + 'opsiclientkiosk.png');
  end;
  if FileExists(skinpath + 'opsiclientkiosk.ini') then
  begin
    LoadSkin(skinpath);
  end;

  // skinpath in opsiclientagent custom dir
  skinpath := Application.Location +
    '..\custom\opsiclientkioskskin' + PathDelim;
  if FileExistsUTF8(skinpath + 'opsiclientkiosk.png') then
  begin
    ImageHeader.Picture.LoadFromFile(skinpath + 'opsiclientkiosk.png');
  end;
  if FileExistsUTF8(skinpath + 'opsiclientkiosk.ini') then
  begin
    LoadSkin(skinpath);
  end;

  GetDefaultLang;
  LogDatei.log('GetDefaultLang: ' + GetDefaultLang, LLEssential);
  //grouplist.Clear;
  DBGrid1.Columns.Add.FieldName := 'ProductId';
  DBGrid1.Columns.Items[0].Title.Caption := 'ProductId';
  DBGrid1.Columns.Items[0].Width := 150;
  DBGrid1.Columns.Add.FieldName := 'ProductName';
  DBGrid1.Columns.Items[1].Width := 300;
  DBGrid1.Columns.Add.FieldName := 'InstallationStatus';
  DBGrid1.Columns.Items[2].Title.Caption := 'InstallationStatus';
  DBGrid1.Columns.Items[2].Width := 100;
  DBGrid1.Columns.Add.FieldName := 'actionrequest';
  DBGrid1.Columns.Items[3].Title.Caption := 'ActionRequest';
  DBGrid1.Columns.Items[3].Width := 100;
  //DBGrid1.Columns.Add.FieldName := 'updatePossible';
  //DBGrid1.Columns.Items[4].Title.Caption := 'updatePossible';
  //DBGrid1.Columns.Items[4].Width := 100;


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

  // localize RadioGroupView
  RadioGroupView.Items[0] := rsViewList;
  RadioGroupView.Items[1] := rsViewTiles;
end;

procedure TFormOpsiClientKiosk.EditSearchEnter(Sender: TObject);
begin
  PanelProductDetail.Height := 0;
end;


procedure TFormOpsiClientKiosk.SearchProducts;
var
  i:integer;
  Filtercond, Filterstr: string;
begin
  try
  //if not DataModuleOCK.SQLTransaction.Active then
  //begin
    //DataModuleOCK.SQLTransaction.StartTransaction;
    //DataModuleOCK.SQLQueryProductData.Open;
  //end;

    Filtercond := '"*' + EditSearch.Text + '*"';
    LogDatei.log('Search for: ' + EditSearch.Text + ' Filter for: ' +
      Filtercond, LLinfo);
    Filterstr := '(ProductId =' + Filtercond;
    Filterstr := Filterstr + 'or ProductName =' + Filtercond;
    Filterstr := Filterstr + 'or DESCRIPTION =' + Filtercond;
    Filterstr := Filterstr + 'or ADVICE =' + Filtercond;
    if SpeedButtonAll.Down then
      Filterstr := Filterstr + 'or INSTALLATIONSTATUS =' + Filtercond +')';
    if SpeedButtonNotInstalled.Down then
      Filterstr := Filterstr + ') and INSTALLATIONSTATUS = ""';
    if SpeedButtonUpdates.Down then
       Filterstr := Filterstr + ' ) and INSTALLATIONSTATUS <> "" and UpdatePossible = "true"';
    DataModuleOCK.SQLQueryProductData.Filtered := False;
    DataModuleOCK.SQLQueryProductData.Filter := Filterstr;
    DataModuleOCK.SQLQueryProductData.FilterOptions := [foCaseInsensitive];
    DataModuleOCK.SQLQueryProductData.Filtered := True;
    DataModuleOCK.SQLQueryProductData.First;
  finally
    //if DataModuleOCK.SQLTransaction.Active then
    //begin
      //DataModuleOCK.SQLQueryProductData.Close;
      //DataModuleOCK.SQLTransaction.Commit;
    //end;
  end;
  //if not SpeedButtonExpertMode.Down then
end;

procedure TFormOpsiClientKiosk.SpeedButtonAllClick(Sender: TObject);
begin
  EditSearch.Clear;
  DataModuleOCK.SQLQueryProductData.Filtered := False;
  TSpeedButton(Sender).Down:= True;
  SetView;
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
    counter := length(ArrayAllProductTiles);
    if counter > 0 then
      //for i:=0 to counter -1 do ArrayAllProductTiles[i].Destroy;
      for i := 0 to counter - 1 do
      begin
        ArrayAllProductTiles[i] := nil;
        ArrayAllProductTiles[i].Free;
        logdatei.log('FreeAndNil(ProductTilesArray', LLDebug2);
      end;
    SetLength(ArrayAllProductTiles, 0);

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

{:Returns user name of the current thread.
  @author  Miha-R, Lee_Nover
  @since   2002-11-25
}
function TFormOpsiClientKiosk.GetUserName_: string;
var
  buffer: PChar;
  bufferSize: DWORD;
begin
  bufferSize := 256; //UNLEN from lmcons.h
  buffer := AllocMem(bufferSize * SizeOf(char));
  try
    GetUserName(buffer, bufferSize);
    Result := string(buffer);
  finally
    FreeMem(buffer, bufferSize);
  end;
end; { DSiGetUserName}


function TFormOpsiClientKiosk.InitLogging(const LogFileName, CallingMethod: string;
  MyLogLevel:integer): boolean;
begin
  LogDatei := TLogInfo.Create;
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  LogDatei.CreateTheLogfile(LogFileName, False);
  LogDatei.LogLevel := MylogLevel;
  LogDatei.log(Application.Name + ' starting at ' + DateTimeToStr(now), LLEssential);
  LogDatei.log('opsi-client-kiosk: version: ' + ProgramInfo.Version, LLEssential);
  LogDatei.log('Initialize Logging,  calling method='+ CallingMethod, LLNotice);
  InitLogging := True;
end;

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
