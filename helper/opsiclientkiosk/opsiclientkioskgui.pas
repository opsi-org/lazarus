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
  Classes, SysUtils, DB, ExtendedNotebook, DividerBevel, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, Grids, DBGrids,
  DBCtrls, ockdata,CommCtrl, BufDataset, typinfo, installdlg, lcltranslator,
  ActnList, Menus, oslog, inifiles, Variants, Lazfileutils, Types,
  opsiconnection,
  jwawinbase,
  osprocesses,
  progresswindow,datadb, proginfo;

type

  { TProductPanel }

  TProductPanel = class(TPanel)
    ShapeRoundSquare : TShape; //just for a nice look
    LabelId: TLabel;
    LabelName: TLabel;
    LabelState: TLabel;
    ImageIcon : TImage; //Icon of the program
    RadioGroupAction: TGroupbox;
    rbsetup: TRadiobutton;
    rbNone: TRadiobutton;
    rbuninstall: TRadiobutton;
    lbsetup: TLabel;
    lbnone: TLabel;
    lbuninstall: TLabel;

    procedure TileActionChanged(Sender: TObject);
    //procedure ProductTileClick(Sender: TObject);
    //procedure ProductTileChildClick(Sender: TObject);
    procedure TileClick(Sender:TObject);
    procedure Scroll(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint; var Handled: boolean);
    procedure ProductTileMouseEnter(Sender :TObject);
    procedure ProductTileMouseLeave(Sender :TObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TWinControl);overload;
    destructor Destroy;override;
  end;

  TPanels = array of TProductPanel;


  { TFormOpsiClientKiosk }

  TFormOpsiClientKiosk = class(TForm)
    BitBtn1: TBitBtn;
    BitBtnInfo: TBitBtn;
    BitBtnShowAction: TBitBtn;
    BitBtnStoreAction: TBitBtn;
    ButtonSoftwareUpdate: TButton;
    ButtonSoftwareInstall: TButton;
    ButtonSoftwareBack: TButton;
    DBComboBox1: TDBComboBox;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBMemoAdvice: TDBMemo;
    DBMemoSoftwareAdvice: TDBMemo;
    DBMemoDescription: TDBMemo;
    DBMemoSoftwareDescription: TDBMemo;
    DBTextSoftwareClientVerStr: TDBText;
    DBTextPriority: TDBText;
    DBTextVerStr: TDBText;
    DBTextClientVerStr: TDBText;
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
    SpeedButtonUpdates: TSpeedButton;
    BtnClearSearchEdit: TSpeedButton;
    SpeedButtonReload: TSpeedButton;
    SpeedButtonAll: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TimerSearchEdit: TTimer;
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
    procedure ButtonSoftwareInstallClick(Sender: TObject);
    //procedure BtnActionClick(Sender: TObject);
    procedure ButtonSoftwareBackClick(Sender: TObject);
    procedure DBComboBox1Exit(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure DBGrid1ColExit(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1Enter(Sender: TObject);
    procedure DBGrid1Exit(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure FlowPanelUpdateTilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GrouplistEnter(Sender: TObject);
    procedure RadioGroupViewSelectionChanged(Sender: TObject);
    procedure ScrollBoxUpdateTilesClick(Sender: TObject);
    procedure ScrollBoxAllTilesMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure BtnClearSearchEditClick(Sender: TObject);
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
    procedure TimerSearchEditTimer(Sender: TObject);
    procedure FilterOnSearch;
    procedure BuildProductTiles(var ArrayProductTiles:TPanels; const OwnerName:string);
    function GetTileIDbyProductID(const ProductID:String):integer;
    procedure SetTileIDbyProductID(const TileID, ProductID:String);
  private
    { private declarations }
    StringListTileIDs:TStringList;
  public
    { public declarations }
  end;

  Tmythread2 = class(TThread)
  public
    procedure Execute; override;
  end;

procedure main;

var
  FormOpsiClientKiosk: TFormOpsiClientKiosk;
  StartupDone: boolean;
  ArrayAllProductTiles: TPanels;
  ArraySearchPanelTiles: TPanels;
  InTileRebuild: boolean = False;
  LastOrderDirAsc: boolean = True;
  LastOrderCol: string;
  detail_visible: boolean = False;
  skinpath: string;
  preLogfileLogList: TStringList;

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
  clUpdate = $00FF8000;
  clNotInstalled = $000080FF;
  clUnknown = clRed;

  //Path to programm icons
  IconPathCustom = '.'+PathDelim + 'progam_icons' + PathDelim + 'custom' + PathDelim;//Application.Location + PathDelim + 'progam_icons' + PathDelim + 'custom' + PathDelim;
  IconPathDefault = '.'+PathDelim + 'progam_icons' + PathDelim + 'default' + PathDelim;

  //Path to screenshots
  ScreenshotPath = '.'+PathDelim + 'screenshots' + PathDelim;

var
  mythread: Tmythread2;
  //title
  title_color: string;
  title_Font_Name: string;
  title_Font_Size: integer;
  title_Font_Color: string;
  title_Font_Bold: boolean;
  title_Font_Italic: boolean;
  title_Font_Underline: boolean;
  title_Text: string;
  //tile
  tile_color: string;
  tile_Font_Name: string;
  tile_Font_Size: integer;
  tile_Font_Color: string;
  tile_Font_Bold: boolean;
  tile_Font_Italic: boolean;
  tile_Font_Underline: boolean;
  tile_width: integer;
  tile_height: integer;
  //TileRadio
  tile_radio_setup_color: string;
  tile_radio_uninstall_color: string;
  tile_radio_none_color: string;
  tile_radio_font_size: integer;

  former_selected_tile: integer;//index of the former selected tile


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
  IconPath : String;
begin
  try
    inherited Create(TheOwner);
    parent := theOwner;
    Width := tile_width;
    Height := tile_height;
    //FlowStyle := fsTopBottomLeftRight;
    //self.OnClick := ProductTileClick;
    //BorderStyle := bsSingle;
    BorderStyle := bsNone;
    //BorderWidth := 4;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    BorderSpacing.Around := 5;
    Color := StringToColor(tile_color);
    Font.Name := tile_Font_Name;
    font.Size := tile_Font_Size;
    font.Color := StringToColor(tile_Font_Color);
    font.Bold := tile_Font_Bold;
    font.Italic := tile_Font_Italic;
    font.Underline := tile_Font_Underline;
    self.OnMouseWheel := scroll;

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
      font.Italic := tile_Font_Italic;
      font.Underline := tile_Font_Underline;
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
    LabelID := TLabel.Create(self);
    with LabelID do begin
      Parent := self;
      Caption := 'id';
      Font.Bold := tile_Font_Bold;
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
    //IconPath := Application.Location + PathDelim + 'progam_icons' + PathDelim + 'default'+ PathDelim + 'opsi-logo.png';
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

    {*
    //RadioGroupAction
    //RadioGroupAction := TRadioGroup.Create(self);
    RadioGroupAction := TGroupbox.Create(self);
    //RadioGroupAction.Parent := self;
    RadioGroupAction.Caption := rsActRequest;
    RadioGroupAction.BorderSpacing.Left := 3;
    RadioGroupAction.Font.Size := tile_radio_font_size;
    RadioGroupAction.AutoSize:= false;
    //RadioGroupAction.Alignment := taCenter;
    RadioGroupAction.Align := alNone;
    RadioGroupAction.Top := 120;
    RadioGroupAction.Left := 0;
    //RadioGroupAction.OnClick := self.OnClick;
    RadioGroupAction.OnClick := ProductTileChildClick;
    RadioGroupAction.OnMouseWheel := scroll;
    //RadioGroupAction.OnSelectionChanged := TileActionChanged;

    // radiobuttons
    // none
    rbNone := TRadioButton.Create(RadioGroupAction);
    rbNone.Caption := '';
    rbNone.Top := 50;
    rbNone.Align := alTop;
    rbNone.Parent := RadioGroupAction;
    rbNone.OnChange := TileActionChanged;
    rbNone.Tag := 0;
    rbNone.OnMouseWheel := scroll;
    lbnone := TLabel.Create(RadioGroupAction);
    lbnone.Caption := rsActNone;
    lbnone.Font.Color := StringToColor(tile_radio_none_color);
    lbnone.Parent := RadioGroupAction;
    lbnone.top := rbNone.top;
    lbnone.Left := 30;
    lbnone.Font.Size := tile_radio_font_size;
    lbnone.OnClick := rbnone.OnClick;
    lbnone.OnMouseWheel := scroll;
    lbnone.BringToFront;

    // setup
    rbsetup := TRadioButton.Create(RadioGroupAction);
    rbsetup.Caption := '';
    rbsetup.Top := 50;
    rbsetup.Align := alTop;
    rbsetup.Parent := RadioGroupAction;
    rbsetup.OnChange := TileActionChanged;
    rbsetup.Tag := 1;
    rbsetup.Enabled := False;
    rbsetup.OnMouseWheel := scroll;
    lbsetup := TLabel.Create(RadioGroupAction);
    lbsetup.Caption := rsActSetup;
    lbsetup.Font.Color := StringToColor(tile_radio_setup_color);
    lbsetup.Parent := RadioGroupAction;
    lbsetup.top := rbsetup.top;
    lbsetup.Left := 30;
    lbsetup.Font.Size := tile_radio_font_size;
    lbsetup.Enabled := False;
    lbsetup.OnClick := rbsetup.OnClick;
    lbsetup.OnMouseWheel := scroll;
    lbsetup.BringToFront;
    //iconsetup:= Timage.Create(self);
    //iconsetup.Parent := RadioGroupAction;
    //iconsetup.top := rbsetup.top;
    //iconsetup.Height:= lbsetup.Height;
    //iconsetup.Align:=alRight;
    //iconsetup.Picture.LoadFromFile(skinpath+Pathdelim+'setup.png');

    // uninstall
    rbuninstall := TRadioButton.Create(RadioGroupAction);
    rbuninstall.Caption := '';
    rbuninstall.Top := 50;
    rbuninstall.Align := alTop;
    rbuninstall.Parent := RadioGroupAction;
    rbuninstall.OnChange := TileActionChanged;
    rbuninstall.Tag := 2;
    rbuninstall.Enabled := False;
    rbuninstall.OnMouseWheel := scroll;
    lbuninstall := TLabel.Create(RadioGroupAction);
    lbuninstall.Caption := rsActUninstall;
    lbuninstall.Font.Color := StringToColor(tile_radio_uninstall_color);
    lbuninstall.Parent := RadioGroupAction;
    lbuninstall.top := rbuninstall.top;
    lbuninstall.Left := 30;
    lbuninstall.Font.Size := tile_radio_font_size;
    lbuninstall.Enabled := False;
    lbuninstall.OnClick := rbuninstall.OnClick;
    lbuninstall.OnMouseWheel := scroll;
    lbuninstall.BringToFront;

    RadioGroupAction.Height := (lbuninstall.Height + 9) * 3;
    *}
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
      FreeAndNil(labelId);
      FreeAndNil(LabelName);
      FreeAndNil(ImageIcon);
      FreeAndNil(ShapeRoundSquare);
      FreeAndNil(LabelState);
      FreeAndNil(rbsetup);
      FreeAndNil(rbNone);
      FreeAndNil(rbuninstall);
      FreeAndNil(lbsetup);
      FreeAndNil(lbnone);
      FreeAndNil(lbuninstall);
      FreeAndNil(RadioGroupAction);
      //ButtonSoftwareBack.Destroy;
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

procedure TProductPanel.TileActionChanged(Sender: TObject);
var
  pid, actionstr: string;
  tileindex, actionindex: integer;
begin
  if not inTileRebuild then
  begin
    tileindex := TRadioButton(Sender).Parent.Parent.Tag;
    //actionindex := ArrayAllProductTiles[tileindex].RadioGroupAction.ItemIndex;
    actionindex := TRadioButton(Sender).Tag;
    pid := ArrayAllProductTiles[tileindex].LabelId.Caption;
    ZMQueryDataSet1.First;
    if ZMQueryDataSet1.Locate('ProductId', VarArrayOf([pid]),
      [loCaseInsensitive]) then
    begin
      ZMQueryDataSet1.Edit;
      case actionindex of
        0: actionstr := 'none';
        1: actionstr := 'setup';
        2: actionstr := 'uninstall';
        else
          logdatei.log('Error: Unexpected tag number in TileActionChanged: ' +
            IntToStr(actionindex), LLError);
      end;
      ZMQueryDataSet1.FieldByName('actionrequest').AsString := actionstr;
      FormOpsiClientKiosk.PanelProductDetail.Height := 185;
      ZMQueryDataSet1.Post;
    end;
  end;
end;

{*procedure TProductPanel.ProductTileClick(Sender: TObject);
var
  pid: string;
  tileindex: integer;
begin
  if not inTileRebuild then
  begin
    tileindex := TProductPanel(Sender).Tag;
    pid := ArrayAllProductTiles[tileindex].LabelId.Caption;
    ZMQueryDataSet1.First;
    if ZMQueryDataSet1.Locate('ProductId', VarArrayOf([pid]),
      [loCaseInsensitive]) then
    begin
      if detail_visible then
      begin
        FormOpsiClientKiosk.PanelProductDetail.Height := 0;
        detail_visible := False;
      end
      else
      begin
        FormOpsiClientKiosk.PanelProductDetail.Height := 185;
        detail_visible := True;
      end;
    end;
  end;
end;*}


{*procedure TProductPanel.ProductTileChildClick(Sender: TObject);
var
  pid: string;
  tileindex: integer;
begin
  if not inTileRebuild then
  begin
    tileindex := TControl(Sender).Parent.Tag; //type convertion to TControl (all visual components are of this type), parent is the Panel
    //if tileindex <> former_slected_tile
    pid := ArrayAllProductTiles[tileindex].LabelID.Caption;
    ZMQueryDataSet1.First;
    if ZMQueryDataSet1.Locate('ProductId', VarArrayOf([pid]),[loCaseInsensitive]) then
    begin
      if detail_visible then
      begin
        FormOpsiClientKiosk.PanelProductDetail.Height := 0;
        ArrayAllProductTiles[tileindex].ShapeRoundSquare.Pen.Color:=clBlack;
        ArrayAllProductTiles[former_selected_tile].ShapeRoundSquare.Pen.Color:=clBlack;
        ArrayAllProductTiles[former_selected_tile].ShapeRoundSquare.Brush.Color:=clWhite;
        detail_visible := False;
      end
      else
      begin
        FormOpsiClientKiosk.NotebookProducts.PageIndex := 2;
        FormOpsiClientKiosk.LabelSoftwareName.Caption := TControl(Sender).Parent.FindChildControl('LabelName').Caption;
        FormOpsiClientKiosk.PanelProductDetail.Height := 185;
        ArrayAllProductTiles[former_selected_tile].ShapeRoundSquare.Pen.Color:=clBlack;
        ArrayAllProductTiles[tileindex].ShapeRoundSquare.Pen.Color:=clBlue;
        ArrayAllProductTiles[tileindex].ShapeRoundSquare.Brush.Color:=$00FEEFD3;
        detail_visible := True;
        former_selected_tile := tileindex;
      end;
    end;
  end;
end;*}

procedure TProductPanel.TileClick(Sender: TObject);
var
  TileIndex :integer;
  pid : String;
begin
  try
    TileIndex := TControl(Sender).Parent.Tag;
    pid := ArrayAllProductTiles[TileIndex].LabelID.Caption;
    DataModuleOCK.SQLQuery1.Open;
    DataModuleOCK.SQLQuery1.First;
    if DataModuleOCK.SQLQuery1.Locate('ProductId', VarArrayOf([pid]),[loCaseInsensitive]) then
    begin
      FormOpsiClientKiosk.PanelProductDetail.Height := 0;
      FormOpsiClientKiosk.NotebookProducts.PageIndex := 2;
      FormOpsiClientKiosk.PanelExpertMode.Visible := False;
      FormOpsiClientKiosk.PanelToolbar.Visible := False;
      FormOpsiClientKiosk.LabelSoftwareName.Caption := ArrayAllProductTiles[TileIndex].LabelName.Caption;
      FormOpsiClientKiosk.ImageIconSoftware.Picture:= ArrayAllProductTiles[TileIndex].ImageIcon.Picture;
      if FileExists(ScreenshotPath +'screenshot1_'+ ArrayAllProductTiles[TileIndex].LabelID.Caption +'.png') then
       FormOpsiClientKiosk.ImageScreenShot.Picture.LoadFromFile
         (ScreenshotPath +'screenshot1_'+ ArrayAllProductTiles[TileIndex].LabelID.Caption +'.png')
      else FormOpsiClientKiosk.ImageScreenShot.Picture.LoadFromFile
           (ScreenshotPath + 'no_screenshot.png');
      if ArrayAllProductTiles[TileIndex].LabelState.Caption = rsInstalled then
      begin
        FormOpsiClientKiosk.ButtonSoftwareInstall.Caption := 'Uninstall';
        if FormOpsiClientKiosk.DBTextSoftwareClientVerStr.Caption <> FormOpsiClientKiosk.DBTextSoftwareVerStr.Caption then
          FormOpsiClientKiosk.ButtonSoftwareUpdate.Enabled := True
        else FormOpsiClientKiosk.ButtonSoftwareUpdate.Enabled := False;
      end
      else if ArrayAllProductTiles[TileIndex].LabelState.Caption = rsNotInstalled then
      begin
        FormOpsiClientKiosk.ButtonSoftwareInstall.Caption := 'Install';
        FormOpsiClientKiosk.ButtonSoftwareUpdate.Enabled := False;
        //FormOpsiClientKiosk.
      end;
    end;
  finally
    DataModuleOCK.SQLQuery1.Close;
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

    //ProgressBar
    with FormProgressWindow do
    begin
      Visible := True;
      ProgressbarDetail.Position := 0;
      LabelInfo.Caption:= 'Please wait while building tils';
      LabelDataload.Caption := 'Clear Tiles';
    end;
    Application.ProcessMessages;

    //if array exists free content and set array length 0
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

    // use complete window for tiles
    FormOpsiClientKiosk.PanelProductDetail.Height := 0;

    // progess
    //FormProgressWindow.Progressbar1.Position := 80;
    with FormProgressWindow do
    begin
      LabelDataload.Caption := 'Fill Tiles';
      ProgressBar1.Step := 1;
      ProgressBar1.Max := ProgressBar1.Position + ZMQUerydataset1.RecordCount;
      ProgressBarDetail.Step := 1;
      ProgressBarDetail.Max := ZMQUerydataset1.RecordCount;
    end;
    Application.ProcessMessages;
    logdatei.log('BuildProductTiles from db start', LLDebug2);
    counter := 0;
    ZMQuerydataset1.Open;
    ZMQuerydataset1.First;
    while not ZMQuerydataset1.EOF do
    begin
      ProductID := ZMQueryDataSet1.FieldByName('ProductId').AsString;
      FormProgressWindow.LabelDataLoadDetail.Caption := ProductID;
      Application.ProcessMessages;
      SetLength(ArrayProductTiles, counter + 1);
      ArrayProductTiles[counter] := TProductPanel.Create(FormOpsiClientKiosk.FindComponent(OwnerName) as TFlowPanel);
      ArrayProductTiles[counter].LabelID.Caption := ProductID;
      ArrayProductTiles[counter].LabelName.Caption := ZMQueryDataSet1.FieldByName('ProductName').AsString;
      if FileExists(IconPathCustom + ProductID + '.png') then
        ArrayProductTiles[counter].ImageIcon.Picture.LoadFromFile(IconPathCustom + ProductID + '.png')
      else
        if FileExists(IconPathDefault + ProductID + '.png') then
          ArrayProductTiles[counter].ImageIcon.Picture.LoadFromFile(IconPathDefault + ProductID + '.png')
        else
          if FileExists(IconPathDefault + 'opsi-logo.png') then
            ArrayProductTiles[counter].ImageIcon.Picture.LoadFromFile(IconPathDefault + 'opsi-logo.png');
      state := ZMQueryDataSet1.FieldByName('installationStatus').AsString;
      if state = 'installed' then
      begin
        update := ZMQueryDataSet1.FieldByName('updatePossible').AsString;
        if update = 'True' then
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
      //radio group
      {*
      ArrayProductTiles[counter].rbsetup.Enabled := True;
      ArrayProductTiles[counter].lbsetup.Enabled := True;
      action := Trim(ockdata.ZMQUerydataset1.FieldByName('possibleAction').AsString);
      if (action = 'uninstall') then
      begin
        ArrayProductTiles[counter].rbuninstall.Enabled := True;
        ArrayProductTiles[counter].lbuninstall.Enabled := True;
      end;
      action := Trim(ockdata.ZMQUerydataset1.FieldByName('actionrequest').AsString);
      if (action = 'none') or (action = '') then
        ArrayProductTiles[counter].rbNone.Checked := True
      else if action = 'setup' then
        ArrayProductTiles[counter].rbsetup.Checked := True
      else if action = 'uninstall' then
        ArrayProductTiles[counter].rbuninstall.Checked := True;
      *}
      ArrayProductTiles[counter].Tag := counter;
      Inc(counter);
      //FormProgressWindow.ProgressbarDetail.Position := counter;
      FormProgressWindow.ProgressbarDetail.StepIt;
      FormProgressWindow.ProgressBar1.StepIt;
      Application.ProcessMessages;
      ZMQUerydataset1.Next;
    end;
  finally
    ZMQUerydataset1.Close;
    inTileRebuild := False;
    logdatei.log('BuildProductTiles stop', LLDebug2);
    {*with FormProgressWindow do
    begin
      //ProgressBar1.Position := 100;
      //ProgressBarDetail.Visible := False;
      //LabelDataLoadDetail.Visible := False;
      //ProgressBar1.Visible := False;
      //LabelDataLoad.Visible := False;

      //Visible := False;
    end;*}
    //FormOpsiClientKiosk.CheckBoxExpertModeChange(FormOpsiClientKiosk);
    FormProgressWindow.Visible := False;
  end;
end;

function TFormOpsiClientKiosk.GetTileIDbyProductID(const ProductID: String): integer;
begin
  Result := StrToInt(StringListTileIDs.Values[ProductID]);
end;

procedure TFormOpsiClientKiosk.SetTileIDbyProductID(const TileID, ProductID: String
  );
begin
  StringListTileIDs.Add(ProductID + '=' + TileID);
end;

procedure Tmythread2.Execute;
begin
  // write back action requests
  ockdata.ZMQUerydataset1.Filtered := False;
  ockdata.ZMQUerydataset1.Filter := 'ActionRequest  <> ""';
  ockdata.ZMQUerydataset1.Filtered := True;
  ZMQUerydataset1.First;
  while not ZMQUerydataset1.EOF do
  begin
    ockdata.setActionrequest(ZMQUerydataset1.FieldByName('ProductId').AsString,
      ZMQUerydataset1.FieldByName('ActionRequest').AsString);
    ZMQUerydataset1.Next;
  end;
  Terminate;
end;


{ TFormOpsiClientKiosk }

//procedure TFormOpsiClientKiosk.ProcessMess;
//begin
//  Application.ProcessMessages;
//end;

procedure TFormOpsiClientKiosk.RadioGroupViewSelectionChanged(Sender: TObject);
begin
  NotebookProducts.PageIndex := RadioGroupView.ItemIndex;
  if RadioGroupView.ItemIndex = 0 then
  begin
    ockdata.ZMQUerydataset1.Open;
  end;
  //if StartupDone then
    //if RadioGroupView.ItemIndex = 1 then
      //BuildProductTiles;
end;

procedure TFormOpsiClientKiosk.ScrollBoxUpdateTilesClick(Sender: TObject);
begin

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
    ockdata.ZMQUerydataset1.Open;
    ockdata.ZMQUerydataset1.Filtered := False;
    SpeedButtonAll.AllowAllUp:= False;
    SpeedButtonAll.Down := True;
    FormOpsiClientKiosk.NotebookProducts.PageIndex:= 1;
  finally
    ockdata.ZMQUerydataset1.Close;
    screen.Cursor := crDefault;
  end;
end;

procedure TFormOpsiClientKiosk.SpeedButtonReloadClick(Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
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

procedure TFormOpsiClientKiosk.DBGrid1TitleClick(Column: TColumn);
var
  direction: string;
begin
  try
    if ZMQueryDataSet1.Active then
      ZMQueryDataSet1.Close;
    ZMQUerydataset1.SQL.Clear;
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
    ZMQUerydataset1.SQL.Add('select * from products order by ' +
      Column.FieldName + direction);
    ZMQUerydataset1.Open;
  except
  end;
end;

procedure TFormOpsiClientKiosk.FlowPanelUpdateTilesClick(Sender: TObject);
begin

end;

procedure TFormOpsiClientKiosk.FormDestroy(Sender: TObject);
var
  counter, i: integer;
begin
  StringListTileIDs.Free;
  if opsidata <> nil then
  begin
    // do not send log
    //opsidata.finishOpsiConf;
    opsidata.Free;
  end;
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
end;


procedure TFormOpsiClientKiosk.SpeedButtonUpdatesClick(Sender: TObject);
var
  i : integer;
begin
  FormOpsiClientKiosk.NotebookProducts.PageIndex:= 1;
  i := 0;
  for i := 0 to Length(ArrayAllProductTiles)-1 do
  begin
    if ArrayAllProductTiles[i].LabelState.Caption <> rsUpdate then
      ArrayAllProductTiles[i].Visible := False
    else ArrayAllProductTiles[i].Visible := True;
  end;
  TSpeedButton(Sender).Down:= True;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareBackClick(Sender: TObject);
begin
  if SpeedButtonExpertMode.Down then FormOpsiClientKiosk.PanelExpertMode.Visible := True;
  FormOpsiClientKiosk.PanelToolbar.Visible := True;
  FormOpsiClientKiosk.NotebookProducts.PageIndex:=1;
end;

procedure TFormOpsiClientKiosk.DBComboBox1Exit(Sender: TObject);
begin
  DBGrid1.Repaint;
  if DBComboBox1.Text <> '' then
    ockdata.ZMQueryDataSet1.Post;
  //  ockdata.ZMQueryDataSet1.FieldByName('actionrequest').AsString := 'none';
  //ockdata.ZMQueryDataSet1.Post;
end;


procedure TFormOpsiClientKiosk.BitBtnShowActionClick(Sender: TObject);
var
  i: integer;
begin
  // write back action requests
  ockdata.ZMQuerydataset1.Filtered := False;
  //ockdata.ZMQUerydataset1.Filter := ' not ((ActionRequest = "") and (ActionRequest = "none"))';
  ockdata.ZMQuerydataset1.Filter := 'ActionRequest <> ""';
  ockdata.ZMQuerydataset1.Filtered := True;
  FormProgressWindow.ProcessMess;
  RadioGroupViewSelectionChanged(self);
end;

procedure TFormOpsiClientKiosk.BitBtnInfoClick(Sender: TObject);
begin
  ShowMessage('opsi-kiosk-client' + LineEnding + 'Display language: ' +
    GetDefaultLang + Lineending + 'Version: ' + ProgramInfo.Version + Lineending +
    'CopyRight: uib gmbh (http://uib.de) under AGPLv3' + LineEnding +
    'http://opsi.org' + Lineending + 'Credits to: Lazarus/FPC,indy,sqllite,superobject');
end;

procedure TFormOpsiClientKiosk.BitBtnStoreActionClick(Sender: TObject);
var
  counter: integer;
  i: integer;
  request: string;
begin
  if TBitBtn(Sender).Caption = rsStoreActions then
  begin
    screen.Cursor := crHourGlass;
    try
      FormProgressWindow.ProcessMess;
      mythread := Tmythread2.Create(False);
      mythread.WaitFor;

      begin
        installdlg.Finstalldlg.Memo1.Text := ockdata.getActionrequests.Text;
        if installdlg.Finstalldlg.Memo1.Text = '' then
          installdlg.Finstalldlg.Memo1.Text := rsNoActionsFound;
        installdlg.Finstalldlg.Show;
      end;

    finally
      mythread.Terminate;
      Screen.Cursor := crDefault;
      FormProgressWindow.ProgressBar1.Style := pbstNormal;
      FormProgressWindow.ProgressBar1.Visible := False;
      FreeAndNil(mythread);
    end;
  end
  else
  begin
    // rsInstallNow
    screen.Cursor := crHourGlass;
    try
      FormProgressWindow.ProcessMess;
      mythread := Tmythread2.Create(False);
      mythread.WaitFor;
      ockdata.firePushInstallation;
    finally
      mythread.Terminate;
      Screen.Cursor := crDefault;
      FormProgressWindow.ProgressBar1.Style := pbstNormal;
      FormProgressWindow.ProgressBar1.Visible := False;
      FreeAndNil(mythread);
    end;
  end;
end;

procedure TFormOpsiClientKiosk.BitBtnToggleViewClick(Sender: TObject);
begin
  if NotebookProducts.PageIndex = 1 then NotebookProducts.PageIndex := 0
    else if NotebookProducts.PageIndex = 0 then NotebookProducts.PageIndex := 1;
  FormOpsiClientKiosk.PanelProductDetail.height := 0;
end;

procedure TFormOpsiClientKiosk.ButtonSoftwareInstallClick(Sender: TObject);
begin
  QuestionDLG('Install Dialog','Install now?',mtConfirmation,[mrYes, 'Install now', mrNo, 'Select for Installation', mrCancel], 0);
end;

{*procedure TFormOpsiClientKiosk.SpeedButtonNotInstalledClick(Sender: TObject);
begin
  FormOpsiClientKiosk.NotebookProducts.PageIndex:= 4;
  EditSearch.Text := '';
  TimerSearchEdit.Enabled := False;
  ockdata.ZMQUerydataset1.Filtered := False;
  ockdata.ZMQUerydataset1.FilterOptions := [foCaseInsensitive];
  ockdata.ZMQUerydataset1.Filter := 'installationStatus = ""';
  ockdata.ZMQUerydataset1.Filtered := True;
  FormProgressWindow.ProcessMess;
  while inTileRebuild do
    Sleep(10);
  if RadioGroupView.ItemIndex = 1 then
    BuildProductTiles(ArrayUpdateProductTiles,'FlowPanelNotInstalledTiles');
end;*}


procedure TFormOpsiClientKiosk.SpeedButtonNotInstalledClick(Sender: TObject);
var
  i : integer;
begin
  FormOpsiClientKiosk.NotebookProducts.PageIndex:= 1;
  i := 0;
  for i := 0 to Length(ArrayAllProductTiles)-1 do
  begin
    if ArrayAllProductTiles[i].LabelState.Caption <> rsNotInstalled then
      ArrayAllProductTiles[i].Visible := False
    else ArrayAllProductTiles[i].Visible := True;
  end;
  TSpeedButton(Sender).Down:= True;
end;
procedure TFormOpsiClientKiosk.SpeedButtonExpertModeClick(Sender: TObject);
begin
  if SpeedButtonExpertMode.Down then
  begin
    // expert mode
    BitBtnStoreAction.Caption := rsStoreActions;
    BitBtnStoreAction.Hint := rsStoreActionsHint;
    // localize RadioGroupView
    RadioGroupView.Items[0] := rsViewList;
    RadioGroupView.Items[1] := rsViewTiles;
    // List view
    RadioGroupView.ItemIndex := 0;
    PanelExpertMode.Visible := True;
    NotebookProducts.PageIndex := RadioGroupView.ItemIndex;
    RadioGroupViewSelectionChanged(self);
  end
  else
  begin
    // standard mode
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
  action := ockdata.ZMQUerydataset1.FieldByName('possibleAction').AsString;
  DBComboBox1.Items.Clear;
  DBComboBox1.Items.Add('none');
  DBComboBox1.Items.Add('setup');
  if action <> 'setup' then
    DBComboBox1.Items.Add(action);
end;

procedure TFormOpsiClientKiosk.DBGrid1ColExit(Sender: TObject);
begin
  if (dbgrid1.SelectedField.FieldName = DBComboBox1.DataField) then
  begin
    DBComboBox1.Visible := False;
  end;
end;

procedure TFormOpsiClientKiosk.DBGrid1DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
  if (gdFocused in State) then
  begin
    if (dbgrid1.SelectedField.FieldName = DBComboBox1.DataField) then
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
  ErrorMsg: string;
  optionlist: TStringList;
  //MyClientID :String;

begin
  if not StartupDone then
  begin
    StartupDone := True;
    Application.ProcessMessages;
    optionlist := TStringList.Create;
    optionlist.Append('fqdn::');
    optionlist.Append('lang:');
    // quick check parameters
    ErrorMsg := Application.CheckOptions('', optionlist);
    if ErrorMsg <> '' then
    begin
      Application.ShowException(Exception.Create(ErrorMsg));
      Application.Terminate;
      Exit;
    end;
    // parse parameters
    if Application.HasOption('fqdn') then
    begin
      MyClientId := Application.GetOptionValue('fqdn');
    end;
    if Application.HasOption('lang') then
    begin
      SetDefaultLang(Application.GetOptionValue('lang'));
    end;
    FormProgressWindow.LabelDataLoadDetail.Caption := '';

    //ockdata.main;
    main;
    Application.ProcessMessages;
    DBGrid1.DataSource := DataModuleOCK.DataSource1;
    DBGrid2.DataSource := DataModuleOCK.DataSource2;
    PanelProductDetail.Height := 0;
    BuildProductTiles(ArrayAllproductTiles, 'FlowPanelAllTiles');
    // log
    LogDatei.log('rsActSetup is: ' + rsActSetup + ' with color: ' +
      tile_radio_setup_color + ' and font size: ' + IntToStr(tile_radio_font_size), LLDebug2);
    LogDatei.log('TitleLabel.Caption: ' + TitleLabel.Caption, LLDebug2);
  end;
end;

procedure TFormOpsiClientKiosk.FormShow(Sender: TObject);
begin
  FormProgressWindow.LabelDataload.Caption := 'Init';
  StartupDone := False;
  TitleLabel.Caption := title_Text;
  TitleLabel.Font.Name := title_Font_Name;
  TitleLabel.Font.Size := title_Font_Size;
  TitleLabel.Font.Color := StringToColor(title_Font_Color);
  TitleLabel.Font.Bold := title_Font_Bold;
  TitleLabel.Font.Italic := title_Font_Italic;
  TitleLabel.Font.Underline := title_Font_Underline;
  FormOpsiClientKiosk.Repaint;
  Application.ProcessMessages;
end;


procedure TFormOpsiClientKiosk.ReloadDataFromServer;
var
  i: integer;
begin
  //ockdata.fetchProductData_by_getKioskProductInfosForClient;
  fetchProductData(ZMQuerydataset1,'getKioskProductInfosForClient');
  StartupDone := False;
  RadioGroupViewSelectionChanged(self);
  StartupDone := True;
end;

procedure TFormOpsiClientKiosk.EditSearchChange(Sender: TObject);
begin
  if EditSearch.Text <> '' then
  begin
    SpeedButtonAll.AllowAllUp:= True;
    SpeedButtonAll.Down:= False;
    SpeedButtonUpdates.Down:= False;
    SpeedButtonNotInstalled.Down:= False;
    NotebookProducts.PageIndex:= 3;
    // search in list
    // if timer running: reset else start (so we wait to finish the input before seach)
    if TimerSearchEdit.Enabled then
    begin
      TimerSearchEdit.Enabled := False;
      logdatei.log('Got while input wait:' + EditSearch.Text, LLDebug2);
    end
    else
      logdatei.log('Got and start input wait:' + EditSearch.Text, LLDebug2);
    // start search via timer
    TimerSearchEdit.Enabled := True;
    //SpeedButton1Click(Sender);
  end;
end;

procedure TFormOpsiClientKiosk.FormCreate(Sender: TObject);

  procedure loadskin(skinpath: string);
  var
    myini: TInifile;
  begin
    if FileExists(skinpath + 'opsiclientkiosk.ini') then
    begin
      preLogfileLogList.Add('loading from: ' + skinpath + 'opsiclientkiosk.ini');
      myini := TIniFile.Create(skinpath + 'opsiclientkiosk.ini');
      //title
      title_Text :=
        myini.ReadString('TitleLabel', 'Text', title_Text);
      preLogfileLogList.Add('title_Text: ' + title_Text);
      title_Font_Name := myini.ReadString('TitleLabel', 'FontName', title_Font_Name);
      title_Font_Size := myini.ReadInteger('TitleLabel', 'FontSize', title_Font_Size);
      title_Font_Color := myini.ReadString('TitleLabel', 'FontColor', title_Font_Color);
      title_Font_Bold := strToBool(myini.ReadString('TitleLabel',
        'FontBold', boolToStr(title_Font_Bold)));
      title_Font_Italic :=
        strToBool(myini.ReadString('TitleLabel', 'FontItalic',boolToStr(title_Font_Italic)));
      title_Font_Underline :=
        strToBool(myini.ReadString('TitleLabel', 'FontUnderline', boolToStr(title_Font_Underline)));
      //tile
      tile_color := myini.ReadString('Tile', 'Color', tile_color);
      tile_Font_Name := myini.ReadString('Tile', 'FontName', tile_Font_Name);
      tile_Font_Size := myini.ReadInteger('Tile', 'FontSize', tile_Font_Size);
      tile_Font_Color := myini.ReadString('Tile', 'FontColor', tile_Font_Color);
      tile_Font_Bold := strToBool(myini.ReadString('Tile', 'FontBold',
        boolToStr(tile_Font_Bold)));
      tile_Font_Italic := strToBool(myini.ReadString('Tile', 'FontItalic',
        boolToStr(tile_Font_Italic)));
      tile_Font_Underline := strToBool(myini.ReadString('Tile',
        'FontUnderline', boolToStr(tile_Font_Underline)));
      tile_width := myini.ReadInteger('Tile', 'Width', tile_width);
      tile_height := myini.ReadInteger('Tile', 'Height', tile_height);
      //TileRadio
      tile_radio_setup_color := myini.ReadString('TileRadio', 'Setup_color',
        tile_radio_setup_color);
      tile_radio_uninstall_color :=
        myini.ReadString('TileRadio', 'Uninstall_color', tile_radio_uninstall_color);
      tile_radio_none_color := myini.ReadString('TileRadio', 'None_color',
        tile_radio_none_color);
      tile_radio_font_size := myini.ReadInteger('TileRadio', 'Fontsize',
        tile_radio_font_size);
      myini.Free;
    end;
  end;

begin
  StringListTileIDs := TStringList.Create;
  NotebookProducts.PageIndex := 1;  //tiles
  FormOpsiClientKiosk.PanelProductDetail.Height := 0;
  detail_visible := False;
  preLogfileLogList := TStringList.Create;
  preLogfileLogList.Add(Application.Name + ' starting at ' + DateTimeToStr(now));

  // Load custom skin
  skinpath := Application.Location + PathDelim + 'opsiclientkioskskin' + PathDelim;
  if FileExistsUTF8(skinpath + 'opsiclientkiosk.png') then
  begin
    ImageHeader.Picture.LoadFromFile(skinpath + 'opsiclientkiosk.png');
  end;
  //title
  title_Text := 'opsi client Kiosk';
  title_Font_Name := 'Arial';
  title_Font_Size := 12;
  title_Font_Color := 'clBlack';
  title_Font_Bold := true;
  title_Font_Italic := false;
  title_Font_Underline := false;
  //tile
  tile_color := 'clCream';
  tile_Font_Name := 'Arial';
  tile_Font_Size := 10;
  tile_Font_Color := 'clBlack';
  tile_Font_Bold := False;
  tile_Font_Italic := False;
  tile_Font_Underline := False;
  tile_width := 140;
  tile_height := 140;
  //TileRadio
  tile_radio_setup_color := 'clGreen';
  tile_radio_uninstall_color := 'clRed';
  tile_radio_none_color := 'clBlack';
  tile_radio_font_size := 10;
  if FileExists(skinpath + 'opsiclientkiosk.ini') then
  begin
    loadskin(skinpath);
  end;

  // skinpath in opsiclientagent custom dir
  skinpath := Application.Location + PathDelim +
    '..\custom\opsiclientkioskskin' + PathDelim;
  if FileExistsUTF8(skinpath + 'opsiclientkiosk.png') then
  begin
    ImageHeader.Picture.LoadFromFile(skinpath + 'opsiclientkiosk.png');
  end;
  if FileExistsUTF8(skinpath + 'opsiclientkiosk.ini') then
  begin
    loadskin(skinpath);
  end;
  GetDefaultLang;
  preLogfileLogList.Add('GetDefaultLang: ' + GetDefaultLang);
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


procedure TFormOpsiClientKiosk.FilterOnSearch;
var
  Filtercond, Filterstr: string;
begin
  try
    ockdata.ZMQUerydataset1.Open;
    if EditSearch.Text = '' then
    begin
      //Filtercond := '"*"';
      ockdata.ZMQUerydataset1.Filtered := False;
      LogDatei.log('Search for: ' + EditSearch.Text + ' Filter off.', LLinfo);
    end
    else
    begin
      Filtercond := '"*' + EditSearch.Text + '*"';
      LogDatei.log('Serach for: ' + EditSearch.Text + ' Filter for: ' +
        Filtercond, LLinfo);
      Filterstr := 'ProductId =' + Filtercond;
      Filterstr := Filterstr + 'or ProductName =' + Filtercond;
      Filterstr := Filterstr + 'or DESCRIPTION =' + Filtercond;
      Filterstr := Filterstr + 'or ADVICE =' + Filtercond;
      Filterstr := Filterstr + 'or INSTALLATIONSTATUS =' + Filtercond;
      ockdata.ZMQuerydataset1.Filter := Filterstr;
      ockdata.ZMQuerydataset1.FilterOptions := [foCaseInsensitive];
      ockdata.ZMQuerydataset1.Filtered := True;
      ZMQuerydataset1.First;
      {******* Hier weiter machen****
         Verknüpfung Database ID mit Panel ID zum Suchen
      *******************************}
      //while not ZMQuerydataset1.EOF do
      //begin
        //ProductID := ZMQueryDataSet1.FieldByName('ProductId').AsString;

      //end;
      BuildProductTiles(ArraySearchPanelTiles, 'FlowPanelSearchTiles');
    end;
  finally
    ockdata.ZMQUerydataset1.Close;
  end;
  //if not SpeedButtonExpertMode.Down then
end;

procedure TFormOpsiClientKiosk.SpeedButtonAllClick(Sender: TObject);
var
  i : integer;
begin
  FormOpsiClientKiosk.NotebookProducts.PageIndex:= 1;
  i := 0;
  for i := 0 to Length(ArrayAllProductTiles)-1 do
  begin
    ArrayAllProductTiles[i].Visible := True;
  end;
  TSpeedButton(Sender).Down:= True;
end;


procedure TFormOpsiClientKiosk.SpeedButtonViewListClick(Sender: TObject);
begin
  NotebookProducts.PageIndex := 0;
end;

procedure TFormOpsiClientKiosk.SpeedButtonViewStoreClick(Sender: TObject);
begin
  NotebookProducts.PageIndex := 1;
end;

procedure TFormOpsiClientKiosk.TimerSearchEditTimer(Sender: TObject);
begin
  TimerSearchEdit.Enabled := False;
  //while inTileRebuild do
    //Sleep(10);
  FilterOnSearch;
end;


procedure TFormOpsiClientKiosk.BitBtnCancelClick(Sender: TObject);
var
  counter, i: integer;
begin
  if opsidata <> nil then
  begin
    // do not send log
    //opsidata.finishOpsiConf;
    opsidata.Free;
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
function GetUserName_: string;
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


function initLogging(const clientname: string): boolean;
var
  i : integer;
begin
  Result := True;
  LogDatei := TLogInfo.Create;
  logfilename := 'kiosk-' + GetUserName_ +'.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile:= False;
  LogDatei.WriteHistFile:= False;
  logdatei.CreateTheLogfile(logfilename, False);
  logdatei.LogLevel := myloglevel;
  for i := 0 to preLogfileLogList.Count-1 do
    logdatei.log(preLogfileLogList.Strings[i], LLessential);
  preLogfileLogList.Free;
  logdatei.log('opsi-client-kiosk: version: ' + ProgramInfo.Version, LLessential);
end;

procedure main;
var
  ErrorMsg: string;
  parameters: array of string;
  resultstring: string;
  i: integer;
  grouplist: TStringList;
  ConnectionInfo:string;
begin
  FormProgressWindow.LabelInfo.Caption := 'Please wait while connecting to service';
  FormProgressWindow.LabelDataload.Caption := 'Connect opsi Web Service';
  FormProgressWindow.ProcessMess;
  myexitcode := 0;
  myerror := '';
  if opsiclientdmode then readconf2
    else readconf;
  // opsiconfd mode
  //readconf;
  // opsiclientd mode
  //readconf2;
  // do not forget to check firePushInstallation
  initlogging(myclientid);
  LogDatei.log('clientid=' + myclientid, LLNotice);
  LogDatei.log('service_url=' + myservice_url, LLNotice);
  LogDatei.log('service_user=' + myclientid, LLNotice);
  logdatei.AddToConfidentials(myhostkey);
  LogDatei.log('host_key=' + myhostkey, LLdebug3);
  // is an other instance running ?
  if numberOfProcessInstances(ExtractFileName(ParamStr(0))) > 1 then
  begin
    LogDatei.log('An other instance of this program is running - so we abort', LLCritical);
    LogDatei.Close;
    halt(1);
  end;
  // is opsiclientd running ?
  if numberOfProcessInstances('opsiclientd') < 1 then
  begin
    LogDatei.log('opsiclientd is not running - so we abort', LLCritical);
    LogDatei.Close;
    ShowMessage('opsiclientd is not running - so we abort');
    halt(1);
  end;
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;

  if initConnection(30, ConnectionInfo) then
  begin
    LogDatei.log('init Connection done', LLNotice);
    FormOpsiClientKiosk.StatusBar1.Panels[0].Text := ConnectionInfo;
    initdb;
    FormProgressWindow.LabelInfo.Caption := 'Please wait while gettting products';
    LogDatei.log('start fetchProductData_by_getKioskProductInfosForClient', LLNotice);
    //fetchProductData_by_getKioskProductInfosForClient;
    fetchProductData(ZMQuerydataset1,'getKioskProductInfosForClient');
    LogDatei.log('Handle products done', LLNotice);
    FormProgressWindow.LabelDataload.Caption := 'Handle Products';
    FormProgressWindow.ProcessMess;
  end
  else
  begin
    LogDatei.log('init Connection failed - Aborting', LLError);
    if opsidata <> nil then
      opsidata.Free;
    FormOpsiClientKiosk.Terminate;
    halt(1);
  end;
  LogDatei.log('main done', LLDebug2);
  FormProgressWindow.ProgressBar1.StepIt;
  FormProgressWindow.ProcessMess;
end;



end.
