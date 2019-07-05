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
  Classes, SysUtils, DB,
  ExtendedNotebook, DividerBevel,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, Grids, DBGrids, DBCtrls, ockdata, CommCtrl,
  BufDataset, typinfo, installdlg, lcltranslator, ActnList, oslog, inifiles,
  Variants,
  Lazfileutils, Types;

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
    procedure ProductTileClick(Sender: TObject);
    procedure ProductTileChildClick(Sender: TObject);
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


  { TFopsiClientKiosk }

  TFopsiClientKiosk = class(TForm)
    BitBtn1: TBitBtn;
    BitBtnInfo: TBitBtn;
    BitBtnShowAction: TBitBtn;
    BitBtnCancel: TBitBtn;
    BitBtnStoreAction: TBitBtn;
    CheckBox1: TCheckBox;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBComboBox1: TDBComboBox;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBMemoAdvice: TDBMemo;
    DBMemoDescription: TDBMemo;
    DBTextPriority: TDBText;
    DBTextVerStr: TDBText;
    DBTextClientVerStr: TDBText;
    DividerBevel1: TDividerBevel;
    ExtendedNotebook1: TExtendedNotebook;
    FlowPanelTiles: TFlowPanel;
    Image2: TImage;
    LabelWait: TLabel;
    LabelPriority: TLabel;
    LabelVerstr: TLabel;
    LabelDescription: TLabel;
    LabelDataload: TLabel;
    LabelDataLoadDetail: TLabel;
    LabelAdvice: TLabel;
    LabelClientVerstr: TLabel;
    PanelSearchEdit: TPanel;
    Panelsearch: TPanel;
    PanelDependencies: TPanel;
    PanelPriority: TPanel;
    PanelDetailsDBText: TPanel;
    PanelDetailsLables: TPanel;
    PanelProgrssbar: TPanel;
    PanelProgressLabel: TPanel;
    PanelProgess: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBarDetail: TProgressBar;
    RadioGroupView: TRadioGroup;
    ScrollBox1: TScrollBox;
    searchEdit: TEdit;
    ImageHeader: TImage;
    datapanel: TPanel;
    BtnUpgrade: TSpeedButton;
    BtnClearSearchEdit: TSpeedButton;
    SpeedButtonReload: TSpeedButton;
    SpeedButtonAll: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TimerSearchEdit: TTimer;
    TitleLabel: TLabel;
    NotebookProducts: TNotebook;
    PageList: TPage;
    PageTile: TPage;
    PanelToolbar: TPanel;
    productlistpanel: TPanel;
    productdetailpanel: TPanel;
    PanelTopImage: TPanel;
    StatusBar1: TStatusBar;
    procedure BitBtnInfoClick(Sender: TObject);
    procedure BitBtnShowActionClick(Sender: TObject);
    procedure BitBtnStoreActionClick(Sender: TObject);
    //procedure BtnActionClick(Sender: TObject);
    procedure BtnUpgradeClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure datapanelMouseEnter(Sender: TObject);
    procedure DBComboBox1Exit(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure DBGrid1ColExit(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1Enter(Sender: TObject);
    procedure DBGrid1Exit(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure FormDestroy(Sender: TObject);
    procedure grouplistEnter(Sender: TObject);
    procedure ProcessMess;
    procedure RadioGroupViewSelectionChanged(Sender: TObject);
    procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure BtnClearSearchEditClick(Sender: TObject);
    procedure SpeedButtonReloadClick(Sender: TObject);
    procedure Terminate;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    //procedure grouplistSelectionChange(Sender: TObject; User: boolean);
    procedure reloadDataFromServer;
    procedure searchEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure RadioGroup1Click(Sender: TObject);
    procedure searchEditEnter(Sender: TObject);
    procedure SpeedButtonAllClick(Sender: TObject);
    procedure SpeedButtonViewListClick(Sender: TObject);
    procedure SpeedButtonViewStoreClick(Sender: TObject);
    procedure TimerSearchEditTimer(Sender: TObject);
    procedure FilterOnSearch;
  private
    { private declarations }
  public
    { public declarations }
  end;

  Tmythread2 = class(TThread)
  public
    procedure Execute; override;
  end;

var
  FopsiClientKiosk: TFopsiClientKiosk;
  StartupDone: boolean;
  ProductTilesArray: TPanels;
  inTileRebuild: boolean = False;
  lastOrderDirAsc: boolean = True;
  lastOrderCol: string;
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
  clNotInstalled = $000080FF;
  clUnknown = $00FF8000;

  //Path to programm icons
  IconPathCustom = '.'+PathDelim + 'progam_icons' + PathDelim + 'custom' + PathDelim;//Application.Location + PathDelim + 'progam_icons' + PathDelim + 'custom' + PathDelim;
  IconPathDefault = '.'+PathDelim + 'progam_icons' + PathDelim + 'default' + PathDelim;

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


function actionRequestToLocale(actionRequest: string): string;
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

function localeToActionRequest(localestr: string): string;
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
    self.OnClick := ProductTileClick;
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
       OnClick := ProductTileChildClick;
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
      OnClick := ProductTileChildClick;
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
      OnClick := ProductTileChildClick;
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
      OnClick := ProductTileChildClick;
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
      //BorderSpacing.Around := 3;
      OnClick := ProductTileChildClick;
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
      //Button1.Destroy;
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
  oldpos := FopsiClientKiosk.ScrollBox1.VertScrollBar.Position;
  FopsiClientKiosk.ScrollBox1.VertScrollBar.Position :=
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
    ProductTilesArray[tileindex].ShapeRoundSquare.Brush.Color:=$00FEEFD3;
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
    ProductTilesArray[tileindex].ShapeRoundSquare.Brush.Color:=clWhite;
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
    //actionindex := ProductTilesArray[tileindex].RadioGroupAction.ItemIndex;
    actionindex := TRadioButton(Sender).Tag;
    pid := ProductTilesArray[tileindex].LabelId.Caption;
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
      FopsiClientKiosk.productdetailpanel.Height := 185;
      ZMQueryDataSet1.Post;
    end;
  end;
end;

procedure TProductPanel.ProductTileClick(Sender: TObject);
var
  pid: string;
  tileindex: integer;
begin
  if not inTileRebuild then
  begin
    tileindex := TProductPanel(Sender).Tag;
    pid := ProductTilesArray[tileindex].LabelId.Caption;
    ZMQueryDataSet1.First;
    if ZMQueryDataSet1.Locate('ProductId', VarArrayOf([pid]),
      [loCaseInsensitive]) then
    begin
      if detail_visible then
      begin
        FopsiClientKiosk.productdetailpanel.Height := 0;
        detail_visible := False;
      end
      else
      begin
        FopsiClientKiosk.productdetailpanel.Height := 185;
        detail_visible := True;
      end;
    end;
  end;
end;


procedure TProductPanel.ProductTileChildClick(Sender: TObject);
var
  pid: string;
  tileindex: integer;
begin
  if not inTileRebuild then
  begin
    tileindex := TControl(Sender).Parent.Tag; //type convertion to TControl (all visual components are of this type), parent is the Panel
    //if tileindex <> former_slected_tile
    pid := ProductTilesArray[tileindex].LabelID.Caption;
    ZMQueryDataSet1.First;
    if ZMQueryDataSet1.Locate('ProductId', VarArrayOf([pid]),[loCaseInsensitive]) then
    begin
      if detail_visible then
      begin
        FopsiClientKiosk.productdetailpanel.Height := 0;
        ProductTilesArray[tileindex].ShapeRoundSquare.Pen.Color:=clBlack;
        ProductTilesArray[former_selected_tile].ShapeRoundSquare.Pen.Color:=clBlack;
        ProductTilesArray[former_selected_tile].ShapeRoundSquare.Brush.Color:=clWhite;
        detail_visible := False;
      end
      else
      begin
        FopsiClientKiosk.productdetailpanel.Height := 185;
        ProductTilesArray[former_selected_tile].ShapeRoundSquare.Pen.Color:=clBlack;
        ProductTilesArray[tileindex].ShapeRoundSquare.Pen.Color:=clBlue;
        ProductTilesArray[tileindex].ShapeRoundSquare.Brush.Color:=$00FEEFD3;
        detail_visible := True;
        former_selected_tile := tileindex;
      end;
    end;
  end;
end;

procedure rebuildProductTiles;
var
  counter, i, index: integer;
  action, state, ProductID: string;
begin
  try
    inTileRebuild := True;
    logdatei.log('rebuildProductTiles start', LLDebug2);
    FopsiClientKiosk.ProgressBarDetail.Visible := True;
    FopsiClientKiosk.ProgressBar1.Visible := True;
    FopsiClientKiosk.LabelDataLoadDetail.Visible := True;
    FopsiClientKiosk.LabelDataLoad.Visible := True;
    FopsiClientKiosk.ProgressbarDetail.Position := 0;
    FopsiClientKiosk.LabelDataload.Caption := 'Clear Tiles';
    Application.ProcessMessages;
    try
      counter := length(ProductTilesArray);
      if counter > 0 then
        //for i:=0 to counter -1 do ProductTilesArray[i].Destroy;
        for i := 0 to counter - 1 do
        begin
          //FreeAndNil(ProductTilesArray[i]);
          ProductTilesArray[i].Visible := False;
          ProductTilesArray[i] := nil;
          ProductTilesArray[i].Free;
          //logdatei.log('FreeAndNil(ProductTilesArray', LLDebug2);
        end;
      SetLength(ProductTilesArray, 0);
    except
      on e: Exception do
      begin
        logdatei.log('Exception rebuildProductTiles: ProductTilesArray[i].Free;',
          LLError);
        logdatei.log('Exception: ' + E.message, LLError);
        logdatei.log('Exception handled at: ' + getCallAddrStr, LLError);
        logdatei.log_exception(E, LLError);
      end;
    end;

    // use complete window for tiles
    FopsiClientKiosk.productdetailpanel.Height := 0;

    // progess
    FopsiClientKiosk.Progressbar1.Position := 80;
    FopsiClientKiosk.LabelDataload.Caption := 'Fill Tiles';
    FopsiClientKiosk.ProgressbarDetail.max := ZMQUerydataset1.RecordCount;
    Application.ProcessMessages;
    logdatei.log('rebuildProductTiles from db start', LLDebug2);
    counter := 0;
    ZMQUerydataset1.First;
    while not ZMQuerydataset1.EOF do
    begin
      ProductID := ZMQueryDataSet1.FieldByName('ProductId').AsString;
      FopsiClientKiosk.LabelDataLoadDetail.Caption := ProductID;
      Application.ProcessMessages;
      SetLength(ProductTilesArray, counter + 1);
      ProductTilesArray[counter] := TProductPanel.Create(FopsiClientKiosk.FlowPanelTiles);
      ProductTilesArray[counter].LabelId.Caption := ProductID;
      ProductTilesArray[counter].LabelName.Caption := ZMQueryDataSet1.FieldByName('ProductName').AsString;
      if FileExists(IconPathCustom + ProductID + '.png') then
        ProductTilesArray[counter].ImageIcon.Picture.LoadFromFile(IconPathCustom + ProductID + '.png')
      else
        if FileExists(IconPathDefault + ProductID + '.png') then
          ProductTilesArray[counter].ImageIcon.Picture.LoadFromFile(IconPathDefault + ProductID + '.png')
        else
          if FileExists(IconPathDefault + 'opsi-logo.png') then
            ProductTilesArray[counter].ImageIcon.Picture.LoadFromFile(IconPathDefault + 'opsi-logo.png');
      state := ZMQueryDataSet1.FieldByName('installationStatus').AsString;
      if state = 'installed' then
      begin
        ProductTilesArray[counter].LabelState.Caption := rsInstalled;
        ProductTilesArray[counter].LabelState.Color := clInstalled;
      end
      else if (state = 'not_installed') or (state = 'not installed') or (state = '') then
      begin
        ProductTilesArray[counter].LabelState.Caption := rsNotInstalled;
        ProductTilesArray[counter].LabelState.Color := clNotInstalled;
      end
      else if state = 'unknown' then
      begin
        ProductTilesArray[counter].LabelState.Caption := rsStateUnknown;
        ProductTilesArray[counter].LabelState.Color := clUnknown;
      end;
      //radio group
      {*
      ProductTilesArray[counter].rbsetup.Enabled := True;
      ProductTilesArray[counter].lbsetup.Enabled := True;
      action := Trim(ockdata.ZMQUerydataset1.FieldByName('possibleAction').AsString);
      if (action = 'uninstall') then
      begin
        ProductTilesArray[counter].rbuninstall.Enabled := True;
        ProductTilesArray[counter].lbuninstall.Enabled := True;
      end;
      action := Trim(ockdata.ZMQUerydataset1.FieldByName('actionrequest').AsString);
      if (action = 'none') or (action = '') then
        ProductTilesArray[counter].rbNone.Checked := True
      else if action = 'setup' then
        ProductTilesArray[counter].rbsetup.Checked := True
      else if action = 'uninstall' then
        ProductTilesArray[counter].rbuninstall.Checked := True;
      *}
      ProductTilesArray[counter].Tag := counter;
      Inc(counter);
      FopsiClientKiosk.ProgressbarDetail.Position := counter;
      Application.ProcessMessages;
      ZMQUerydataset1.Next;
    end;
  finally
    inTileRebuild := False;
    logdatei.log('rebuildProductTiles stop', LLDebug2);
    FopsiClientKiosk.ProgressBar1.Position := 66;
    FopsiClientKiosk.ProgressBarDetail.Visible := False;
    FopsiClientKiosk.LabelDataLoadDetail.Visible := False;
    FopsiClientKiosk.ProgressBar1.Visible := False;
    FopsiClientKiosk.LabelDataLoad.Visible := False;
    FopsiClientKiosk.CheckBox1Change(FopsiClientKiosk);
  end;
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


{ TFopsiClientKiosk }

procedure TFopsiClientKiosk.ProcessMess;
begin
  Application.ProcessMessages;
end;

procedure TFopsiClientKiosk.RadioGroupViewSelectionChanged(Sender: TObject);
begin
  NotebookProducts.PageIndex := RadioGroupView.ItemIndex;
  if StartupDone then
    if RadioGroupView.ItemIndex = 1 then
      rebuildProductTiles;
end;

procedure TFopsiClientKiosk.ScrollBox1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  logdatei.log('ScrollBox1MouseWheel WheelDelta: ' + IntToStr(WheelDelta), LLDebug2);
end;

procedure TFopsiClientKiosk.BtnClearSearchEditClick(Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    if searchedit.Text = '' then
      FilterOnSearch
    else
      searchedit.Text := '';
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFopsiClientKiosk.SpeedButtonReloadClick(Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    reloadDataFromServer;
    FilterOnSearch;
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFopsiClientKiosk.Terminate;
begin
  Application.Terminate;
end;

procedure TFopsiClientKiosk.DBGrid1TitleClick(Column: TColumn);
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
    ZMQUerydataset1.SQL.Add('select * from kioskmaster order by ' +
      Column.FieldName + direction);
    ZMQUerydataset1.Open;
  except
  end;
end;

procedure TFopsiClientKiosk.FormDestroy(Sender: TObject);
begin
  LogDatei.log('FopsiClientKiosk.FormDestroy: Application terminates', LLInfo);
end;

procedure TFopsiClientKiosk.grouplistEnter(Sender: TObject);
begin
  productdetailpanel.Height := 0;
end;

procedure TFopsiClientKiosk.DBGrid1Exit(Sender: TObject);
begin
  //productdetailpanel.Height := 0;
end;

procedure TFopsiClientKiosk.DBGrid1Enter(Sender: TObject);
begin
  //productdetailpanel.Height := 0;
  //productdetailpanel.Height:=185;
end;

procedure TFopsiClientKiosk.BtnUpgradeClick(Sender: TObject);
begin
  searchEdit.Text := '';
  TimerSearchEdit.Enabled := False;
  ockdata.ZMQUerydataset1.Filtered := False;
  ockdata.ZMQUerydataset1.FilterOptions := [foCaseInsensitive];
  ockdata.ZMQUerydataset1.Filter := 'updatePossible = "True"';
  ockdata.ZMQUerydataset1.Filtered := True;
  ProcessMess;
  while inTileRebuild do
    Sleep(10);
  if RadioGroupView.ItemIndex = 1 then
    rebuildProductTiles;
end;

procedure TFopsiClientKiosk.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    // expert mode
    RadioGroupView.Visible := True;
    BitBtnInfo.Visible := True;
    SpeedButtonReload.Visible := True;
    BtnUpgrade.Visible := True;
    SpeedButtonAll.Visible := True;
    BitBtnShowAction.Visible := True;
    BitBtnStoreAction.Caption := rsStoreActions;
    BitBtnStoreAction.Hint := rsStoreActionsHint;
  end
  else
  begin
    // standard mode
    RadioGroupView.Visible := False;
    BitBtnInfo.Visible := False;
    SpeedButtonReload.Visible := False;
    BtnUpgrade.Visible := False;
    SpeedButtonAll.Visible := False;
    BitBtnShowAction.Visible := False;
    BitBtnStoreAction.Caption := rsInstallNow;
    BitBtnStoreAction.Hint := rsInstallNowHint;
  end;
  // localize RadioGroupView
  RadioGroupView.Items[0] := rsViewList;
  RadioGroupView.Items[1] := rsViewTiles;
  repaint;
  Application.ProcessMessages;
end;

procedure TFopsiClientKiosk.datapanelMouseEnter(Sender: TObject);
begin

end;

procedure TFopsiClientKiosk.DBComboBox1Exit(Sender: TObject);
begin
  DBGrid1.Repaint;
  if DBComboBox1.Text <> '' then
    ockdata.ZMQueryDataSet1.Post;
  //  ockdata.ZMQueryDataSet1.FieldByName('actionrequest').AsString := 'none';
  //ockdata.ZMQueryDataSet1.Post;
end;


procedure TFopsiClientKiosk.BitBtnShowActionClick(Sender: TObject);
var
  i: integer;
begin
  // write back action requests
  ockdata.ZMQUerydataset1.Filtered := False;
  //ockdata.ZMQUerydataset1.Filter := ' not ((ActionRequest = "") and (ActionRequest = "none"))';
  ockdata.ZMQUerydataset1.Filter := 'ActionRequest <> ""';
  ockdata.ZMQUerydataset1.Filtered := True;
  ProcessMess;
  RadioGroupViewSelectionChanged(self);
end;

procedure TFopsiClientKiosk.BitBtnInfoClick(Sender: TObject);
begin
  ShowMessage('opsi-kiosk-client' + LineEnding + 'Display language: ' +
    GetDefaultLang + Lineending + 'Version: ' + myVersion + Lineending +
    'CopyRight: uib gmbh (http://uib.de) under AGPLv3' + LineEnding +
    'http://opsi.org' + Lineending + 'Credits to: Lazarus/FPC,indy,sqllite,superobject');
end;

procedure TFopsiClientKiosk.BitBtnStoreActionClick(Sender: TObject);
var
  counter: integer;
  i: integer;
  request: string;
begin
  if TBitBtn(Sender).Caption = rsStoreActions then
  begin
    screen.Cursor := crHourGlass;
    try
      ProcessMess;
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
      ProgressBar1.Style := pbstNormal;
      ProgressBar1.Visible := False;
      FreeAndNil(mythread);
    end;
  end
  else
  begin
    // rsInstallNow
    screen.Cursor := crHourGlass;
    try
      ProcessMess;
      mythread := Tmythread2.Create(False);
      mythread.WaitFor;
      ockdata.firePushInstallation;
    finally
      mythread.Terminate;
      Screen.Cursor := crDefault;
      ProgressBar1.Style := pbstNormal;
      ProgressBar1.Visible := False;
      FreeAndNil(mythread);
    end;
  end;
end;

procedure TFopsiClientKiosk.DBGrid1CellClick(Column: TColumn);
var
  action: string;
begin
  productdetailpanel.Height := 185;
  action := ockdata.ZMQUerydataset1.FieldByName('possibleAction').AsString;
  DBComboBox1.Items.Clear;
  DBComboBox1.Items.Add('none');
  DBComboBox1.Items.Add('setup');
  if action <> 'setup' then
    DBComboBox1.Items.Add(action);
end;

procedure TFopsiClientKiosk.DBGrid1ColExit(Sender: TObject);
begin
  if (dbgrid1.SelectedField.FieldName = DBComboBox1.DataField) then
  begin
    DBComboBox1.Visible := False;
  end;
end;

procedure TFopsiClientKiosk.DBGrid1DrawColumnCell(Sender: TObject;
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

procedure TFopsiClientKiosk.FormActivate(Sender: TObject);
var
  ErrorMsg: string;
  optionlist: TStringList;

begin

  if not StartupDone then
  begin

    RadioGroupViewSelectionChanged(self);
    CheckBox1Change(self);
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
      myClientId := Application.GetOptionValue('fqdn');
    end;

    if Application.HasOption('lang') then
    begin
      SetDefaultLang(Application.GetOptionValue('lang'));
    end;

    ockdata.main;
    Progressbar1.Position := Progressbar1.Max;
    ProgressbarDetail.Position := ProgressbarDetail.Max;
    LabelDataload.Caption := '';
    LabelDataLoadDetail.Caption := '';
    ProgressBar1.Visible := False;
    ProgressBarDetail.Visible := False;
    LabelWait.Visible := False;
    //grouplist.Enabled := True;
    RadioGroupViewSelectionChanged(self);

    // log
    LogDatei.log('rsActSetup is: ' + rsActSetup + ' with color: ' +
      tile_radio_setup_color + ' and font size: ' + IntToStr(tile_radio_font_size), LLDebug2);
    LogDatei.log('TitleLabel.Caption: ' + TitleLabel.Caption, LLDebug2);

  end;
end;

procedure TFopsiClientKiosk.FormShow(Sender: TObject);
begin
  LabelDataload.Caption := 'Init';
  StartupDone := False;
  TitleLabel.Caption := title_Text;
  TitleLabel.Font.Name := title_Font_Name;
  TitleLabel.Font.Size := title_Font_Size;
  TitleLabel.Font.Color := StringToColor(title_Font_Color);
  TitleLabel.Font.Bold := title_Font_Bold;
  TitleLabel.Font.Italic := title_Font_Italic;
  TitleLabel.Font.Underline := title_Font_Underline;
  FopsiClientKiosk.Repaint;
  Application.ProcessMessages;
end;


procedure TFopsiClientKiosk.reloadDataFromServer;
var
  i: integer;
begin
  ockdata.fetchProductData_by_getKioskProductInfosForClient;
  StartupDone := False;
  RadioGroupViewSelectionChanged(self);
  StartupDone := True;
end;

procedure TFopsiClientKiosk.searchEditChange(Sender: TObject);
begin
  // search in list
  // if timer running: reset else start (so we wait to finish the input before seach)
  if TimerSearchEdit.Enabled then
  begin
    TimerSearchEdit.Enabled := False;
    logdatei.log('Got while input wait:' + searchEdit.Text, LLDebug2);
  end
  else
    logdatei.log('Got and start input wait:' + searchEdit.Text, LLDebug2);
  // start search via timer
  TimerSearchEdit.Enabled := True;
  //SpeedButton1Click(Sender);
end;

procedure TFopsiClientKiosk.FormCreate(Sender: TObject);

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
  NotebookProducts.PageIndex := 1;  //tiles
  FopsiClientKiosk.productdetailpanel.Height := 0;
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

procedure TFopsiClientKiosk.searchEditEnter(Sender: TObject);
begin
  productdetailpanel.Height := 0;
end;


procedure TFopsiClientKiosk.FilterOnSearch;
var
  Filtercond, Filterstr: string;
begin
  if searchedit.Text = '' then
  begin
    Filtercond := '"*"';
    ockdata.ZMQUerydataset1.Filtered := False;
    LogDatei.log('Search for: ' + searchedit.Text + ' Filter off.', LLinfo);
  end
  else
  begin
    Filtercond := '"*' + searchedit.Text + '*"';
    LogDatei.log('Serach for: ' + searchedit.Text + ' Filter for: ' +
      Filtercond, LLinfo);
    Filterstr := 'ProductId =' + Filtercond;
    Filterstr := Filterstr + 'or ProductName =' + Filtercond;
    Filterstr := Filterstr + 'or DESCRIPTION =' + Filtercond;
    Filterstr := Filterstr + 'or ADVICE =' + Filtercond;
    Filterstr := Filterstr + 'or INSTALLATIONSTATUS =' + Filtercond;
    ockdata.ZMQUerydataset1.Filter := Filterstr;
    ockdata.ZMQUerydataset1.FilterOptions := [foCaseInsensitive];
    ockdata.ZMQUerydataset1.Filtered := True;
  end;
  if RadioGroupView.ItemIndex = 1 then
    rebuildProductTiles;
end;

procedure TFopsiClientKiosk.SpeedButtonAllClick(Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    if searchedit.Text = '' then
      FilterOnSearch
    else
      searchedit.Text := '';
    // this shoud call  searchEditChange
    // so we do nothing else here
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFopsiClientKiosk.SpeedButtonViewListClick(Sender: TObject);
begin
  NotebookProducts.PageIndex := 0;
end;

procedure TFopsiClientKiosk.SpeedButtonViewStoreClick(Sender: TObject);
begin
  NotebookProducts.PageIndex := 1;
end;

procedure TFopsiClientKiosk.TimerSearchEditTimer(Sender: TObject);
begin
  TimerSearchEdit.Enabled := False;
  while inTileRebuild do
    Sleep(10);
  FilterOnSearch;
end;


procedure TFopsiClientKiosk.BitBtnCancelClick(Sender: TObject);
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
    counter := length(ProductTilesArray);
    if counter > 0 then
      //for i:=0 to counter -1 do ProductTilesArray[i].Destroy;
      for i := 0 to counter - 1 do
      begin
        ProductTilesArray[i] := nil;
        ProductTilesArray[i].Free;
        logdatei.log('FreeAndNil(ProductTilesArray', LLDebug2);
      end;
    SetLength(ProductTilesArray, 0);

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

end.
