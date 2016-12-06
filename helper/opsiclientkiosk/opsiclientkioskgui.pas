// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 494 $
// $Author: oertel $
// $Date: 2016-08-31 18:26:25 +0200 (Mi, 31 Aug 2016) $
//***************************************************************************


unit opsiclientkioskgui;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DB, FileUtil, ExtendedNotebook, ZMConnection,
  ZMQueryDataSet, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, Grids, DBGrids, DBCtrls, ockdata, CommCtrl,
  BufDataset, typinfo, installdlg,lcltranslator,oslog,inifiles;

type

  { TFopsiClientKiosk }

  TFopsiClientKiosk = class(TForm)
    BitBtnShowAction: TBitBtn;
    BitBtnCancel: TBitBtn;
    BitBtnInfo: TBitBtn;
    BitBtnStoreAction: TBitBtn;
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
    ExtendedNotebook1: TExtendedNotebook;
    Image2: TImage;
    LabelWait: TLabel;
    LabelPriority: TLabel;
    LabelVerstr: TLabel;
    LabelDescription: TLabel;
    LabelDataload: TLabel;
    LabelDataLoadDetail: TLabel;
    LabelAdvice: TLabel;
    LabelClientVerstr: TLabel;
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
    searchEdit: TEdit;
    Image1: TImage;
    ImageHeader: TImage;
    datapanel: TPanel;
    SpeedButton1: TSpeedButton;
    BtnUpgrade: TSpeedButton;
    SpeedButtonReload: TSpeedButton;
    SpeedButtonAll: TSpeedButton;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TitleLabel: TLabel;
    grouplist: TListBox;
    Notebook1: TNotebook;
    PageList: TPage;
    Page2: TPage;
    PanelToolbar: TPanel;
    grouppanel: TPanel;
    productlistpanel: TPanel;
    productdetailpanel: TPanel;
    logopanel: TPanel;
    PanelTopImage: TPanel;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    procedure BitBtnInfoClick(Sender: TObject);
    procedure BitBtnShowActionClick(Sender: TObject);
    procedure BitBtnStoreActionClick(Sender: TObject);
    //procedure BtnActionClick(Sender: TObject);
    procedure BtnUpgradeClick(Sender: TObject);
    procedure DBComboBox1Exit(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure DBGrid1ColExit(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1Enter(Sender: TObject);
    procedure DBGrid1Exit(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure grouplistEnter(Sender: TObject);
    procedure PageListBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: integer);
    procedure ProcessMess;
    procedure SpeedButtonReloadClick(Sender: TObject);
    procedure Terminate;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure CheckListBox1ShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure ExtendedNotebook1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grouplistSelectionChange(Sender: TObject; User: boolean);
    procedure searchEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure RadioGroup1Click(Sender: TObject);
    procedure searchEditEnter(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButtonAllClick(Sender: TObject);
    procedure SpeedButtonViewListClick(Sender: TObject);
    procedure SpeedButtonViewStoreClick(Sender: TObject);
    procedure ZMQueryDataSet1NewRecord(DataSet: TDataSet);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FopsiClientKiosk: TFopsiClientKiosk;
  StartupDone: boolean;

resourcestring
  rsNoActionsFound = 'No action requests found.';


implementation

{$R *.lfm}

//http://wiki.freepascal.org/How_to_write_in-memory_database_applications_in_Lazarus/FPC#Sorting_DBGrid_on_TitleClick_event_for_TBufDataSet
function SortBufDataSet(DataSet: TBufDataSet; const FieldName: string): boolean;
var
  i: integer;
  IndexDefs: TIndexDefs;
  IndexName: string;
  IndexOptions: TIndexOptions;
  Field: TField;
begin
  Result := False;
  Field := DataSet.Fields.FindField(FieldName);
  //If invalid field name, exit.
  if Field = nil then
    Exit;
  //if invalid field type, exit.
  if {(Field is TObjectField) or} (Field is TBlobField) or
    {(Field is TAggregateField) or} (Field is TVariantField) or
    (Field is TBinaryField) then
    Exit;
  //Get IndexDefs and IndexName using RTTI
  if IsPublishedProp(DataSet, 'IndexDefs') then
    IndexDefs := GetObjectProp(DataSet, 'IndexDefs') as TIndexDefs
  else
    Exit;
  if IsPublishedProp(DataSet, 'IndexName') then
    IndexName := GetStrProp(DataSet, 'IndexName')
  else
    Exit;
  //Ensure IndexDefs is up-to-date
  IndexDefs.Updated := False;
  {<<<<---This line is critical as IndexDefs.Update will do nothing on the next sort if it's already true}
  IndexDefs.Update;
  //If an ascending index is already in use,
  //switch to a descending index
  if IndexName = FieldName + '__IdxA' then
  begin
    IndexName := FieldName + '__IdxD';
    IndexOptions := [ixDescending];
  end
  else
  begin
    IndexName := FieldName + '__IdxA';
    IndexOptions := [];
  end;
  //Look for existing index
  for i := 0 to Pred(IndexDefs.Count) do
  begin
    if IndexDefs[i].Name = IndexName then
    begin
      Result := True;
      Break;
    end;  //if
  end; // for
  //If existing index not found, create one
  if not Result then
  begin
    if IndexName = FieldName + '__IdxD' then
      DataSet.AddIndex(IndexName, FieldName, IndexOptions, FieldName)
    else
      DataSet.AddIndex(IndexName, FieldName, IndexOptions);
    Result := True;
  end; // if not
  //Set the index
  SetStrProp(DataSet, 'IndexName', IndexName);
end;

{ TFopsiClientKiosk }

procedure TFopsiClientKiosk.ProcessMess;
begin
  Application.ProcessMessages;
end;

procedure TFopsiClientKiosk.SpeedButtonReloadClick(Sender: TObject);
begin
  grouplistSelectionChange(Sender,true);
end;

procedure TFopsiClientKiosk.Terminate;
begin
  Application.Terminate;
end;

procedure TFopsiClientKiosk.DBGrid1TitleClick(Column: TColumn);
begin
  try
    //ockdata.ZMQUerydataset1.SortDataset(Column.FieldName);
    SortBufDataSet(TBufDataset(ockdata.ZMQUerydataset1), Column.FieldName);

  except
  end;
end;

procedure TFopsiClientKiosk.grouplistEnter(Sender: TObject);
begin
  productdetailpanel.Height := 0;
end;

procedure TFopsiClientKiosk.PageListBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: integer);
begin

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
  ockdata.ZMQUerydataset1.Filter := 'updatePossible = "True"';
  ockdata.ZMQUerydataset1.Filtered := True;
end;

procedure TFopsiClientKiosk.DBComboBox1Exit(Sender: TObject);
begin
  DBGrid1.Repaint;
  if DBComboBox1.Text <> '' then ockdata.ZMQueryDataSet1.Post;
  //  ockdata.ZMQueryDataSet1.FieldByName('actionrequest').AsString := 'none';
  //ockdata.ZMQueryDataSet1.Post;
end;

(*
procedure TFopsiClientKiosk.BtnActionClick(Sender: TObject);
begin
  ZMQueryDataSet1.Edit;
  ZMQueryDataSet1.FieldByName('actionrequest').AsString := BtnAction.Caption;
  ZMQueryDataSet1.Post;
end;
*)

procedure TFopsiClientKiosk.BitBtnShowActionClick(Sender: TObject);
var
  i: integer;
begin
  // write back action requests
  ockdata.ZMQUerydataset1.Filtered := False;
  ockdata.ZMQUerydataset1.Filter := 'ActionRequest <> ""';
  ockdata.ZMQUerydataset1.Filtered := True;
  ProcessMess;
end;

procedure TFopsiClientKiosk.BitBtnInfoClick(Sender: TObject);
begin
  ShowMessage('opsi-kiosk-client' + LineEnding
            + 'Display language: '+GetDefaultLang+Lineending
            + 'Version: ' + myVersion + Lineending
            + 'CopyRight: uib gmbh (http://uib.de) under AGPLv3' + LineEnding
            + 'http://opsi.org' + Lineending
            + 'Credits to: Lazarus/FPC,indy,zmsql,superobject');
end;

procedure TFopsiClientKiosk.BitBtnStoreActionClick(Sender: TObject);
var
  counter: integer;
  i : integer;
begin
  try
    screen.Cursor := crHourGlass;
    // write back action requests
    ockdata.ZMQUerydataset1.Filtered := False;
    ockdata.ZMQUerydataset1.Filter := 'ActionRequest  <> ""';
    ockdata.ZMQUerydataset1.Filtered := True;
    ProcessMess;
    counter := 0;
    ZMQUerydataset1.First;
    while not ZMQUerydataset1.EOF do
    begin
      ockdata.setActionrequest(ZMQUerydataset1.FieldByName('ProductId').AsString,
        ZMQUerydataset1.FieldByName('ActionRequest').AsString);
      ZMQUerydataset1.Next;
      Inc(counter);
    end;
    //if counter > 0 then
    begin
      installdlg.Finstalldlg.Memo1.Text := ockdata.getActionrequests.Text;
      if installdlg.Finstalldlg.Memo1.Text = '' then
        installdlg.Finstalldlg.Memo1.Text := rsNoActionsFound;
      installdlg.Finstalldlg.Show;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFopsiClientKiosk.DBGrid1CellClick(Column: TColumn);
var
  action: string;
begin
  (*
  BtnAction.Caption := ockdata.ZMQUerydataset1.FieldByName('possibleAction').AsString;
  if BtnAction.Caption = '' then
    BtnAction.Enabled := False
  else
    BtnAction.Enabled := True;
    *)
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

procedure TFopsiClientKiosk.CheckListBox1ShowHint(Sender: TObject; HintInfo: PHintInfo);
begin

end;

procedure TFopsiClientKiosk.ExtendedNotebook1Change(Sender: TObject);
begin

end;

procedure TFopsiClientKiosk.FormActivate(Sender: TObject);
var
  ErrorMsg: String;
  optionlist : TStringlist;

begin
  if not StartupDone then
  begin
    StartupDone := True;
    Application.ProcessMessages;
    optionlist := TStringlist.Create;
    optionlist.Append('fqdn::');
    optionlist.Append('lang:');
    // quick check parameters
    ErrorMsg:= Application.CheckOptions('',optionlist);
    if ErrorMsg<>'' then begin
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
    LabelWait.Visible:=false;
    grouplist.Enabled:=true;
  end;
end;

procedure TFopsiClientKiosk.FormShow(Sender: TObject);
begin
  LabelDataload.Caption := 'Init';
  StartupDone := False;
end;

procedure TFopsiClientKiosk.grouplistSelectionChange(Sender: TObject; User: boolean);
var
  i: integer;
begin
  ProgressBar1.Visible := True;
  ProgressBarDetail.Visible := True;
  LabelWait.Visible:=true;
  grouplist.Enabled:=false;

  if (grouplist.Items.Count <= 0) or ('' = grouplist.Items.Strings[0]) then
  begin
    LogDatei.log('No productgroups found', LLError);
    Fopsiclientkiosk.StatusBar1.Panels[0].Text := rsNoGroups;
  end
  else
  begin
    // fill product list
    if grouplist.SelCount = 0 then
    begin
      ; // clear product list
      ockdata.ZMQUerydataset1.EmptyDataSet;
      ockdata.ZMQUerydataset1.Open;
      ockdata.ZMQUerydataset2.EmptyDataSet;
      ockdata.ZMQUerydataset2.Open;
      ockdata.ZMQUerydataset2.MasterFields.Clear;
      ockdata.ZMQUerydataset2.MasterDetailFiltration := False;
    end
    else
    begin
      ockdata.ZMQUerydataset1.EmptyDataSet;
      ockdata.ZMQUerydataset1.Open;
      ockdata.ZMQUerydataset2.EmptyDataSet;
      ockdata.ZMQUerydataset2.Open;
      ockdata.ZMQUerydataset2.MasterFields.Clear;
      ockdata.ZMQUerydataset2.MasterDetailFiltration := False;
      productIdsList.Clear;
      if grouplist.Items.Count = 1 then
      begin
        fillproductsbygroup(grouplist.Items.Strings[0]);
        ockdata.fetchProductData;
      end
      else
      begin
        if grouplist.Selected[0] and (grouplist.Count > 1) then
        begin
          // 'All groups' are selected
          for i := 1 to grouplist.Items.Count - 1 do
            fillproductsbygroup(grouplist.Items.Strings[i]);
          ockdata.fetchProductData;
        end
        else
        begin
          for i := 1 to grouplist.Items.Count - 1 do
            if grouplist.Selected[i] then
              fillproductsbygroup(grouplist.Items.Strings[i]);
          ockdata.fetchProductData;
        end;
      end;
    end;
    ZMQUerydataset2.MasterFields.Add('ProductId');
    //ZMQUerydataset2.MasterReferentialKeys.;
    try
      ZMQUerydataset2.MasterDetailFiltration := True;
    except
    end;
  end;
  LabelDataload.Caption := '';
  LabelDataLoadDetail.Caption := '';
  ProgressBar1.Visible := False;
  ProgressBarDetail.Visible := False;
  LabelWait.Visible:=false;
  grouplist.Enabled:=true;
end;

procedure TFopsiClientKiosk.searchEditChange(Sender: TObject);
begin
  // search in list
  SpeedButton1Click(Sender);
end;

procedure TFopsiClientKiosk.FormCreate(Sender: TObject);
var
  skinpath : string;
  myini: TInifile;
begin
  // Load custom skin
  skinpath := Application.Location+PathDelim+'opsiclientkioskskin'+PathDelim;
  If FileExistsUTF8(skinpath+'opsiclientkiosk.png') then
  begin
    ImageHeader.Picture.LoadFromFile(skinpath+'opsiclientkiosk.png');
  end;
  If FileExistsUTF8(skinpath+'opsiclientkiosk.ini') then
  begin
    myini := TIniFile.Create(skinpath+'opsiclientkiosk.ini');
    TitleLabel.Caption        :=myini.ReadString('TitleLabel', 'text', 'opsi Client Kiosk');
    TitleLabel.Font.Name      :=myini.ReadString('TitleLabel', 'FontName', 'Arial');
    TitleLabel.Font.Size      :=myini.ReadInteger('TitleLabel', 'FontSize', 20);
    TitleLabel.Font.Color     :=myini.ReadInteger('TitleLabel', 'FontColor', $00000000);
    TitleLabel.Font.Bold      :=myini.ReadBool('TitleLabel', 'FontBold', true);
    TitleLabel.Font.Italic    :=myini.ReadBool('TitleLabel', 'FontItalic', false);
    TitleLabel.Font.Underline :=myini.ReadBool('TitleLabel', 'FontUnderline', false);
    myini.Free;
    (*
    [TitleLabel]
    Text= opsi Client Kiosk
    FontName = Arial
    FontSize = 20
    FontColor = $00000000
    FontBold = false
    FontItalic = false
    FontUnderline = false
    *)
  end;

  // skinpath in opsiclientagent custom dir
  skinpath := Application.Location+PathDelim+'..\custom\opsiclientkioskskin'+PathDelim;
  If FileExistsUTF8(skinpath+'opsiclientkiosk.png') then
  begin
    ImageHeader.Picture.LoadFromFile(skinpath+'opsiclientkiosk.png');
  end;
  If FileExistsUTF8(skinpath+'opsiclientkiosk.ini') then
  begin
    myini := TIniFile.Create(skinpath+'opsiclientkiosk.ini');
    TitleLabel.Caption        :=myini.ReadString('TitleLabel', 'text', 'opsi Client Kiosk');
    TitleLabel.Font.Name      :=myini.ReadString('TitleLabel', 'FontName', 'Arial');
    TitleLabel.Font.Size      :=myini.ReadInteger('TitleLabel', 'FontSize', 20);
    TitleLabel.Font.Color     :=myini.ReadInteger('TitleLabel', 'FontColor', $00000000);
    TitleLabel.Font.Bold      :=myini.ReadBool('TitleLabel', 'FontBold', true);
    TitleLabel.Font.Italic    :=myini.ReadBool('TitleLabel', 'FontItalic', false);
    TitleLabel.Font.Underline :=myini.ReadBool('TitleLabel', 'FontUnderline', false);
    myini.Free;
    (*
    [TitleLabel]
    Text= opsi Client Kiosk
    FontName = Arial
    FontSize = 20
    FontColor = $00000000
    FontBold = false
    FontItalic = false
    FontUnderline = false
    *)
  end;
  GetDefaultLang;
  grouplist.Clear;
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

  ockdata.initdb;
  //DataSource1.DataSet:= ockdata.ZMQUerydataset2;
  DataSource1.DataSet := ZMQUerydataset1;
  DBGrid1.DataSource := DataSource1;
  DataSource2.DataSet := ockdata.ZMQUerydataset2;
  DBGrid2.DataSource := DataSource2;
  ZMQUerydataset2.MasterSource := DataSource1;
  LabelDataload.Caption := '';
  LabelDataLoadDetail.Caption := '';
  Progressbar1.Position := 0;
  ProgressBar1.Max := 8;
  ProgressbarDetail.Position := 0;
  //ProgressBar1.Brush.Color:=clBlue;
  //ProgressbarDetail.Brush.Color:=clBlue;
  //progressbar1.Perform(PBM_SETBARCOLOR, 0, clBlue);
  //SendMessage(ProgressBar1.Handle, PBM_SETBARCOLOR, 0, clBlue);
  productdetailpanel.Height := 0;
  //ZMReferentialKey1.MasterDataSet := ZMQUerydataset1;
  //ZMReferentialKey1.SlaveDataSet := ZMQUerydataset2;
  //ZMReferentialKey1.JoinedFields.Add('ProductId=ProductId');
end;

(*
procedure TFopsiClientKiosk.RadioGroup1Click(Sender: TObject);
begin
  Notebook1.PageIndex := RadioGroup1.ItemIndex;
end;
*)

procedure TFopsiClientKiosk.searchEditEnter(Sender: TObject);
begin
  productdetailpanel.Height := 0;
end;

procedure TFopsiClientKiosk.SpeedButton1Click(Sender: TObject);
var
  Filtercond, Filterstr: string;
begin
  // search in list
  //ockdata.ZMQUerydataset1.SQL.Text:='select * from kiosk where ProductId like %'+searchedit.Text+'%;';
  //ockdata.ZMQUerydataset1.QueryExecute;
  if searchedit.Text = '' then
    Filtercond := '"*"'
  else
    Filtercond := '"*' + searchedit.Text + '*"';
  Filterstr := 'ProductId =' + Filtercond;
  Filterstr := Filterstr + 'or ProductName =' + Filtercond;
  Filterstr := Filterstr + 'or DESCRIPTION =' + Filtercond;
  Filterstr := Filterstr + 'or ADVICE =' + Filtercond;
  Filterstr := Filterstr + 'or INSTALLATIONSTATUS =' + Filtercond;
  ockdata.ZMQUerydataset1.Filter := Filterstr;
  ockdata.ZMQUerydataset1.FilterOptions:=[foCaseInsensitive];
  ockdata.ZMQUerydataset1.Filtered := True;
end;

procedure TFopsiClientKiosk.SpeedButtonAllClick(Sender: TObject);
begin
  searchedit.Text := '';
  ockdata.ZMQUerydataset1.Filter := '"*"';
  ockdata.ZMQUerydataset1.Filtered := False;
end;

procedure TFopsiClientKiosk.SpeedButtonViewListClick(Sender: TObject);
begin
  Notebook1.PageIndex := 0;
end;

procedure TFopsiClientKiosk.SpeedButtonViewStoreClick(Sender: TObject);
begin
  Notebook1.PageIndex := 1;
end;

procedure TFopsiClientKiosk.ZMQueryDataSet1NewRecord(DataSet: TDataSet);
begin

end;

procedure TFopsiClientKiosk.BitBtnCancelClick(Sender: TObject);
begin
  if opsidata <> nil then
  begin
    // do not send log
    //opsidata.finishOpsiConf;
    opsidata.Free;
  end;
  Application.Terminate;
end;

end.
