unit subform;

{$mode objfpc}{$H+}

interface

{These should be fairly obvious!}
{$DEFINE DEBUGDUPINGCONTROLEVENTS}
{$DEFINE DEBUGDUPINGCONTROLS}
{$DEFINE DEBUGREPARENTING}
{$DEFINE DEBUGUPDATES}
{$DEFINE DEBUGFINDEDITDATASET}

uses
  Classes, SysUtils, forms, ExtCtrls, dbctrls, DB, LMessages, DBGrids, Controls;

Type
  TSubFormDirection = (
    sfHorizontal,
    sfVertical
    );

  TSubForm = class(TPanel)
  private
    { private declarations }
    FComponentDataLink : TComponentDataLink;
    FDirection : TSubFormDirection;
    TemplatePanel : TPanel;
    ScrollBox: TScrollBox;
    Bookmarks: TFPList;
    IsLoaded: Boolean;
    SelfOperatingOnDataSet : Boolean;
    FOnFindClass : TFindComponentClassEvent;
    fCascadeOnFindClass : Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function DupeControl(AControl: TControl; NewOwner: TControl): TControl;
    procedure MemDSBeforePost(DataSet: TDataSet);
    procedure InternalOnFindClass(Reader: TReader; const AClassName: string; var ComponentClass: TComponentClass);
    procedure OnDataLinkFired(DataSet: TDataSet);
    function GetDirection: TSubFormDirection;
    procedure SetDirection(Value: TSubFormDirection);
    //FIXME This procedure should work but it's crashing, it's not necessary in my design, just a useful one.
    procedure Post;
  protected
    procedure Loaded; override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSubForm;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Direction: TSubFormDirection read GetDirection write SetDirection default sfVertical;
    property CascadeOnFindClass: Boolean read fCascadeOnFindClass write fCascadeOnFindClass;
    property OnFindClass: TFindComponentClassEvent read fOnFindClass write fOnFindClass;
  end;
  
procedure Register;

implementation

Uses LResources, lclproc, TypInfo, StdCtrls, Buttons, Maskedit, CheckLst, Grids,
  PairSplitter, ColorBox, ComCtrls, Dialogs, Spin, Arrow, Calendar, EditBtn,
  FileCtrl, TaChart, ButtonPanel, MemDS;
  
procedure Register;
Begin
  RegisterComponents('Data Controls', [TSubForm]);
End;

Constructor TSubForm.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  IsLoaded := False;
  SelfOperatingOnDataSet := False;
  FDirection := sfVertical;
  TemplatePanel := TPanel.Create(nil);
  TemplatePanel.Name := 'XYZZY_TemplatePanel';
  TemplatePanel.Parent := nil;
  TemplatePanel.Width := Width;
  TemplatePanel.Height := Height;
  ScrollBox := TScrollBox.Create(nil);
  ScrollBox.Name := 'XYZZY_ScrollBox';
  if (not (csDesigning in ComponentState)) then
  Begin
    ScrollBox.Parent := Self;
    ScrollBox.Width := Width;
    ScrollBox.Height := Height;
    ScrollBox.Align := alClient;
    ScrollBox.AutoScroll := True;
  End;
  FComponentDataLink := TComponentDataLink.Create;
  FComponentDataLink.OnDataSetChanged := @OnDataLinkFired;
  FComponentDataLink.OnNewDataSet := @OnDataLinkFired;
  FComponentDataLink.OnDataSetOpen := @OnDataLinkFired;
  FComponentDataLink.OnInvalidDataSet := @OnDataLinkFired;
  FComponentDataLink.OnInvalidDataSource := @OnDataLinkFired;
  FComponentDataLink.OnLayoutChanged := @OnDataLinkFired;
  FComponentDataLink.OnDataSetClose := @OnDataLinkFired;
  FComponentDataLink.OnEditingChanged := @OnDataLinkFired;
End;

function TSubForm.DupeControl(AControl: TControl; NewOwner: TControl): TControl;
Var
  AStream : TMemoryStream;
  I : Integer;
  Count, Loop: Integer;
  List: TPropList;

Procedure MakeOwnedByMeChildControls(AControl: TWinControl);
//
//  This procedure iterates through the child controls of AControl and ensures
//   that the Owner is AControl.  This is because Lazarus (because Delphi does)
//   makes all components created at designtime owned by the form, not their
//   parent
Var
  I : Integer;
  TheControl: TComponent;
Begin
  For I := 0 to AControl.ComponentCount-1 do
  Begin
    TheControl := AControl.Components[I];
    {$IFDEF DEBUGREPARENTING}
    Debugln('[TSubForm.MakeOwned...] Reparenting '+AControl.Components[I].Name);
    {$ENDIF}
//  The next two lines do the equivilant of
//  TheControl.Owner := AControl;
    TheControl.Owner.RemoveComponent(TheControl);
    AControl.InsertComponent(TheControl);
    If TheControl is TWinControl then
      MakeOwnedByMeChildControls(TheControl as TWinControl)
  End
End;

Procedure DupeEvents(Control, Dupe: TControl);
Var
  Loop, Count : Integer;
Begin
  Count := GetPropList(Control.ClassInfo, tkMethods, @List);
  for Loop := 0 to Count - 1 do
  Begin
    {$IFDEF DEBUGDUPINGCONTROLEVENTS}
    Debugln('Setting '+List[Loop]^.Name+' method of control '+Control.Name+' which is of type '+Control.ClassName);
    {$ENDIF}
    SetMethodProp(Dupe, List[Loop]^.Name, GetMethodProp(Control, List[Loop]^.Name));
  End;
  If Control is TWinControl then
    For Loop := 0 to (Control as TWinControl).ControlCount-1 do
    begin
      DupeEvents((Control as TWinControl).Controls[Loop], (Dupe as TWinControl).Controls[Loop])
    end
End;

Begin
  // Workaround - not expecting fix!  All components created at designtime are
  // owned by the form, not by their parent.
  If AControl is TWinControl then
    MakeOwnedByMeChildControls(AControl as TWinControl);
  AStream := TMemoryStream.Create;
  WriteComponentAsBinaryToStream(AStream, AControl);
  AStream.Position := 0;
  Result := nil;
  ReadComponentFromBinaryStream(AStream, Result, @Self.InternalOnFindClass, NewOwner);
  If NewOwner is TWinControl then
    Result.Parent := TWinControl(NewOwner);
  DupeEvents(AControl, Result);
End;

procedure TSubForm.InternalOnFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  If fCascadeOnFindClass or (FOnFindClass = nil) then
  Begin
    If CompareText(AClassName,'TLabel')=0 then
      ComponentClass:=TLabel
    else if CompareText(AClassName,'TPanel')=0 then
      ComponentClass:=TPanel
    else if CompareText(AClassName,'TEdit')=0 then
      ComponentClass:=TEdit
    else if CompareText(AClassName,'TCheckBox')=0 then
      ComponentClass:=TCheckBox
    else if CompareText(AClassName,'TToggleBox')=0 then
      ComponentClass:=TToggleBox
    else if CompareText(AClassName,'TRadioButton')=0 then
      ComponentClass:=TRadioButton
    else if CompareText(AClassName,'TListBox')=0 then
      ComponentClass:=TListBox
    else if CompareText(AClassName,'TComboBox')=0 then
      ComponentClass:=TComboBox
    else if CompareText(AClassName,'TScrollBar')=0 then
      ComponentClass:=TScrollBar
    else if CompareText(AClassName,'TBevel')=0 then
      ComponentClass:=TBevel
    else if CompareText(AClassName,'TMemo')=0 then
      ComponentClass:=TMemo
    else if CompareText(AClassName,'TGroupBox')=0 then
      ComponentClass:=TGroupBox
    else if CompareText(AClassName,'TRadioGroup')=0 then
      ComponentClass:=TRadioGroup
    else if CompareText(AClassName,'TCheckGroup')=0 then
      ComponentClass:=TCheckGroup
    else if CompareText(AClassName,'TButton')=0 then
      ComponentClass:=TButton
    else if CompareText(AClassName,'TBitBtn')=0 then
      ComponentClass:=TBitBtn
    else if CompareText(AClassName,'TSpeedButton')=0 then
      ComponentClass:=TSpeedButton
    else if CompareText(AClassName,'TStaticText')=0 then
      ComponentClass:=TStaticText
    else if CompareText(AClassName,'TImage')=0 then
      ComponentClass:=TImage
    else if CompareText(AClassName,'TShape')=0 then
      ComponentClass:=TShape
    else if CompareText(AClassName,'TNotebook')=0 then
      ComponentClass:=TNotebook
    else if CompareText(AClassName,'TPage')=0 then
      ComponentClass:=TPage
    else if CompareText(AClassName,'TPaintBox')=0 then
      ComponentClass:=TPaintBox
    else if CompareText(AClassName,'TLabeledEdit')=0 then
      ComponentClass:=TLabeledEdit
    else if CompareText(AClassName,'TSplitter')=0 then
      ComponentClass:=TSplitter
    else if CompareText(AClassName,'TMaskEdit')=0 then
      ComponentClass:=TMaskEdit
    else if CompareText(AClassName,'TCheckListBox')=0 then
      ComponentClass:=TCheckListBox
    else if CompareText(AClassName,'TScrollBox')=0 then
      ComponentClass:=TScrollBox
    else if CompareText(AClassName,'TStringGrid')=0 then
      ComponentClass:=TStringGrid
    else if CompareText(AClassName,'TDrawGrid')=0 then
      ComponentClass:=TDrawGrid
    else if CompareText(AClassName,'TPairSplitter')=0 then
      ComponentClass:=TPairSplitter
    else if CompareText(AClassName,'TColorBox')=0 then
      ComponentClass:=TColorBox
    else if CompareText(AClassName,'TColorListBox')=0 then
      ComponentClass:=TColorListBox
    else if CompareText(AClassName,'TTrackBar')=0 then
      ComponentClass:=TTrackBar
    else if CompareText(AClassName,'TProgressBar')=0 then
      ComponentClass:=TProgressBar
    else if CompareText(AClassName,'TTreeView')=0 then
      ComponentClass:=TTreeview
    else if CompareText(AClassName,'TListView')=0 then
      ComponentClass:=TListView
    else if CompareText(AClassName,'TStatusBar')=0 then
      ComponentClass:=TStatusBar
    else if CompareText(AClassName,'TToolBar')=0 then
      ComponentClass:=TToolBar
    else if CompareText(AClassName,'TToolButton')=0 then
      ComponentClass:=TToolButton
    else if CompareText(AClassName,'TUpDown')=0 then
      ComponentClass:=TUpDown
    else if CompareText(AClassName,'TPageControl')=0 then
      ComponentClass:=TPageControl
    else if CompareText(AClassName,'TTabControl')=0 then
      ComponentClass:=TTabControl
    else if CompareText(AClassName,'TColorButton')=0 then
      ComponentClass:=TColorButton
    else if CompareText(AClassName,'TSpinEdit')=0 then
      ComponentClass:=TSpinEdit
    else if CompareText(AClassName,'TFloatSpinEdit')=0 then
      ComponentClass:=TFloatSpinEdit
    else if CompareText(AClassName,'TArrow')=0 then
      ComponentClass:=TArrow
    else if CompareText(AClassName,'TCalendar')=0 then
      ComponentClass:=TCalendar
    else if CompareText(AClassName,'TEditButton')=0 then
      ComponentClass:=TEditButton
    else if CompareText(AClassName,'TFileNameEdit')=0 then
      ComponentClass:=TFileNameEdit
    else if CompareText(AClassName,'TDirectoryEdit')=0 then
      ComponentClass:=TDirectoryEdit
    else if CompareText(AClassName,'TDateEdit')=0 then
      ComponentClass:=TDateEdit
    else if CompareText(AClassName,'TCalcEdit')=0 then
      ComponentClass:=TCalcEdit
    else if CompareText(AClassName,'TFileListBox')=0 then
      ComponentClass:=TFileListBox
    else if CompareText(AClassName,'TBarChart')=0 then
      ComponentClass:=TBarChart
    else if CompareText(AClassName,'TButtonPanel')=0 then
      ComponentClass:=TButtonPanel
    else if CompareText(AClassName,'TSubForm')=0 then
      ComponentClass:=TSubform
    else if CompareText(AClassName,'TDBNavigator')=0 then
      ComponentClass:=TDBNavigator
    else if CompareText(AClassName,'TDBText')=0 then
      ComponentClass:=TDBText
    else if CompareText(AClassName,'TDBEdit')=0 then
      ComponentClass:=TDBEdit
    else if CompareText(AClassName,'TDBMemo')=0 then
      ComponentClass:=TDBMemo
    else if CompareText(AClassName,'TDBImage')=0 then
      ComponentClass:=TDBImage
    else if CompareText(AClassName,'TDBListBox')=0 then
      ComponentClass:=TDBListBox
    else if CompareText(AClassName,'TDBCombobox')=0 then
      ComponentClass:=TDBComboBox
    else if CompareText(AClassName,'TDBCheckbox')=0 then
      ComponentClass:=TDBCheckBox
    else if CompareText(AClassName,'TDBRadioGroup')=0 then
      ComponentClass:=TDBRadioGroup
    else if CompareText(AClassName,'TDBCalendar')=0 then
      ComponentClass:=TDBCalendar
    else if CompareText(AClassName,'TDBGrid')=0 then
      ComponentClass:=TDBGrid
    else if CompareText(AClassName,'TDBGroupBox')=0 then
      ComponentClass:=TDBGroupBox
  End;
  If Assigned(FOnFindClass) then FOnFindClass(Reader, AClassName, ComponentClass)
end;

procedure TSubForm.UpdateSubForm;
Var
  APanel : TPanel;
  AMemDS : TMemDataset;
  ADS : TDataSource;
  I, I2 : Integer;
  MaxExtent ,
  LastExtent: Integer;
  ABookMark : TBookMark;
  Count, Loop: Integer;
  List: TPropList;
  HoldSelfOp : Boolean;
begin
  If not IsLoaded then Exit;
  if csDesigning in ComponentState then Exit;
  If BookMarks = nil then BookMarks := TFPList.Create;
  If Bookmarks.Count > 0 then
  Begin
    HoldSelfOp := SelfOperatingOnDataSet;
    SelfOperatingOnDataSet := True;
    For I := Bookmarks.Count-1 downto 0 do
      Try
        If Assigned(FComponentDataLink.DataSet) then
          FComponentDataLink.DataSet.FreeBookmark(Bookmarks[I])
      Except
        //quite probable the datasource / dataset has been changed on us, so don't error here
      End;
    SelfOperatingOnDataSet := HoldSelfOp;
  End;
  // Tidy up incase
  Bookmarks.Free;
  BookMarks := TFPList.Create;
  // Remove everything but the TemplatePanel
  For I := ScrollBox.ComponentCount-1 downto 0 do
    If (ScrollBox.Components[I] is TPanel) and ((ScrollBox.Components[I] as TPanel) <> TemplatePanel) then
      ScrollBox.Components[I].Free;
  // And exit if there's no data to display
  If FComponentDataLink.DataSource = nil then exit;
  If FComponentDataLink.DataSet = nil then exit;
  If FComponentDataLink.DataSet.RecordCount = 0 then exit;
  HoldSelfOp := SelfOperatingOnDataSet;
  SelfOperatingOnDataSet := True;
  ScrollBox.Width := ClientWidth;
  ScrollBox.Height := ClientHeight;
  LastExtent := 0;
  FComponentDataLink.DataSet.First;
  For I := 0 to FComponentDataLink.Dataset.RecordCount - 1 do
  Begin
    APanel := TPanel(DupeControl(TemplatePanel,ScrollBox));
    APanel.Caption := '';
    APanel.Visible := True;
    AMemDS := TMemDataset.Create(APanel);  // Created owned by panel so when the panel gets freed...
    AMemDS.CopyFromDataset(FComponentDataLink.Dataset, False);
    //Note: MemDS doesn't do well with CopyFromDataset in a fair few instances, hence the manual dump of data below.
    //I may even have to manually create the fields to do something sensible like create autoinc's as integers.
    AMemDS.Open;
    AMemDS.Append;
    For I2 := 0 to FComponentDataLink.Dataset.FieldCount-1 do
      AMemDS.Fields[I2].AsVariant := FComponentDataLink.Dataset.Fields[I2].AsVariant;
    AMemDS.Post;
    AMemDS.Tag := I;  // Just in case we or the user needs a way to work out what record number they're on (zero based)
    AMemDS.BeforePost := @MemDSBeforePost;
    ADS := TDataSource.Create(APanel);     // Created owned by panel so when the panel gets freed...
    ADS.DataSet := AMemDS;
    ADS.AutoEdit := FComponentDataLink.DataSource.AutoEdit;
    ABookMark := FComponentDataLink.Dataset.GetBookmark;
    If (ABookMark = nil) or (not FComponentDataLink.Dataset.BookmarkValid(ABookmark)) then
    Begin
      DebugLn('[TSubForm.UpdateSubForm] Not a bookmarkable dataset! Expect problems!');
      DebugLn('[TSubForm.UpdateSubForm] Note that in fpc fixes_2_2 branch, as late as r7162, some');
      DebugLn('[TSubForm.UpdateSubForm]  DataSets always report bookmarks as invalid but they');
      DebugLn('[TSubForm.UpdateSubForm]  still actually work.  I would normally error out here,');
      DebugLn('[TSubForm.UpdateSubForm]  but as this is quite a common situation, we must just');
      DebugLn('[TSubForm.UpdateSubForm]  hope that there aren''t any real unbookmarkable datasets,');
      DebugLn('[TSubForm.UpdateSubForm]  else this component could silently corrupt data!')
      //APanel.Free;
      //SelfOperatingOnDataSet := HoldSelfOp;
      //Exit
    End;
    BookMarks.Add(ABookmark);
    For I2 := 0 to APanel.ControlCount-1 do
    Begin
      {$IFDEF DEBUGUPDATES}
      Debugln('[TSubForm.UpdateSubForm] Considering '+APanel.Controls[I2].Name);
      {$ENDIF}
      Count := GetPropList(APanel.Controls[I2].ClassInfo, tkAny, @List);
      for Loop := 0 to Count - 1 do
      Begin
        If List[Loop]^.PropType^.Kind in [tkObject, tkClass] then
        Begin
          {$IFDEF DEBUGUPDATES}
          Debugln('[TSubForm.UpdateSubForm] Considering '+APanel.Controls[I2].Name+'.'+List[Loop]^.Name);
          {$ENDIF}
          If GetObjectPropClass(APanel.Controls[I2], List[Loop]^.Name).InheritsFrom(FComponentDataLink.DataSource.ClassType) then
          Begin
            {$IFDEF DEBUGUPDATES}
            Debugln('[TSubForm.UpdateSubForm] Inherits from TDataSource');
            {$ENDIF}
            if (GetObjectProp(APanel.Controls[I2], List[Loop]^.Name) as TDataSource) = FComponentDataLink.DataSource then
            Begin
              {$IFDEF DEBUGUPDATES}
              Debugln('[TSubForm.UpdateSubForm] Is same DataSource');
              Debugln('[TSubForm.UpdateSubForm] Setting '+APanel.Controls[I2].Name+'.'+List[Loop]^.Name+' to internal memds');
              {$ENDIF}
              SetObjectProp(APanel.Controls[I2], List[Loop]^.Name, ADS);
            End
          End
        End
      End;
    End;
    // This goes at the end because any data-aware component could change size based on the data in the dataset
    // So the extents of the panel are most likely to change per-record after the code above.
    If fDirection = sfVertical then
    Begin
      MaxExtent := 0;
      For I2 := 0 to APanel.ControlCount-1 do
        If APanel.Controls[I2].Top + APanel.Controls[I2].Height > MaxExtent then
          MaxExtent := APanel.Controls[I2].Top + APanel.Controls[I2].Height;
      // Add a border
      MaxExtent := MaxExtent + 2;
      APanel.Top := LastExtent;
      APanel.Left := 0;
      APanel.Width := ScrollBox.Width;
      APanel.Height := MaxExtent;
      LastExtent := APanel.Top + APanel.Height
    End
    else
    Begin
      MaxExtent := 0;
      For I2 := 0 to APanel.ControlCount-1 do
        If APanel.Controls[I2].Left + APanel.Controls[I2].Width > MaxExtent then
          MaxExtent := APanel.Controls[I2].Left + APanel.Controls[I2].Width;
      // Add a border
      MaxExtent := MaxExtent + 2;
      APanel.Left := LastExtent;
      APanel.Top := 0;
      APanel.Height := ScrollBox.Height;
      APanel.Width := MaxExtent;
      LastExtent := APanel.Left + APanel.Width
    End;
    FComponentDataLink.Dataset.Next
  End;
  FComponentDataLink.Dataset.First;
  SelfOperatingOnDataSet := HoldSelfOp
end;

destructor TSubForm.Destroy;
begin
  ScrollBox.Free;
  TemplatePanel.Free;
  FComponentDataLink.Free;
  inherited Destroy;
end;

function TSubForm.GetDataSource: TDataSource;
begin
  Result := FComponentDataLink.DataSource;
end;

procedure TSubForm.SetDataSource(Value: TDataSource);
begin
  FComponentDataLink.DataSource := Value;
  If IsLoaded and (not (csDesigning in ComponentState)) then UpdateSubForm
end;

function TSubForm.GetDirection: TSubFormDirection;
begin
  Result := FDirection
end;

procedure TSubForm.SetDirection(Value: TSubFormDirection);
begin
  If Value = FDirection then exit;
  FDirection := Value;
  If IsLoaded and (not (csDesigning in ComponentState)) then UpdateSubForm
end;

{ Protected Methods}
procedure TSubForm.Loaded;
Var
  AddedControl : TControl;
  I : Integer;
begin
  inherited Loaded;
  If (Self.ControlCount > 0) and (not (csDesigning in ComponentState)) then
    For I := Self.ControlCount-1 downto 0 do
    Begin
      AddedControl := Self.Controls[I];
      {$IFDEF DEBUGREPARENTING}
      Debugln('[TSubForm.Loaded] Control ('+AddedControl.ClassName+') '+AddedControl.Name+' found.');
      If (AddedControl.Owner = nil) then
        Debugln('[TSubForm.Loaded] Control '+AddedControl.Name+' has no owner.')
      else
        Debugln('[TSubForm.Loaded] Control''s Owner is '+AddedControl.Owner.ClassName);
      {$ENDIF}
      If ((AddedControl.Owner is TForm) or
        ((AddedControl.Owner=nil) and (AddedControl.Name <> 'XYZZY_ScrollBox') and (AddedControl.Name <> 'XYZZY_TemplatePanel'))) then  // Added at designtime ;-)
      Begin
        {$IFDEF DEBUGREPARENTING}
        Debugln('[TSubForm.Loaded] Control '+AddedControl.Name+'''s owner is a form or nil and not one of our special two, so reparenting.');
        {$ENDIF}
        AddedControl.Parent.Owner.RemoveComponent(AddedControl);
        TemplatePanel.InsertComponent(AddedControl);
        AddedControl.Parent := TemplatePanel
      End
    End;
  IsLoaded := True;
  If (not (csDesigning in ComponentState)) then UpdateSubForm
end;

Procedure TSubForm.OnDataLinkFired(DataSet: TDataSet);
Begin
  If not IsLoaded then Exit;
  if csDesigning in ComponentState then Exit;
  if Self = nil then Exit;
  if SelfOperatingOnDataSet then Exit;
  UpDateSubForm
End;

Procedure TSubForm.Post;

Function IterateForDataSetInEditMode(TheControl: TComponent): TDataSet;
Var
  Compon : Integer;
  ADataSet : TMemDataset;
Begin
  ADataSet := TMemDataSet.Create(Nil);
  Result := nil;
  Compon := TheControl.ComponentCount-1;
  Repeat
    {$IFDEF DEBUGFINDEDITDATASET}
    DebugLn('[TSubForm.Post] Considering a '+TheControl.Components[Compon].ClassName);
    {$ENDIF}
    If TheControl.Components[Compon].ClassType.InheritsFrom(ADataSet.ClassType) then
    Begin
      {$IFDEF DEBUGFINDEDITDATASET}
      DebugLn('[TSubForm.Post] Found a Dataset Descendant : '+TheControl.Components[Compon].ClassName+' : '+TheControl.Components[Compon].Name);
      {$ENDIF}
      // Is a DataSet of some description!
      if TMemDataSet(TheControl.Components[Compon]).State in dsEditModes then
      Begin
        Result := TMemDataSet(TheControl.Components[Compon]);
        {$IFDEF DEBUGFINDEDITDATASET}
        DebugLn('[TSubForm.Post] Found an editmode dataset!!! I should be exiting now.');
        {$ENDIF}
        Compon := 0
      End;
    End
    else //not a dataset
    Begin
      If TheControl.Components[Compon].ComponentCount > 0 then
      // has components itself, they may be datasets!
        Result := IterateForDataSetInEditMode(TheControl.Components[Compon]);
    End;
    Dec(Compon)
  Until Compon = -1;
  ADataSet.Free
End;

Var
  TheEditingDataSet : TDataSet;
  HoldSelfOp : Boolean;
Begin
  TheEditingDataSet := IterateForDataSetInEditMode(ScrollBox);
  If TheEditingDataSet <> nil then
  Begin
    HoldSelfOp := SelfOperatingOnDataSet;
    SelfOperatingOnDataSet := True;
    TheEditingDataSet.Post;
    SelfOperatingOnDataSet := HoldSelfOp
  End
End;

Procedure TSubForm.MemDSBeforePost(DataSet: TDataSet);

function dsstostr(state : TDataSetState): String;
begin
  Result := 'Unknown state result!';
  case State of
    dsInactive     : Result := 'dsInactive';
    dsBrowse       : Result := 'dsBrowse';
    dsEdit         : Result := 'dsEdit';
    dsInsert       : Result := 'dsInsert';
    dsSetKey       : Result := 'dsSetKey';
    dsCalcFields   : Result := 'dsCalcFields';
    dsFilter       : Result := 'dsFilter';
    dsNewValue     : Result := 'dsNewValue';
    dsOldValue     : Result := 'dsOldValue';
    dsCurValue     : Result := 'dsCurValue';
    dsBlockRead    : Result := 'dsBlockRead';
    dsInternalCalc : Result := 'dsInternalCalc';
    dsOpening      : Result := 'dsOpening'
  End;
end;
Var
  FieldNo : Integer;
  HoldSelfOp : Boolean;
Begin
  HoldSelfOp := SelfOperatingOnDataSet;
  SelfOperatingOnDataSet := True;
  if FComponentDataLink.DataSource.AutoEdit or (FComponentDataLink.DataSet.State in dsEditModes) then
  Begin
    FComponentDataLink.DataSet.GotoBookmark(Bookmarks[DataSet.Tag]);
    If not (FComponentDataLink.DataSet.State in dsEditModes) then
      FComponentDataLink.DataSet.Edit;
    For fieldno := 0 to DataSet.FieldCount-1 do
    Begin
      FComponentDataLink.DataSet.FieldByName(DataSet.Fields[fieldNo].FieldName).AsVariant :=
        DataSet.Fields[FieldNo].AsVariant;
    End;
    FComponentDataLink.DataSet.Post
  End
  else
    DataSet.Cancel;
  SelfOperatingOnDataSet := HoldSelfOp
//  UpdateSubForm
End;

initialization

{$I component.lrs}

end.

