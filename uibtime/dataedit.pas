unit dataedit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, {LMessages, Messages,} SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  {StdCtrls,} Buttons, Grids, DBGrids, ExtCtrls, DBCtrls, {ToolWin,} ComCtrls,
  DB, sqldb, {DBCGrids, ExtDlgs,} clipbrd, StdCtrls, DateTimePicker, types;

type

  { TFDataedit }

  TFDataedit = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    ButtonReactivate: TButton;
    ButtonRenameEvent: TButton;
    CheckBoxUseDate: TCheckBox;
    ComboBoxOldEvent: TComboBox;
    ComboBoxOldAllEvent: TComboBox;
    DateTimePickerFromDate: TDateTimePicker;
    DBGrid10: TDBGrid;
    DBNavigator10: TDBNavigator;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelProgress: TLabel;
    PageControl1: TPageControl;
    SQLQueryRenameEvent: TSQLQuery;
    TabSheet1: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet7: TTabSheet;
    ToolBar1: TToolBar;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    TabSheet2: TTabSheet;
    ToolBar10: TToolBar;
    ToolBar2: TToolBar;
    DBNavigator2: TDBNavigator;
    DBGrid2: TDBGrid;
    TabSheet3: TTabSheet;
    ToolBar3: TToolBar;
    DBNavigator3: TDBNavigator;
    DBGrid3: TDBGrid;
    TabSheet4: TTabSheet;
    ToolBar4: TToolBar;
    DBNavigator4: TDBNavigator;
    DBGrid4: TDBGrid;
    TabSheet5: TTabSheet;
    ToolBar5: TToolBar;
    DBNavigator5: TDBNavigator;
    DBGrid5: TDBGrid;
    TabSheet6: TTabSheet;
    ToolBar6: TToolBar;
    DBNavigator6: TDBNavigator;
    DBGrid6: TDBGrid;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    TabSheet8: TTabSheet;
    ToolBar8: TToolBar;
    DBNavigator8: TDBNavigator;
    BitBtn8: TBitBtn;
    DBGrid8: TDBGrid;
    TabSheet9: TTabSheet;
    ToolBar9: TToolBar;
    DBNavigator9: TDBNavigator;
    BitBtn9: TBitBtn;
    DBGrid9: TDBGrid;
    DBLookupComboBox1: TDBLookupComboBox;
    BtnMultiDay: TBitBtn;
    DBLookupComboBox2: TDBLookupComboBox;
    BtnDuplicate: TBitBtn;
    DBComboBoxBool: TDBComboBox;
    DBLookupComboBox3: TDBLookupComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    DBMemo1: TDBMemo;
    DBLookupComboBox4: TDBLookupComboBox;
    DBLookupComboBox5: TDBLookupComboBox;
    procedure ButtonReactivateClick(Sender: TObject);
    procedure ButtonRenameEventClick(Sender: TObject);
    procedure CheckBoxUseDateChange(Sender: TObject);
    procedure ComboBoxOldAllEventEnter(Sender: TObject);
    procedure ComboBoxOldEventEnter(Sender: TObject);
    procedure DBGrid1EditingDone(Sender: TObject);
    procedure DBGrid1FieldEditMask(Sender: TObject; const Field: TField;
      var Value: string);
    procedure DBGrid3DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid5DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid6DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid9DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure DBLookupComboBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure FormCreate(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure PageControl1Enter(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure DBGrid1Enter(Sender: TObject);
    procedure DBLookupComboBox1Exit(Sender: TObject);
    procedure DBLookupComboBox2Exit(Sender: TObject);
    procedure DBGrid1ColExit(Sender: TObject);
    procedure DBGrid3ColExit(Sender: TObject);
    procedure DBGrid3Enter(Sender: TObject);
    procedure DBLookupComboBox3Exit(Sender: TObject);
    procedure BtnMultiDayClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure DBGrid5Enter(Sender: TObject);
    procedure DBGrid5ColExit(Sender: TObject);
    procedure DBImage1Exit(Sender: TObject);
    procedure BtnDuplicateClick(Sender: TObject);
    procedure DBComboBoxBoolExit(Sender: TObject);
    procedure DBLookupComboBox3Enter(Sender: TObject);
    procedure DBGrid9ColExit(Sender: TObject);
    procedure DBLookupComboBox4Exit(Sender: TObject);
    procedure DBGrid9Enter(Sender: TObject);
    procedure DBLookupComboBox5Exit(Sender: TObject);
    procedure DBGrid6ColExit(Sender: TObject);
    procedure DBGrid6Enter(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
    procedure TabSheet4Show(Sender: TObject);
    procedure TabSheet5Show(Sender: TObject);
    procedure TabSheet6Show(Sender: TObject);
    procedure TabSheet7Show(Sender: TObject);
    procedure TabSheet8Show(Sender: TObject);
    procedure TabSheet9Show(Sender: TObject);

  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  FDataedit: TFDataedit;

implementation

uses uibdata, {debug,} multiday;

{$R *.lfm}

procedure TFDataedit.PageControl1Enter(Sender: TObject);
begin
  Datamodule1.SQuibevent.Last;
  DataModule1.SQQueryAktEvents.Active := False;
  DataModule1.SQQueryAktEvents.Active := True;
end;

procedure TFDataedit.DBGrid3DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
  if (gdFocused in State) then
  begin
    if (dbgrid3.SelectedField.FieldName = 'PARENTEVENT') then
    begin
      DBLookupComboBox3.DataField := dbgrid3.SelectedField.FieldName;
      DBLookupComboBox3.Left := Rect.Left + DBGrid3.Left;
      DBLookupComboBox3.Top := Rect.Top + DBGrid3.top;
      DBLookupComboBox3.Width := Rect.Right - Rect.Left;
      DBLookupComboBox3.Visible := True;
    end;
  end;
end;

procedure TFDataedit.ButtonRenameEventClick(Sender: TObject);
var
  oldname, newname: string;
  usedate: boolean;
  YY, MM, DD: word;
begin
  try
    screen.Cursor := crHourGlass;
    DataModule1.debugOut(5, 'dataedit: ButtonRenameEventClick: Rename from ' +
      ComboBoxOldEvent.Text + ' to ' + edit2.Text);
    LabelProgress.Caption := 'Erstelle neuen Eintrag ....';
    Application.ProcessMessages;
    oldname := ComboBoxOldEvent.Text;
    newname := edit2.Text;
    usedate := CheckBoxUseDate.Checked;
    // create new entry
    SQLQueryRenameEvent.SQL.Clear;
    SQLQueryRenameEvent.SQL.Add('insert into uibaktevent (EVENT,PARENTEVENT,');
    SQLQueryRenameEvent.SQL.Add('COMMENT,ID_STRING,KD_String,TIME_H,PROJECTSTART,');
    SQLQueryRenameEvent.SQL.Add('REPORTREQUIRED,ACC_PER_MONTHNUM,ACCOUNTINGREQUIRED) ');
    SQLQueryRenameEvent.SQL.Add('select "' + newname + '", PARENTEVENT,"COMMENT",');
    SQLQueryRenameEvent.SQL.Add('ID_STRING,KD_String,TIME_H,PROJECTSTART,');
    SQLQueryRenameEvent.SQL.Add('REPORTREQUIRED,ACC_PER_MONTHNUM,ACCOUNTINGREQUIRED ');
    SQLQueryRenameEvent.SQL.Add('from uibaktevent where event = :oldname');
    SQLQueryRenameEvent.ParamByName('oldname').AsString := oldname;
    SQLQueryRenameEvent.ExecSQL;
    DataModule1.debugOut(5,
      'dataedit: ButtonRenameEventClick: created in uibaktevent: ' + newname);
    // del old entry
    LabelProgress.Caption := 'Lösche alten Eintrag ....';
    Application.ProcessMessages;
    SQLQueryRenameEvent.SQL.Clear;
    SQLQueryRenameEvent.SQL.Add('delete from uibaktevent ');
    SQLQueryRenameEvent.SQL.Add('where event = :oldname');
    SQLQueryRenameEvent.ParamByName('oldname').AsString := oldname;
    SQLQueryRenameEvent.ExecSQL;
    DataModule1.debugOut(5,
      'dataedit: ButtonRenameEventClick: deleted from uibaktevent: ' + oldname);
    // rename in event table
    LabelProgress.Caption := 'Ändere Buchungen uibevent....';
    Application.ProcessMessages;
    SQLQueryRenameEvent.SQL.Clear;
    SQLQueryRenameEvent.SQL.Add('update uibevent ');
    SQLQueryRenameEvent.SQL.Add('set event = :newname ');
    SQLQueryRenameEvent.SQL.Add('where event = :oldname');
    if usedate then
    begin
      SQLQueryRenameEvent.SQL.Add(' and starttime >= :mystarttime');
      SQLQueryRenameEvent.ParamByName('mystarttime').AsDate :=
        DateTimePickerFromDate.Date;
    end;
    SQLQueryRenameEvent.ParamByName('oldname').AsString := oldname;
    SQLQueryRenameEvent.ParamByName('newname').AsString := newname;

    SQLQueryRenameEvent.ExecSQL;
    DataModule1.debugOut(5, 'dataedit: ButtonRenameEventClick: renamed in uibevent' +
      oldname + ' to' + newname);
    // rename in UIB_WORK_DESCRIPTION table
    LabelProgress.Caption := 'Ändere Buchungen uibevent....';
    Application.ProcessMessages;
    SQLQueryRenameEvent.SQL.Clear;
    SQLQueryRenameEvent.SQL.Add('update UIB_WORK_DESCRIPTION ');
    SQLQueryRenameEvent.SQL.Add('set event = :newname ');
    SQLQueryRenameEvent.SQL.Add('where event = :oldname');
    if usedate then
    begin
      // = Jahr = Monat >= Tag
      SQLQueryRenameEvent.SQL.Add(' and ((jahr = :mystartjahr ');
      SQLQueryRenameEvent.SQL.Add('       and monat = :mystartmonat ');
      SQLQueryRenameEvent.SQL.Add('       and tag >= :mystarttag) ');
      // = Jahr, > Monat,  Tag egal
      SQLQueryRenameEvent.SQL.Add(' or   (jahr = :mystartjahr ');
      SQLQueryRenameEvent.SQL.Add('       and monat >= :mystartmonat) ');
      // > Jahr,  Monat egal,  Tag egal
      SQLQueryRenameEvent.SQL.Add(' or jahr >= :mystartjahr) ');
      DecodeDate(DateTimePickerFromDate.Date, YY, MM, DD);
      SQLQueryRenameEvent.ParamByName('mystartjahr').AsInteger := YY;
      SQLQueryRenameEvent.ParamByName('mystartmonat').AsDate := MM;
      SQLQueryRenameEvent.ParamByName('mystarttag').AsDate := DD;
    end;
    SQLQueryRenameEvent.ParamByName('oldname').AsString := oldname;
    SQLQueryRenameEvent.ParamByName('newname').AsString := newname;
    SQLQueryRenameEvent.ExecSQL;
    DataModule1.debugOut(5,
      'dataedit: ButtonRenameEventClick: renamed in UIB_WORK_DESCRIPTION' +
      oldname + ' to' + newname);
  finally
    screen.Cursor := crDefault;
    try
      if DataModule1.SQLTransaction1.Active then
      begin
        DataModule1.SQLTransaction1.CommitRetaining;
        DataModule1.debugOut(5, 'Rename Event: (CommitRetaining)');
      end
    except
      DataModule1.debugOut(2, 'Exception in Rename Event: (CommitRetaining)');
    end;
    LabelProgress.Caption := 'Fertig.';
    DataModule1.debugOut(5, 'dataedit: ButtonRenameEventClick: finished');
    Application.ProcessMessages;
  end;
end;

procedure TFDataedit.ButtonReactivateClick(Sender: TObject);
var
  oldname: string;
  oldfound: boolean;
  reavtivate: boolean = False;
begin
  try
    screen.Cursor := crHourGlass;
    DataModule1.debugOut(5, 'dataedit: ButtonReactivateClick: Rename from ' +
      ComboBoxOldAllEvent.Text + ' to ' + edit2.Text);
    oldfound := False;
    Application.ProcessMessages;
    oldname := ComboBoxOldAllEvent.Text;
    // dependen entries exist ?
    SQLQueryRenameEvent.SQL.Clear;
    SQLQueryRenameEvent.SQL.Add('select count(*) as zahl from uibevent ');
    SQLQueryRenameEvent.SQL.Add(' where event = :oldname');
    SQLQueryRenameEvent.ParamByName('oldname').AsString := oldname;
    SQLQueryRenameEvent.Open;
    if SQLQueryRenameEvent.FieldByName('zahl').AsInteger > 0 then
      oldfound := True;
    SQLQueryRenameEvent.SQL.Clear;
    SQLQueryRenameEvent.SQL.Add('select count(*) as zahl from UIB_WORK_DESCRIPTION ');
    SQLQueryRenameEvent.SQL.Add(' where event = :oldname');
    SQLQueryRenameEvent.ParamByName('oldname').AsString := oldname;
    SQLQueryRenameEvent.Open;
    if SQLQueryRenameEvent.FieldByName('zahl').AsInteger > 0 then
    begin
      oldfound := True;
      reavtivate := False;
    end;

    if oldfound then
    begin
      DataModule1.debugOut(5,
        'dataedit: ButtonReactivateClick: Warning: existing datasets.');
      if MessageDlg('Warnung',
        'Altes Event hat noch abhängige Datensätze in uibevent.' +
        sLineBreak + 'Trotzdem reaktivieren ?', mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        reavtivate := True;
        DataModule1.debugOut(5,
          'dataedit: ButtonReactivateClick: Problem: existing datasets. : override by user');
      end
      else
      begin
        DataModule1.debugOut(5,
          'dataedit: ButtonReactivateClick: Problem: existing datasets. : cancled by user');
      end;
    end;

    if reavtivate then
    begin
      // disable trigger
      SQLQueryRenameEvent.SQL.Clear;
      SQLQueryRenameEvent.SQL.Add('ALTER TRIGGER TR_UIBAKTEVENT2UIBALL_IN INACTIVE');
      SQLQueryRenameEvent.ExecSQL;
      // create dataset
      SQLQueryRenameEvent.SQL.Clear;
      SQLQueryRenameEvent.SQL.Add('insert into uibaktevent (EVENT,PARENTEVENT,');
      SQLQueryRenameEvent.SQL.Add('COMMENT,ID_STRING,KD_String,TIME_H,PROJECTSTART,');
      SQLQueryRenameEvent.SQL.Add(
        'REPORTREQUIRED,ACC_PER_MONTHNUM,ACCOUNTINGREQUIRED) ');
      SQLQueryRenameEvent.SQL.Add('select "' + oldname + '", PARENTEVENT,"COMMENT",');
      SQLQueryRenameEvent.SQL.Add('ID_STRING,KD_String,TIME_H,PROJECTSTART,');
      SQLQueryRenameEvent.SQL.Add('REPORTREQUIRED,ACC_PER_MONTHNUM,ACCOUNTINGREQUIRED ');
      SQLQueryRenameEvent.SQL.Add('from uiballevent where event = :oldname');
      SQLQueryRenameEvent.ParamByName('oldname').AsString := oldname;
      SQLQueryRenameEvent.ExecSQL;


      DataModule1.debugOut(5, 'dataedit: ButtonReactivateClick: created in uibaktevent: '
        + oldname);
    end;
  finally
    // enable trigger
    SQLQueryRenameEvent.SQL.Clear;
    SQLQueryRenameEvent.SQL.Add('ALTER TRIGGER TR_UIBAKTEVENT2UIBALL_IN ACTIVE');
    SQLQueryRenameEvent.ExecSQL;
    screen.Cursor := crDefault;
    LabelProgress.Caption := 'Fertig.';
    DataModule1.debugOut(5, 'dataedit: ButtonReactivateClick: finished');
    Application.ProcessMessages;
  end;
end;

procedure TFDataedit.CheckBoxUseDateChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    DateTimePickerFromDate.Enabled := True
  else
    DateTimePickerFromDate.Enabled := False;
end;

procedure TFDataedit.ComboBoxOldAllEventEnter(Sender: TObject);
begin
  TComboBox(Sender).Text := '';
  TComboBox(Sender).Items.Clear;
  DataModule1.SQuiballevent.First;
  while not DataModule1.SQuiballevent.EOF do
  begin
    TComboBox(Sender).Items.Add(DataModule1.SQuiballevent.FieldByName('event').AsString);
    DataModule1.SQuiballevent.Next;
  end;
end;

procedure TFDataedit.ComboBoxOldEventEnter(Sender: TObject);
begin
  TComboBox(Sender).Text := '';
  TComboBox(Sender).Items.Clear;
  DataModule1.SQuibaktevent.First;
  while not DataModule1.SQuibaktevent.EOF do
  begin
    TComboBox(Sender).Items.Add(DataModule1.SQuibaktevent.FieldByName('event').AsString);
    DataModule1.SQuibaktevent.Next;
  end;
end;

procedure TFDataedit.DBGrid1EditingDone(Sender: TObject);
begin
  DataModule1.debugOut(5, 'start  ' + Sender.ClassName + ' DBGrid1EditingDone');
end;

procedure TFDataedit.DBGrid1FieldEditMask(Sender: TObject; const Field: TField;
  var Value: string);
begin

end;

procedure TFDataedit.DBGrid5DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
  if (gdFocused in State) then
  begin
    if (dbgrid5.SelectedField.FieldName = 'EVENT') then
    begin
      DBLookupComboBox2.DataField := dbgrid5.SelectedField.FieldName;
      DBLookupComboBox2.Left := Rect.Left + DBGrid5.Left;
      DBLookupComboBox2.Top := Rect.Top + DBGrid5.top;
      DBLookupComboBox2.Width := Rect.Right - Rect.Left;
      DBLookupComboBox2.Visible := True;
    end;
    if (DBGrid5.SelectedField.FieldName = DBComboBoxBool.DataField) then
    begin
      DBComboBoxBool.Left := Rect.Left + DBGrid5.Left;
      DBComboBoxBool.Top := Rect.Top + DBGrid5.top;
      DBComboBoxBool.Width := Rect.Right - Rect.Left;
      DBComboBoxBool.Visible := True;
    end;
  end;
end;

procedure TFDataedit.DBGrid6DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
  if (gdFocused in State) then
  begin
    if (DBGrid6.SelectedField.FieldName = DBLookupComboBox5.DataField) then
    begin
      DBLookupComboBox5.Left := Rect.Left + DBGrid6.Left;
      DBLookupComboBox5.Top := Rect.Top + DBGrid6.top;
      DBLookupComboBox5.Width := Rect.Right - Rect.Left;
      DBLookupComboBox5.Visible := True;
    end;
  end;
end;

procedure TFDataedit.DBGrid9DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
  if (gdFocused in State) then
  begin
    if (DBGrid9.SelectedField.FieldName = DBLookupComboBox4.DataField) then
    begin
      DBLookupComboBox4.Left := Rect.Left + DBGrid9.Left;
      DBLookupComboBox4.Top := Rect.Top + DBGrid9.top;
      DBLookupComboBox4.Width := Rect.Right - Rect.Left;
      DBLookupComboBox4.Visible := True;
    end;
  end;
end;

procedure TFDataedit.DBLookupComboBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  Handled := True;
  if PtInRect(TDBLookupCombobox(Sender).ReadBounds, mousepos) then
    Handled := False;
end;

procedure TFDataedit.FormCreate(Sender: TObject);
begin
  //TForm(sender).Font.Name:=myFont;
  DataModule1.configureLookupComboBox(DBLookupComboBox1);
  (*
 {$IFDEF LINUX}
  DBLookupComboBox1.AutoComplete:=true;
  DBLookupComboBox1.AutoDropDown:=false;
  DBLookupComboBox1.AutoSelect:=false;
  DBLookupComboBox1.ReadOnly:=false;
  DBLookupComboBox1.Style:= csDropDown;
  DBLookupComboBox1.Sorted:=false;
  {$ENDIF LINUX}
   *)
  DataModule1.SetFontName(TControl(sender),myFont);
end;

procedure TFDataedit.Label6Click(Sender: TObject);
begin

end;

procedure TFDataedit.FormActivate(Sender: TObject);
begin
  datamodule1.debugOut(5, 'Activate FDataedit');
  // Datamodule1.TimerOnTop.Enabled := false;
end;

procedure TFDataedit.DBGrid1Enter(Sender: TObject);
begin
  DBGrid1.repaint;
end;

procedure TFDataedit.DBLookupComboBox1Exit(Sender: TObject);
begin
  DBGrid1.repaint;
end;

procedure TFDataedit.DBGrid1ColExit(Sender: TObject);
begin
  if DBGrid1.SelectedField.FieldName = DBLookupComboBox1.DataField then
    DBLookupComboBox1.Visible := False;
end;

// Ersetzt dbgriddrawdatacell das nicht mehr funktionierte
// seit bei 3.1 es hier nicht gezeigte columns gibt
procedure TFDataedit.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
  if (gdFocused in State) then
  begin
    if (dbgrid1.SelectedField.FieldName = DBLookupComboBox1.DataField) then
    begin
      DBLookupComboBox1.Left := Rect.Left + DBGrid1.Left;
      DBLookupComboBox1.Top := Rect.Top + DBGrid1.top;
      DBLookupComboBox1.Width := Rect.Right - Rect.Left;
      DBLookupComboBox1.Visible := True;
    end;
  end;
end;

(*++++++++++++++++*)
procedure TFDataedit.DBGrid3Enter(Sender: TObject);
begin
  DBGrid3.repaint;
end;


procedure TFDataedit.DBLookupComboBox3Exit(Sender: TObject);
begin
  DBGrid3.repaint;
end;

procedure TFDataedit.DBGrid3ColExit(Sender: TObject);
begin
  if DBGrid3.SelectedField.FieldName = DBLookupComboBox3.DataField then
    DBLookupComboBox3.Visible := False;
end;


procedure TFDataedit.DBGrid5Enter(Sender: TObject);
begin
  DBGrid5.repaint;
end;


procedure TFDataedit.DBLookupComboBox2Exit(Sender: TObject);
begin
  DBGrid5.repaint;
end;

procedure TFDataedit.DBGrid5ColExit(Sender: TObject);
begin
  if DBGrid5.SelectedField.FieldName = DBLookupComboBox2.DataField then
    DBLookupComboBox2.Visible := False;
  if DBGrid5.SelectedField.FieldName = DBComboBoxBool.DataField then
    DBComboBoxBool.Visible := False;
end;

procedure TFDataedit.DBComboBoxBoolExit(Sender: TObject);
begin
  DBGrid5.repaint;
end;

(*++++++++++++++++*)




procedure TFDataedit.BtnMultiDayClick(Sender: TObject);
begin
  FMultiday := TFMultiday.Create(self);
  FMultiday.showmodal;
  FMultiday.Free;
end;


procedure TFDataedit.FormHide(Sender: TObject);
begin
  datamodule1.debugOut(5, 'Hide FDataedit');
  // Datamodule1.TimerOnTop.Enabled := true;
end;

procedure TFDataedit.DBImage1Exit(Sender: TObject);
begin
  DBGrid4.repaint;
end;

(*
procedure TFDataedit.BtnSearchClick(Sender: TObject);
begin
 if OpenPictureDialog1.Execute then
 begin
  //DataModule1.SQuiballevent.Edit;
  Image1.Picture.LoadFromFile(OpenPictureDialog1.filename);
  Clipboard.assign(Image1.Picture.Bitmap);
  DBImage1.PasteFromClipboard;
 end;
end;
*)

procedure TFDataedit.BtnDuplicateClick(Sender: TObject);
var
  userid: string;
  jahr, monat: integer;
  stunden: double;
begin

  if DataModule1.SQuibsoll.State in [dsInsert, dsEdit] then
  begin
    DataModule1.SQuibsoll.Post;
    //DataModule1.SQuibsoll.ApplyUpdates;
  end;
  DataModule1.SQuibsoll.Last;
  userid := DataModule1.SQuibsoll.FieldByName('userid').AsString;
  jahr := DataModule1.SQuibsoll.FieldByName('jahr').AsInteger;
  monat := DataModule1.SQuibsoll.FieldByName('monat').AsInteger;
  stunden := DataModule1.SQuibsoll.FieldByName('stunden').AsFloat;
  Inc(monat);
  if monat > 12 then
  begin
    monat := 1;
    Inc(jahr);
  end;
  DataModule1.SQuibsoll.Append;
  DataModule1.SQuibsoll.FieldByName('userid').AsString := userid;
  DataModule1.SQuibsoll.FieldByName('jahr').AsInteger := jahr;
  DataModule1.SQuibsoll.FieldByName('monat').AsInteger := monat;
  DataModule1.SQuibsoll.FieldByName('stunden').AsFloat := stunden;
  DataModule1.SQuibsoll.post;
  //DataModule1.SQuibsoll.ApplyUpdates;
end;



procedure TFDataedit.DBLookupComboBox3Enter(Sender: TObject);
begin
  DataModule1.SQQueryAktEvents.Active := False;
  DataModule1.SQQueryAktEvents.Active := True;
end;

procedure TFDataedit.DBGrid9ColExit(Sender: TObject);
begin
  if DBGrid9.SelectedField.FieldName = DBLookupComboBox4.DataField then
    DBLookupComboBox4.Visible := False;
end;

procedure TFDataedit.DBLookupComboBox4Exit(Sender: TObject);
begin
  DBGrid9.repaint;
end;

procedure TFDataedit.DBGrid9Enter(Sender: TObject);
begin
  DBGrid9.repaint;
end;

procedure TFDataedit.DBLookupComboBox5Exit(Sender: TObject);
begin
  DBGrid6.repaint;
end;

procedure TFDataedit.DBGrid6ColExit(Sender: TObject);
begin
  if DBGrid6.SelectedField.FieldName = DBLookupComboBox5.DataField then
    DBLookupComboBox5.Visible := False;
end;

procedure TFDataedit.DBGrid6Enter(Sender: TObject);
begin
  DBGrid6.repaint;
end;

procedure TFDataedit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DataModule1.Dateneditieren1Cancel;
end;

procedure TFDataedit.BitBtn2Click(Sender: TObject);
begin
  hide;
  Close;
end;

procedure TFDataedit.TabSheet1Show(Sender: TObject);
begin
  datamodule1.debugOut(5, 'TabSheet1Show');
  if not datamodule1.SQuibaktevent.Active then
  begin
    datamodule1.SQuibaktevent.Open;
    datamodule1.debugOut(5, 'TabSheet1Show: opend query');
  end;
  if not DataModule1.SQuibevent.Active then
    DataModule1.SQuibevent.Open;
end;

procedure TFDataedit.TabSheet2Show(Sender: TObject);
begin
  datamodule1.debugOut(5, 'TabSheet2Show');
  if not datamodule1.SQuibaktuser.Active then
  begin
    datamodule1.SQuibaktuser.Open;
    datamodule1.debugOut(5, 'TabSheet2Show: opend query');
  end;
end;

procedure TFDataedit.TabSheet3Show(Sender: TObject);
begin
  datamodule1.debugOut(5, 'TabSheet3Show');
  if not datamodule1.SQuibaktevent.Active then
  begin
    datamodule1.SQuibaktevent.Open;
    datamodule1.debugOut(5, 'TabSheet3Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet4Show(Sender: TObject);
begin
  datamodule1.debugOut(5, 'TabSheet4Show');
  if not datamodule1.SQuiballevent.Active then
  begin
    datamodule1.SQuiballevent.Open;
    datamodule1.debugOut(5, 'TabSheet4Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet5Show(Sender: TObject);
begin
  datamodule1.debugOut(5, 'TabSheet5Show');
  if not DataModule1.SQuibuserevent.Active then
  begin
    DataModule1.SQuibuserevent.Open;
    datamodule1.debugOut(5, 'TabSheet5Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet6Show(Sender: TObject);
begin
  datamodule1.debugOut(5, 'TabSheet6Show');
  if not DataModule1.SQuibtimeout.Active then
  begin
    DataModule1.SQuibtimeout.Open;
    datamodule1.debugOut(5, 'TabSheet6Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet7Show(Sender: TObject);
begin
  DateTimePickerFromDate.Date := Date;
end;


procedure TFDataedit.TabSheet8Show(Sender: TObject);
begin
  datamodule1.debugOut(5, 'TabSheet8Show');
  if not DataModule1.SQuibsoll.Active then
  begin
    DataModule1.SQuibsoll.Open;
    datamodule1.debugOut(5, 'TabSheet8Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet9Show(Sender: TObject);
begin
  datamodule1.debugOut(5, 'TabSheet9Show');
  if not DataModule1.SQuibdefproj.Active then
  begin
    DataModule1.SQuibdefproj.Open;
    datamodule1.debugOut(5, 'TabSheet9Show: opend query');
  end;
end;

(*
procedure TFDataedit.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
  Application.ShowException(E);
  DumpExceptionCallStack(E);
  datamodule1.debugOut(1, 'Exception', 'Exception dumped - terminating');
  //Application.Terminate;
end;
*)
end.
