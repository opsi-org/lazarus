unit dataedit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, {LMessages, Messages,} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {StdCtrls,} Buttons, Grids, DBGrids, ExtCtrls, DBCtrls, {ToolWin,} ComCtrls,
  DB, {DBCGrids, ExtDlgs,} clipbrd, types;

type

  { TFDataedit }

  TFDataedit = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ToolBar1: TToolBar;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    TabSheet2: TTabSheet;
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
    procedure DBGrid3DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid5DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid6DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid9DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
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
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
    procedure TabSheet4Show(Sender: TObject);
    procedure TabSheet5Show(Sender: TObject);
    procedure TabSheet6Show(Sender: TObject);
    procedure TabSheet8ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
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
 DataModule1.SQQueryAktEvents.Active := false;
 DataModule1.SQQueryAktEvents.Active := true;
end;

procedure TFDataedit.DBGrid3DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
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

procedure TFDataedit.DBGrid5DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
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
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
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
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
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

procedure TFDataedit.FormActivate(Sender: TObject);
begin
 datamodule1.debugOut(5,'Activate FDataedit');
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
 If DBGrid1.SelectedField.FieldName = DBLookupComboBox1.DataField then
    DBLookupComboBox1.Visible := false;
end;

// Ersetzt dbgriddrawdatacell das nicht mehr funktionierte
// seit bei 3.1 es hier nicht gezeigte columns gibt
procedure TFDataedit.DBGrid1DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
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
 If DBGrid3.SelectedField.FieldName = DBLookupComboBox3.DataField then
    DBLookupComboBox3.Visible := false;
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
 If DBGrid5.SelectedField.FieldName = DBLookupComboBox2.DataField then
    DBLookupComboBox2.Visible := false;
 If DBGrid5.SelectedField.FieldName = DBComboBoxBool.DataField then
    DBComboBoxBool.Visible := false;
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
 datamodule1.debugOut(5,'Hide FDataedit');
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
 userid : string;
 jahr, monat : integer;
 stunden : double;
begin
 //
 if DataModule1.SQuibsoll.State in [dsInsert, dsEdit] then
 begin
  DataModule1.SQuibsoll.Post;
  //DataModule1.SQuibsoll.ApplyUpdates;
 end;
 DataModule1.SQuibsoll.Last;
 userid := DataModule1.SQuibsoll.fieldbyname('userid').asstring;
 jahr := DataModule1.SQuibsoll.fieldbyname('jahr').asinteger;
 monat := DataModule1.SQuibsoll.fieldbyname('monat').asinteger;
 stunden := DataModule1.SQuibsoll.fieldbyname('stunden').asfloat;
 inc(monat);
 if monat > 12 then
 begin
  monat := 1;
  inc(jahr);
 end;
 DataModule1.SQuibsoll.Append;
 DataModule1.SQuibsoll.fieldbyname('userid').asstring := userid;
 DataModule1.SQuibsoll.fieldbyname('jahr').asinteger := jahr;
 DataModule1.SQuibsoll.fieldbyname('monat').asinteger := monat;
 DataModule1.SQuibsoll.fieldbyname('stunden').asfloat := stunden;
 DataModule1.SQuibsoll.post;
 //DataModule1.SQuibsoll.ApplyUpdates;
end;



procedure TFDataedit.DBLookupComboBox3Enter(Sender: TObject);
begin
 DataModule1.SQQueryAktEvents.Active := false;
 DataModule1.SQQueryAktEvents.Active := true;
end;

procedure TFDataedit.DBGrid9ColExit(Sender: TObject);
begin
 If DBGrid9.SelectedField.FieldName = DBLookupComboBox4.DataField then
    DBLookupComboBox4.Visible := false;
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
 If DBGrid6.SelectedField.FieldName = DBLookupComboBox5.DataField then
    DBLookupComboBox5.Visible := false;
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
 close;
end;

procedure TFDataedit.TabSheet1Show(Sender: TObject);
begin
  datamodule1.debugOut(5,'TabSheet1Show');
  if not datamodule1.SQuibaktevent.Active then
  begin
    datamodule1.SQuibaktevent.Open;
    datamodule1.debugOut(5,'TabSheet1Show: opend query');
  end;
  if not DataModule1.SQuibevent.Active then DataModule1.SQuibevent.open;
end;

procedure TFDataedit.TabSheet2Show(Sender: TObject);
begin
  datamodule1.debugOut(5,'TabSheet2Show');
  if not datamodule1.SQuibaktuser.Active then
  begin
    datamodule1.SQuibaktuser.Open;
    datamodule1.debugOut(5,'TabSheet2Show: opend query');
  end;
end;

procedure TFDataedit.TabSheet3Show(Sender: TObject);
begin
    datamodule1.debugOut(5,'TabSheet3Show');
  if not datamodule1.SQuibaktevent.Active then
  begin
    datamodule1.SQuibaktevent.Open;
    datamodule1.debugOut(5,'TabSheet3Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet4Show(Sender: TObject);
begin
    datamodule1.debugOut(5,'TabSheet4Show');
  if not datamodule1.SQuiballevent.Active then
  begin
    datamodule1.SQuiballevent.Open;
    datamodule1.debugOut(5,'TabSheet4Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet5Show(Sender: TObject);
begin
  datamodule1.debugOut(5,'TabSheet5Show');
  if not DataModule1.SQuibuserevent.Active then
  begin
    DataModule1.SQuibuserevent.Open;
    datamodule1.debugOut(5,'TabSheet5Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet6Show(Sender: TObject);
begin
  datamodule1.debugOut(5,'TabSheet6Show');
  if not DataModule1.SQuibtimeout.Active then
  begin
    DataModule1.SQuibtimeout.Open;
    datamodule1.debugOut(5,'TabSheet6Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet8ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TFDataedit.TabSheet8Show(Sender: TObject);
begin
  datamodule1.debugOut(5,'TabSheet8Show');
  if not DataModule1.SQuibsoll.Active then
  begin
    DataModule1.SQuibsoll.Open;
    datamodule1.debugOut(5,'TabSheet8Show: opend query');
  end;

end;

procedure TFDataedit.TabSheet9Show(Sender: TObject);
begin
  datamodule1.debugOut(5,'TabSheet9Show');
  if not DataModule1.SQuibdefproj.Active then
  begin
    DataModule1.SQuibdefproj.Open;
    datamodule1.debugOut(5,'TabSheet9Show: opend query');
  end;

end;

end.



