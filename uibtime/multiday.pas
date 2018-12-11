unit multiday;

{$MODE Delphi}

interface

uses
//{$IFDEF WINDOWS}
//  Windows,
//     {$ENDIF WINDOWS}
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
//  ExtCtrls, Grids, Calendar, ComCtrls, DBCtrls, maskedit, ExtDlgs, EditBtn,
  ExtCtrls, Calendar, DBCtrls, maskedit, ExtDlgs, EditBtn, DateTimePicker,
  dateutils,
  uibdatetime,
  strutils;

type

  { TFMultiday }

  TFMultiday = class(TForm)
    CheckBoxHolydays: TCheckBox;
    CheckBoxOnlyWorkdays: TCheckBox;
    DateTimePickerStart: TDateTimePicker;
    DateTimePickerEnd: TDateTimePicker;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OKBtn: TButton;
    cal: TCalendar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MaskEditStart: TMaskEdit;
    MaskEditStunden: TMaskEdit;
    BtnInsertAll: TBitBtn;
    ComboBoxEvent: TComboBox;
    procedure BtnInsertAllClick(Sender: TObject);
    procedure CalendarDialog1Show(Sender: TObject);
    procedure ComboBoxEventSelect(Sender: TObject);
    procedure EditButtonEndDateButtonClick(Sender: TObject);
    procedure EditButtonStartDateButtonClick(Sender: TObject);
    procedure EditButtonStartDateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMultiday: TFMultiday;

implementation

{$R *.lfm}
uses uibdata;

procedure TFMultiday.BtnInsertAllClick(Sender: TObject);
var
  firstdate, aktdate: tdate;
  startzeit, hoursperday: TTime;
  doinsert : boolean;
begin
  OKBtn.Enabled:=false;
  //firstdate := ScanDateTime('dd.mm.yyyy',EditButtonStartDate.text);
  firstdate := DateTimePickerStart.Date;
  aktdate := firstdate - 1;
  if MaskEditStart.Text = '' then
    startzeit := strtotime('10:00')
  else
      startzeit := strtotime(MaskEditStart.Text);
  hoursperday := strtotime(MaskEditStunden.Text);
  Datamodule1.SQuibevent.First;

  repeat
    doinsert := true;
    aktdate := aktdate + 1;
    if CheckBoxOnlyWorkdays.Checked and not(DayOfTheWeek(aktdate) in user_work_days) then doinsert := false;
    if doinsert then
    begin
      Datamodule1.SQuibevent.insert;
      if CheckBoxHolydays.Checked and Datamodule1.dateIsHolyday(aktdate) then
      begin
        Datamodule1.SQuibevent.FieldByName('event').AsString := 'Feiertag';
        Datamodule1.SQuibevent.FieldByName('stoptime').AsDateTime :=
        aktdate + startzeit + strtotime(timeFloatTohourminutesStr(user_h_per_day));
      end
      else
      begin
        Datamodule1.SQuibevent.FieldByName('event').AsString := ComboBoxEvent.Text;
        Datamodule1.SQuibevent.FieldByName('stoptime').AsDateTime :=
        aktdate + startzeit + hoursperday;
      end;
      Datamodule1.SQuibevent.FieldByName('starttime').AsDateTime := aktdate + startzeit;
      Datamodule1.SQuibevent.Post;
      //DataModule1.SQuibevent.ApplyUpdates;
      sleep(1000);
    end;
  //until (ScanDateTime('dd.mm.yyyy',EditButtonEndDate.text) = aktdate);
  until (DateTimePickerEnd.Date = aktdate);
  OKBtn.Enabled:=true;
end;

procedure TFMultiday.CalendarDialog1Show(Sender: TObject);
begin

end;


procedure TFMultiday.ComboBoxEventSelect(Sender: TObject);
var
  event,str : string;
  i : integer;
begin
  i := ComboBoxEvent.ItemIndex;
  event := ComboBoxEvent.Items[i];
  if MatchStr(event,['Krank','Feiertag','Urlaub']) then
  begin
    MaskEditStart.EditText:='10:00';
    str := timeFloatTohourminutesStr(user_h_per_day);
    MaskEditStunden.EditText:= str;
  end;
  if lowerCase(event) =  lowerCase('Gleittag') then
  begin
    MaskEditStart.EditText:='10:00';
    MaskEditStunden.EditText:='00:00';
  end;
end;

procedure TFMultiday.EditButtonEndDateButtonClick(Sender: TObject);
begin

end;


procedure TFMultiday.EditButtonStartDateButtonClick(Sender: TObject);
var
  str : string;
begin
  (*
  CalendarDialog1.Date:=ScanDateTime('dd.mm.yyyy',EditButtonEndDate.text);
  CalendarDialog1.Execute;
  EditButtonStartDate.text := DateToStr(CalendarDialog1.Date);
  EditButtonEndDate.text := EditButtonStartDate.text;
  *)
  DateTimePickerEnd.Date:= DateTimePickerStart.Date;
  if Datamodule1.dateIsHolyday(DateTimePickerStart.Date) then
  begin
    MaskEditStart.EditText:='10:00';
    str := timeFloatTohourminutesStr(user_h_per_day);
    MaskEditStunden.EditText:= str;
    ComboBoxEvent.ItemIndex:=-1;
    ComboBoxEvent.SelText:='Feiertag';
  end;
end;

procedure TFMultiday.EditButtonStartDateChange(Sender: TObject);
begin

end;

procedure TFMultiday.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  ComboBoxEvent.AutoDropDown:=false;
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  ComboBoxEvent.AutoDropDown:=true;
  {$ENDIF WINDOWS}
  DateTimePickerStart.Date:=now;
  DateTimePickerEnd.Date:=now;
end;

procedure TFMultiday.FormShow(Sender: TObject);
begin
 (*
 cal.MaxSelectRange := 3000;
 cal.EndDate := dateutils.Tomorrow;
 cal.Date :=dateutils.Today;
 cal.MaxSelectRange := 31;
 *)
  ComboBoxEvent.Items.Clear;
  Datamodule1.SQQueryAktEvents.First;
  while not Datamodule1.SQQueryAktEvents.EOF do
  begin
    ComboBoxEvent.Items.Add(Datamodule1.SQQueryAktEvents.FieldByName('event').AsString);
    Datamodule1.SQQueryAktEvents.Next;
  end;
  (*
  EditButtonStartDate.text := DateToStr(Date);
  EditButtonEndDate.text := DateToStr(Date);
  *)
end;

procedure TFMultiday.Label1Click(Sender: TObject);
begin

end;

end.
