unit work_description;

{$MODE Delphi}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF WINDOWS}
  Messages,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, sqldb, ExtCtrls,
  //Grids,
  DBGrids, DBCtrls, StdCtrls,
  {ComCtrls,} DateUtils, Buttons, ExtDlgs, EditBtn{, Types};

type

  { Tfwork_description }

  Tfwork_description = class(TForm)
    CalendarDialog1: TCalendarDialog;
    EditButtonDate: TEditButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    DBNavigator1: TDBNavigator;
    DBGrid_day_report: TDBGrid;
    Splitter1: TSplitter;
    Label3: TLabel;
    Label4: TLabel;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    BitBtn1: TBitBtn;
    procedure BtnLoadresultsClick(Sender: TObject);
    procedure DBGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure DBGrid1KeyPress(Sender: TObject; var Key: char);
    procedure EditButtonDateButtonClick(Sender: TObject);
    procedure EditButtonDateChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SQwork_descriptionAfterInsert(DataSet: TDataSet);
    procedure BitBtn1Click(Sender: TObject);
    procedure Loadresults4date;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fwork_description: Tfwork_description;

implementation

uses uibdata;

{$R *.lfm}

procedure Tfwork_description.Loadresults4date;
begin
  with DataModule1 do
  begin
    if Query_day_report.Active then
      Query_day_report.Close;
    Query_day_report.ParamByName('userid').AsString := uid;
    Query_day_report.ParamByName('jahr').AsInteger :=
      YearOf(StrToDate(EditButtonDate.text));
    Query_day_report.ParamByName('monat').AsInteger :=
      MonthOf(StrToDate(EditButtonDate.text));
    Query_day_report.ParamByName('tag').AsInteger :=
      DayOf(StrToDate(EditButtonDate.text));
    Query_day_report.Open;
  end;
end;

procedure Tfwork_description.BtnLoadresultsClick(Sender: TObject);
begin

end;

procedure Tfwork_description.DBGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {$IFDEF WINDOWS}
  if Key in [VK_INSERT] then Key := 0;
  {$ENDIF WINDOWS}
end;


procedure Tfwork_description.DBGrid1KeyPress(Sender: TObject; var Key: char);
begin
  //if Key in [VK_INSERT] then Key := #0;
end;

procedure Tfwork_description.EditButtonDateButtonClick(Sender: TObject);
begin
  CalendarDialog1.Date:=now;
  CalendarDialog1.Execute;
  EditButtonDate.text := DateToStr(CalendarDialog1.Date);
  Loadresults4date;
end;

procedure Tfwork_description.EditButtonDateChange(Sender: TObject);
begin

end;

procedure Tfwork_description.FormActivate(Sender: TObject);
begin
  with DataModule1 do
  begin
    SQwork_description.ParamByName('userid').AsString := uid;
    SQwork_description.Open;
    SQwork_description.Last;
    Loadresults4date;
  end;
  //FormStyle:=fsNormal;
end;


procedure Tfwork_description.FormShow(Sender: TObject);
begin
  CalendarDialog1.Date:=now;
  EditButtonDate.text := DateToStr(Date);
  //FormStyle:=fsNormal;
end;




procedure Tfwork_description.SQwork_descriptionAfterInsert(DataSet: TDataSet);
begin
  with DataModule1 do
  begin
    SQwork_description.FieldByName('userid').AsString := uid;
    SQwork_description.FieldByName('jahr').AsInteger :=
      YearOf(StrToDate(EditButtonDate.text));
    SQwork_description.FieldByName('monat').AsInteger :=
      MonthOf(StrToDate(EditButtonDate.text));
    SQwork_description.FieldByName('tag').AsInteger :=
      DayOf(StrToDate(EditButtonDate.text));
    SQwork_description.FieldByName('event').AsString :=
      Query_day_report.FieldByName('event').AsString;
  end;
end;

procedure Tfwork_description.BitBtn1Click(Sender: TObject);
begin
  hide;
  //Close;
end;

end.
