unit rtest1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, IBConnection, FileUtil, LR_Class, LR_DBSet,
  LR_View, LR_PGrid, Forms, Controls, Graphics, Dialogs, StdCtrls, printers,
  DBGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    frDBDataSet1: TfrDBDataSet;
    FrPrintGrid1: TFrPrintGrid;
    frReport1: TfrReport;
    IBConnection1: TIBConnection;
    SQLTransaction1: TSQLTransaction;
    SQuibaktuser: TSQLQuery;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { private declarations }
    Pages: TfrPages;
    Page: TfrPageReport;
    Band: TfrBand;
    Bandview: TfrBandView;
    Memo: TfrMemoView;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}




{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  { clear a report }
  frReport1.Clear;
  { add a dataset to the list of ones accessible for a report }
  //frReport1.DataSets.Add(frDBDataSet1);
  frReport1.Dataset :=   frDBDataSet1;
  { add the "Data" page }
  Pages := TfrPages.Create(frReport1);
  { add a page }
  Page := TfrPageReport.Create(nil);
  Page.CreatePage;
  page.CreateUniqueName;
  pages.Add(Page.ClassName);
  { create a unique name }
  //Page.CreateUniqueName;

  { set sizes of fields, paper and orientation by default }
  //Page.SetDefaults;
  //Page.Clear;
  { modify paper’s orientation }
  Page.Orientation := poLandscape;
  Bandview :=  TfrBandView.Create(Page);
  Bandview.BandType:=btMasterData;

  { add a report title band}
  ///Band := TfrReportTitle.Create(Page);
  Band := TfrBand.Create(btReportTitle, Page);
  //Band.CreateUniqueName;
  { it is sufficient to set the «Top» coordinate and height for a band }
  { both coordinates are in pixels }
  Band.Top := 0;
  Band.Height := 20;
  { add an object to the report title band }
  Band.Memo.Text:='Hello FastReport!';

  //Memo := TfrMemoView.Create(Band);
  Memo := TfrMemoView.Create(Page);
  //Memo := TfrMemoStrings.Create;
  //Band.Memo.CreateUniqueName;
  //Memo.Text := 'Hello FastReport!';
  //Memo.memo.Text := 'Hello FastReport!';
  //Memo.Height := 20;
  { this object will be stretched according to band’s width }
  //Memo.Align := baWidth;

  { add the masterdata band }
  ///DataBand := TfrMasterData.Create(Page);

  { the Top coordinate should be greater than the previously added band’s top + height}
  //DataBand.Top := 100;
  //DataBand.Height := 20;
  { add an object on master data }
  //Memo := TfrMemoView.Create(DataBand);
  //Memo.CreateUniqueName;
  { connect to data }
  //Memo.DataSet := frDBDataSet1;
  //DataBand.;
  //Memo.DataField := 'userid';
  //Memo.SetBounds(0, 0, 100, 20);
  { adjust the text to the right object’s margin }
  //Memo.HAlign := haRight;
  { show the report }
  frReport1.SaveToFile('rtest2.lrf');
  frReport1.ShowReport;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  frReport1.LoadFromFile('rtest.lrf');
  frReport1.;
  frReport1.ShowReport;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FrPrintGrid1.PreviewReport;
end;



end.

