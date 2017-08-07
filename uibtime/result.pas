{ Visual Querybuilder
  Delphi 1.02
  (c) by umweltinformatik büro dr. detlef oertel (uib)
  under General Public Licence
  www.uib.de
}

unit result;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, LCLType,
  //LMessages, Messages,
  Classes, Graphics, Controls,
  Forms, Dialogs, DB, DBCtrls, Grids, DBGrids, ExtCtrls, Buttons, StdCtrls,
  ///rlgencol, rlgenrep,
  printers,
  //vq8qrgen, vq8fprev, Spin,
  sqldb, fpcsvexport,
  ///expo2,
  Spin, LR_PGrid,
  //LR_E_CSV,
  LR_DBSet,
  //fpdataexporter,
  lr_e_pdf;

type

  { TFResult }

  TFResult = class(TForm)
    CSVExporter1: TCSVExporter;
    frDBDataSet1: TfrDBDataSet;
    FrPrintGrid1: TFrPrintGrid;
    frTNPDFExport1: TfrTNPDFExport;
    Label5: TLabel;
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    Btnclose: TBitBtn;
    BtnPrint: TSpeedButton;
    BtnPreview: TSpeedButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    Label4: TLabel;
    Label9: TLabel;
    BtnDBexp: TBitBtn;
    SpinEdit1: TSpinEdit;
    //SQLQueryResult: TSQLQuery;
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure FormCreate(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
    procedure BtnPrintClick(Sender: TObject);
    procedure ShowPreview;
    ///procedure CreateFQReportGen;
    procedure FormShow(Sender: TObject);
    procedure BtnDBexpClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    QueryResult: TSQLQuery;
  end;

var
  FResult: TFResult;


implementation

uses uibdata;
///uses statistik, rledcol, expo;

{$R *.lfm}
//var
 //header  : string;
 //PVResult : TModalResult;

procedure TFResult.FormCreate(Sender: TObject);
begin
end;

procedure TFResult.DBGrid1TitleClick(Column: TColumn);
begin
  // http://delphiexamples.com/databases/sortgrid.html
  (Column.Field.DataSet as TSQLQuery).IndexFieldNames:=Column.FieldName;
end;

procedure TFResult.ShowPreview;
begin
end;

procedure TFResult.BtnPreviewClick(Sender: TObject);
begin
 FrPrintGrid1.DBGrid := DBGrid1;
 FrPrintGrid1.ShowCaption:=true;
 FrPrintGrid1.Caption:=Edit1.Caption;
 //FrPrintGrid1.Template:='gridtemplate.lrf';
 FrPrintGrid1.Font.Size:=Spinedit1.value;
 if RadioGroup1.ItemIndex = 0 then FrPrintGrid1.Orientation:= poPortrait
 else FrPrintGrid1.Orientation:= poLandscape;
 FrPrintGrid1.PreviewReport;
end;



procedure TFResult.BtnPrintClick(Sender: TObject);
begin

end;

procedure TFResult.FormShow(Sender: TObject);
begin
  //Label2.caption := inttostr(Datasource1.Dataset.RecordCount);
  Label2.caption := inttostr(DataModule1.Query4Result.RecordCount);
  //DataSource1.DataSet := DataModule1.Query4Result;
 //Label2.caption := inttostr(frDBDataSet1..);
  //Listbox1.Items.clear;
  //Datasource1.Dataset.GetFieldNames(Listbox1.Items);
  //DataModule1.Query4Result.GetFieldNames(Listbox1.Items);
  if DataModule1.Query4Result.ReadOnly then
  begin
    //DBNavigator1.VisibleButtons:= [nbFirst, nbPrior, nbNext, nbLast];
    DBGrid1.AutoEdit:=false;
  end
  else
  begin
    DBGrid1.AutoEdit:=true;
    //DBNavigator1.VisibleButtons:= [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    // nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  end;
end;

procedure TFResult.BtnDBexpClick(Sender: TObject);
//var
// iresult : integer;
begin
  if SaveDialog1.Execute then
  begin
    CSVExporter1.FileName:=SaveDialog1.FileName;
    CSVExporter1.Dataset := Datasource1.Dataset;
    Datasource1.Dataset.First;
    //CSVExporter1.Dataset := FStatistik.query1;
    //FStatistik.query1.First;
    //iresult :=
    CSVExporter1.Execute;
  end;
 (*
 FExport2 := TFExport2.Create(self);
 FExport2.ShowModal;
 FExport2.Free
 *)
end;

end.
