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
  Variants,
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
    ComboBoxSearchfield: TComboBox;
    CSVExporter1: TCSVExporter;
    EditSearch: TEdit;
    frDBDataSet1: TfrDBDataSet;
    FrPrintGrid1: TFrPrintGrid;
    frTNPDFExport1: TfrTNPDFExport;
    ImageViewmag: TImage;
    Label5: TLabel;
    Label6: TLabel;
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
    PanelSearch: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroupSearchOption: TRadioGroup;
    SaveDialog1: TSaveDialog;
    Label4: TLabel;
    Label9: TLabel;
    BtnDBexp: TBitBtn;
    SpeedButtonClearSearchEdit: TSpeedButton;
    SpinEdit1: TSpinEdit;
    //SQLQueryResult: TSQLQuery;
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure EditSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
    procedure BtnPrintClick(Sender: TObject);
    procedure ShowPreview;
    ///procedure CreateFQReportGen;
    procedure FormShow(Sender: TObject);
    procedure BtnDBexpClick(Sender: TObject);
    procedure SpeedButtonClearSearchEditClick(Sender: TObject);
    procedure FilterInResult;
    procedure LocateInResult;
    procedure setResultDataset(QueryResult: TSQLQuery);

  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    resultdataset: TSQLQuery;
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

procedure TFResult.EditSearchChange(Sender: TObject);
begin
  if EditSearch.Text <> '' then
  begin
    if RadioGroupSearchOption.ItemIndex = 0 then LocateInResult
    else FilterInResult;
  end
  else
  begin
    resultDataset.Filtered := False;
    resultDataset.First;
  end;
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
var
  i : integer;
begin
  Label2.caption := inttostr(resultDataset.RecordCount);
  DataSource1.DataSet := resultDataset;
  if resultDataset.ReadOnly then
  begin
    DBGrid1.AutoEdit:=false;
  end
  else
  begin
    DBGrid1.AutoEdit:=true;
  end;
  ComboBoxSearchfield.Clear;
  for i := 0 to resultdataset.Fields.Count -1 do
   ComboBoxSearchfield.Items.Add(resultDataset.Fields[i].FieldName);
  ComboBoxSearchfield.ItemIndex:=0;


end;

procedure TFResult.BtnDBexpClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    CSVExporter1.FileName:=SaveDialog1.FileName;
    CSVExporter1.Dataset := Datasource1.Dataset;
    Datasource1.Dataset.First;
    CSVExporter1.Execute;
  end;
end;

procedure TFResult.FilterInResult;
var
  //i:integer;
  Filtercond, Filterstr: string;
begin
  try
    Filtercond := '"*' + EditSearch.Text + '*"';
    //LogDatei.log('Search for: ' + EditSearch.Text + ' Filter for: ' +
    //  Filtercond, LLinfo);
    Filterstr := '('+ ComboBoxSearchfield.Caption+' = ' + Filtercond+')';
    (*
    Filterstr := '('+ resultDataset.Fields[0].FieldName+' =' + Filtercond
             + 'or '+ resultDataset.Fields[1].FieldName+' =' + Filtercond
             + 'or '+ resultDataset.Fields[2].FieldName+' =' + Filtercond
             + 'or '+ resultDataset.Fields[3].FieldName+' =' + Filtercond;
             *)
    resultDataset.Filtered := False;
    resultDataset.Filter := Filterstr;
    resultDataset.FilterOptions := [foCaseInsensitive];
    resultDataset.Filtered := True;
    resultDataset.First;
  finally
  end;

end;

procedure TFResult.LocateInResult;
//var
  //i:integer;
//  Filtercond, Filterstr, keyfields: string;
begin
  try
    //Filtercond := '"*' + EditSearch.Text + '*"';
    //LogDatei.log('Search for: ' + EditSearch.Text + ' Filter for: ' +
    //  Filtercond, LLinfo);
    (*
    keyfields := resultDataset.Fields[0].FieldName+';' +
                 resultDataset.Fields[1].FieldName+';' +
                 resultDataset.Fields[2].FieldName+';' +
                 resultDataset.Fields[3].FieldName;
    *)
    resultDataset.Filtered := False;
    resultDataset.Locate(ComboBoxSearchfield.Caption, EditSearch.Text, [loCaseInsensitive,loPartialKey]);
    (*
                                    VarArrayOf([Filtercond, Filtercond,Filtercond,Filtercond]),
                                    [loCaseInsensitive,loPartialKey]);
                                    *)
  finally
  end;
end;



procedure TFResult.SpeedButtonClearSearchEditClick(Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    EditSearch.Clear;
    resultDataset.Filtered := False;
    resultDataset.First;
    //SpeedButtonAll.AllowAllUp:= False;
    //SpeedButtonAll.Click;
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFResult.setResultDataset(QueryResult: TSQLQuery);
begin
  resultDataset := QueryResult;

end;

end.
