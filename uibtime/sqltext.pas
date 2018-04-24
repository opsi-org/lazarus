
unit sqltext;

{$MODE Delphi}

interface

uses
  SysUtils,
  ///WinTypes, WinProcs,
  //Messages,
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  ///printers;
  ///dbitypes, dbiprocs, dbierrs,
  ///DBTables,
  uibdata,
  PrintersDlgs;

type
  TFSQL = class(TForm)
    Memosql: TMemo;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    BtnPrint: TSpeedButton;
    PrintDialog1: TPrintDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    //procedure resize;
    procedure FormCreate(Sender: TObject);
    procedure BtnPrintClick(Sender: TObject);
  private
    { Private-Deklarationen }
    relaktsize : integer;
  public
    { Public-Deklarationen }
  end;

var
  FSQL: TFSQL;

implementation

{$R *.lfm}

uses
 //statistik,
  result;


procedure TFSQL.BitBtn1Click(Sender: TObject);
(*
type
 Tsection = (seSQL,seSelect,seTables,seConditions,
             seConnects,seOrder,seGroup,seFTables,seLines);
*)
var
  clearmemo : boolean;
  (*
 i : integer;
 clearmemo, indelpart : boolean;
 section : Tsection;
 dummy : string;
 oldcolor :  TColor;
 *)
begin
 clearmemo := true;
 if memoSQL.lines.count > 0 then
  clearmemo :=
   (messagedlg('Aktuelle Auswahl wird überschrieben !',
                mtWarning,
                mbOKCancel,0) = mrOK);
 if clearmemo and opendialog1.execute then
 begin
  Memosql.lines.loadfromfile(opendialog1.filename);
 end;
end;

procedure TFSQL.BitBtn2Click(Sender: TObject);
var
 //i : integer;
 //indelpart : boolean;
 dummy : string;
begin
 if savedialog1.execute then
 begin
  Memosql.lines.savetofile(savedialog1.filename);
  dummy := MemoSQL.lines[0];
  MemoSQL.lines[0] := dummy;
 end;
end;

procedure TFSQL.SpeedButton1Click(Sender: TObject);
//var
// dbresult, ecode : integer;
// errorstring : String;

begin
 Fresult := TFResult.create(self);
 if DataModule1.Query4Result.Active then DataModule1.Query4Result.close;
 DataModule1.Query4Result.SQL.Text := FSQL.memosql.Text;
 DataModule1.Query4Result.open;
 (*
 FStatistik.Query1.SQL.Text := FSQL.memosql.Text;
 FStatistik.query1.prepare;
 FStatistik.Query1.Open;
 *)
 (*
 dbresult := DBIGetErrorEntry(1,ecode,@errorstring[1]);
 if dbresult <> DBIERR_NONE then
 begin
  dbiError(ecode);
  showmessage(errorstring);
 end;
 *)
 Screen.Cursor := crDefault;
 Fresult.showmodal;
 Fresult.Free;
 DataModule1.Query4Result.close;
end;

(*
procedure TFSQL.resize;
begin
end;
*)

procedure TFSQL.FormCreate(Sender: TObject);
begin
 relaktsize := 100;
end;

procedure TFSQL.BtnPrintClick(Sender: TObject);
//var
//  Line: Integer;
//  PrintText: TextFile;   {Deklaration einer Dateivariablen}
begin
 (*
  if PrintDialog1.Execute then
  begin
    AssignPrn(PrintText);   {Zuweisung von PrintText an den Drucker}
    Rewrite(PrintText);     {Erzeugen und Öffnen der Ausgabedatei}
    Printer.Canvas.Font := Memosql.Font;{Zuweisung der eingestellten Schriftart an die Leinwand}
    for Line := 0 to Memosql.Lines.Count - 1 do

      Writeln(PrintText, Memosql.Lines[Line]);	{Schreiben des Inhalts von Memo1 in das Druckerobjekt}
    CloseFile(PrintText); {Schließen der Druckervariablen}
  end;
  *)
end;
end.
