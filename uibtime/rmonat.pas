unit rmonat;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, QuickRpt,uibdata, Db, DBTables, Qrctrls,statistik;

type
  TFRmonat = class(TForm)
    QuickRep1: TQuickRep;
    QRBand1: TQRBand;
    QRBand2: TQRBand;
    QRBand3: TQRBand;
    QRDBText1: TQRDBText;
    Query1: TQuery;
    QRDBText2: TQRDBText;
    QRLabel1: TQRLabel;
    QRLabel2: TQRLabel;
    QRLabel3: TQRLabel;
    QRLabel4: TQRLabel;
    QRBand4: TQRBand;
    QRExprIstSum: TQRExpr;
    QRLabel5: TQRLabel;
    QRLabel6: TQRLabel;
    QRLabel7: TQRLabel;
    QRDBSoll: TQRDBText;
    QRLabel8: TQRLabel;
    QRBand5: TQRBand;
    QRSysData1: TQRSysData;
    QRLabel9: TQRLabel;
    QRLabel10: TQRLabel;
    QRLabel11: TQRLabel;
    procedure QRLabel8Print(sender: TObject; var Value: String);
    procedure QRLabel11Print(sender: TObject; var Value: String);
    procedure QRBand3BeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FRmonat: TFRmonat;

implementation

{$R *.lfm}

procedure TFRmonat.QRLabel8Print(sender: TObject; var Value: String);
begin
 value := floattostrf(qrexprIstSum.Value.dblResult
        - fstatistik.querysollstunden.Fieldbyname('stunden').asfloat,
        fffixed,12,2);
end;

procedure TFRmonat.QRLabel11Print(sender: TObject; var Value: String);
var
 wochentag : string;
 wtnr : integer;
begin
 wtnr := Fstatistik.Query1.fieldbyname('wochentag').asinteger;
 case wtnr of
  0 : wochentag := 'So';
  1 : wochentag := 'Mo';
  2 : wochentag := 'Di';
  3 : wochentag := 'Mi';
  4 : wochentag := 'Do';
  5 : wochentag := 'Fr';
  6 : wochentag := 'Sa';
 end;
 caption := wochentag;
end;

procedure TFRmonat.QRBand3BeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 wochentag : string;
 wtnr : integer;
begin
 wtnr := Fstatistik.Query1.fieldbyname('wochentag').asinteger;
 case wtnr of
  0 : wochentag := 'So';
  1 : wochentag := 'Mo';
  2 : wochentag := 'Di';
  3 : wochentag := 'Mi';
  4 : wochentag := 'Do';
  5 : wochentag := 'Fr';
  6 : wochentag := 'Sa';
 end;
 QRLabel11.caption := wochentag;
end;

end.
