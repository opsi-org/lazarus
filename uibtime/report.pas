unit report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, LR_Class, LR_DBSet,
  LR_Desgn, Forms, Controls, Graphics, Dialogs;

type

  { TFReport }

  TFReport = class(TForm)
    frDBDataSet1: TfrDBDataSet;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FReport: TFReport;

implementation

{$R *.lfm}

end.

