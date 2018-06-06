unit getoptinlinku1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls,
  getoptinlinkd1;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
(*
https://download.uib.de/verify/newsletter.php?CUSTOMER=XXX-SAMMEL-XXX&CONTACTNUM=00100188&EMAIL=d.oertel@uib.de&LANG=DE&CONTACTNAME=Oertel
*)

procedure TForm1.Button1Click(Sender: TObject);
var
  myurl, basep, customerp, contactnump, emailp, langp, contactnamep : string;
begin
  if DataModule1.SQLQuery1.Active then DataModule1.SQLQuery1.Close;
  DataModule1.SQLQuery1.SQL.Clear;
  DataModule1.SQLQuery1.SQL.Add('select distinct contactnum , p.customer as ccustno, email , surname  ');
  DataModule1.SQLQuery1.SQL.Add('from IASADRBOOKCONTACT c, iascustper p  ');
  //DataModule1.SQLQuery1.SQL.Add('LEFT JOIN IASCUSTPERVERIFED v on ((p.customer= v.customer) and (p.pernum = v.pernum)) ');
  DataModule1.SQLQuery1.SQL.Add('where (Ccompany="UIB") and (c.contactnum=p.persnum) ');
  DataModule1.SQLQuery1.SQL.Add(' and (contactnum like :mypersnum)');
  DataModule1.SQLQuery1.ParamByName('mypersnum').AsString:=edit1.Text;
  DataModule1.SQLQuery1.Open;
  if DataModule1.SQLQuery1.EOF then
  begin
    edit2.Text := 'Fehler: keinen Datensatz gefunden';
  end
  else
  begin
    basep := 'https://download.uib.de/verify/newsletter.php?';
    customerp := 'CUSTOMER='+DataModule1.SQLQuery1.FieldByName('ccustno').AsString;
    contactnump := '&CONTACTNUM='+DataModule1.SQLQuery1.FieldByName('contactnum').AsString;
    emailp := '&EMAIL='+DataModule1.SQLQuery1.FieldByName('email').AsString;
    langp := '&LANG=DE';
    contactnamep := '&CONTACTNAME='+DataModule1.SQLQuery1.FieldByName('surname').AsString;
    myurl := basep+customerp+contactnump+emailp+langp+contactnamep;
    edit2.Text:= myurl;
    if customerp = 'CUSTOMER=' then edit2.Text := 'Fehler: keinen CUSTOMER gefunden';
    if contactnump = '&CONTACTNUM=' then edit2.Text := 'Fehler: keinen CONTACTNUM gefunden';
    if emailp = '&EMAIL=' then edit2.Text := 'Fehler: keinen EMAIL gefunden';
    if langp = '&LANG=' then edit2.Text := 'Fehler: keinen LANG gefunden';
    if contactnamep = '&CONTACTNAME=' then edit2.Text := 'Fehler: keinen CONTACTNAME gefunden';
  end;


end;

end.

