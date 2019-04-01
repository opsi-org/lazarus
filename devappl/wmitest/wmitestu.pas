unit wmitestu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  oswmi;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    EditNamespace: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    MemoProp: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  myWMIResultList : Tstringlist;
  i : integer;
  errormsg : string;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Label2Click(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  try
  memo1.Clear;
  myWMIResultList := Tstringlist.Create;
  if not osGetWMI(Edit1.Text,MemoProp.Lines,Edit3.Text,myWMIResultList,errormsg) then
     begin
     memo1.Append('Failed');
     memo1.Append(errormsg);
     end
  else
    Memo1.Text:= myWMIResultList.Text;
  (*
    for i := 0 to Pred(myWMIResultList.Count) do
    begin
     memo1.Append(myWMIResultList.Strings[i]);
    end;
    *)
  finally
    myWMIResultList.Free;
  end;
end;

end.

