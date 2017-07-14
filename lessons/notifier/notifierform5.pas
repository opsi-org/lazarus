unit notifierform5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtDlgs, ExtCtrls, Buttons, notifierform5a, inifiles, contnrs;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    ButtonLoadIni: TButton;
    ButtonLabel: TButton;
    ButtonShow: TButton;
    ButtonLoad: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure ButtonLabelClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonLoadIniClick(Sender: TObject);
    procedure ButtonShowClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  //objlist : TObjectList;
  navlist: TStringList;
  sectionlist: TStringList;
  endHandler: TNotifyEvent;


implementation

{$R *.lfm}

{ TForm1 }

function objectByIndex(myIni: TIniFile; aktsection: string): TObject;
var
  myLabel: TLabel;
  myButton: TButton;
  mytmpstr: string;
begin
  if aktsection = 'Form' then
  begin

  end
  else
  if aktsection = 'ImageBg' then
  begin
    mytmpstr := ExtractFilePath(myini.FileName);
    mytmpstr := mytmpstr + myini.ReadString(aktsection, 'File', '');
    form2.Image1.Picture.LoadFromFile(mytmpstr);
    form2.Image1.Repaint;
    application.ProcessMessages;
  end
  else
  if pos('Label', aktsection) > 0 then
  begin
    if aktsection = 'LabelStatus' then
    begin
      myLabel := TLabel.Create(form2);
      myLabel.Parent := form2;
      myLabel.Name:=aktsection;
      myLabel.Caption := myini.ReadString(aktsection, 'Text', '');
    end;
  end
  else
  if pos('Button', aktsection) > 0 then
  begin
     if aktsection = 'ButtonStop' then
    begin
      myButton := TButton.Create(form2);
      myButton.Parent := form2;
      myButton.Name:=aktsection;
      myButton.Caption := myini.ReadString(aktsection, 'Text', 'emppty');
    end;
  end;
end;

function fillnavlist(var myIni: TIniFile): TStrings;
var
  //sectionlist,
  keylist: TStringList;
  i: integer;
  aktsection: string;
begin
  //sectionlist := Tstringlist.Create;
  keylist := TStringList.Create;
  Result := TStringList.Create;
  myini.ReadSections(sectionlist);
  for i := 0 to sectionlist.Count - 1 do
  begin
    aktsection := sectionlist[i];
    keylist.Clear;
    myIni.readsection(aktsection, keylist);
    if keylist.IndexOf('SubjectId') > -1 then
    begin
      //Result.Add(keylist.Values['SubjectId']+'='+aktsection);
      Result.Add(myIni.ReadString(aktsection, 'SubjectId', 'null') + '=' + aktsection);
    end;
  end;
end;

procedure TForm1.ButtonLoadClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Form2.Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    Form2.Show;
  end;
end;

procedure TForm1.ButtonLoadIniClick(Sender: TObject);
var
  myini: TIniFile;
  aktsection: string;
  i: integer;
begin
  if OpenDialog1.Execute then
  begin
    myini := TIniFile.Create(OpenDialog1.FileName);
    navlist.AddStrings(fillnavlist(myIni));
    for i := 0 to navlist.Count - 1 do
    begin
      memo1.Append(navlist.Strings[i]);
    end;
    for i := 0 to sectionlist.Count - 1 do
    begin
      aktsection := sectionlist[i];
      objectByIndex(myIni, aktsection);
    end;
  end;
  myini.Free;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  //Application.Terminate;
  //exit;
  halt;
end;

procedure TForm1.ButtonLabelClick(Sender: TObject);
var
  mylabel: TLabel;
begin
  mylabel := TLabel.Create(self);
  mylabel.Caption := 'Hello World';
  ;
  mylabel.Parent := form2;
end;

procedure TForm1.ButtonShowClick(Sender: TObject);
begin
  form2.Show;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //if Assigned(form2.objlist) then objlist.Free;
  if Assigned(navlist) then
    navlist.Free;
end;


begin
  //objlist := TObjectList.Create;
  navlist := TStringList.Create;
  sectionlist := TStringList.Create;
end.
