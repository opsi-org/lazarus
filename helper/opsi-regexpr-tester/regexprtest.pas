unit RegExprTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, RichMemo, LazUTF8, RegExpr;

type
  { TFormRegExpr }

  TFormRegExpr = class(TForm)
    ButtonClear: TButton;
    ButtonExamine: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    EditRegExpr: TEdit;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    LabelFlags: TLabel;
    LabelNotice: TLabel;
    LabelRegExpr: TLabel;
    LabelText: TLabel;
    RichMemoText: TRichMemo;
    StaticTextResult: TStaticText;
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonExamineClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private

  public

  end;

var
  FormRegExpr: TFormRegExpr;
  RegExpObj: TRegExpr;
  nbFlags: integer;

implementation

{$R *.lfm}

{ TFormRegExpr }

procedure TFormRegExpr.FormCreate(Sender: TObject);
begin
  RegExpObj := TRegExpr.Create;
  RichMemoText.Caption := '';
  nbFlags := 0;
end;

procedure TFormRegExpr.CheckBox1Click(Sender: TObject);
var
  regExpr: string;
begin
  regExpr := EditRegExpr.Text;
  if CheckBox1.State = cbChecked then
  begin
    if nbFlags = 0 then
      regExpr := '(?i)' + regExpr
    else
      Insert('i', regExpr, Pos(')', regExpr));
    nbFlags := nbFlags + 1;
  end;
  if CheckBox1.State = cbUnchecked then
  begin
    if nbFlags = 1 then
      Delete(regExpr, Pos('(?i)', regExpr), 4)
    else
      Delete(regExpr, Pos('i', regExpr), 1);
    nbFlags := nbFlags - 1;
  end;
  EditRegExpr.Text := regExpr;
end;

procedure TFormRegExpr.CheckBox2Click(Sender: TObject);
var
  regExpr: string;
begin
  regExpr := EditRegExpr.Text;
  if CheckBox2.State = cbChecked then
  begin
    if nbFlags = 0 then
      regExpr := '(?m)' + regExpr
    else
      Insert('m', regExpr, Pos(')', regExpr));
    nbFlags := nbFlags + 1;
  end;
  if CheckBox2.State = cbUnchecked then
  begin
    if nbFlags = 1 then
      Delete(regExpr, Pos('(?m)', regExpr), 4)
    else
      Delete(regExpr, Pos('m', regExpr), 1);
    nbFlags := nbFlags - 1;
  end;
  EditRegExpr.Text := regExpr;
end;

procedure TFormRegExpr.CheckBox3Click(Sender: TObject);
var
  regExpr: string;
begin
  regExpr := EditRegExpr.Text;
  if CheckBox3.State = cbChecked then
  begin
    if nbFlags = 0 then
      regExpr := '(?s)' + regExpr
    else
      Insert('s', regExpr, Pos(')', regExpr));
    nbFlags := nbFlags + 1;
  end;
  if CheckBox3.State = cbUnchecked then
  begin
    if nbFlags = 1 then
      Delete(regExpr, Pos('(?s)', regExpr), 4)
    else
      Delete(regExpr, Pos('s', regExpr), 1);
    nbFlags := nbFlags - 1;
  end;
  EditRegExpr.Text := regExpr;
end;

procedure TFormRegExpr.CheckBox4Click(Sender: TObject);
var
  regExpr: string;
begin
  regExpr := EditRegExpr.Text;
  if CheckBox4.State = cbChecked then
  begin
    if nbFlags = 0 then
      regExpr := '(?g)' + regExpr
    else
      Insert('g', regExpr, Pos(')', regExpr));
    nbFlags := nbFlags + 1;
  end;
  if CheckBox4.State = cbUnchecked then
  begin
    if nbFlags = 1 then
      Delete(regExpr, Pos('(?g)', regExpr), 4)
    else
      Delete(regExpr, Pos('g', regExpr), 1);
    nbFlags := nbFlags - 1;
  end;
  EditRegExpr.Text := regExpr;
end;

procedure TFormRegExpr.CheckBox5Click(Sender: TObject);
var
  regExpr: string;
begin
  regExpr := EditRegExpr.Text;
  if CheckBox5.State = cbChecked then
  begin
    if nbFlags = 0 then
      regExpr := '(?x)' + regExpr
    else
      Insert('x', regExpr, Pos(')', regExpr));
    nbFlags := nbFlags + 1;
  end;
  if CheckBox5.State = cbUnchecked then
  begin
    if nbFlags = 1 then
      Delete(regExpr, Pos('(?x)', regExpr), 4)
    else
      Delete(regExpr, Pos('x', regExpr), 1);
    nbFlags := nbFlags - 1;
  end;
  EditRegExpr.Text := regExpr;
end;

procedure TFormRegExpr.CheckBox6Click(Sender: TObject);
var
  regExpr: string;
begin
  regExpr := EditRegExpr.Text;
  if CheckBox6.State = cbChecked then
  begin
    if nbFlags = 0 then
      regExpr := '(?r)' + regExpr
    else
      Insert('r', regExpr, Pos(')', regExpr));
    nbFlags := nbFlags + 1;
  end;
  if CheckBox6.State = cbUnchecked then
  begin
    if nbFlags = 1 then
      Delete(regExpr, Pos('(?r)', regExpr), 4)
    else
      Delete(regExpr, Pos('r', regExpr), 1);
    nbFlags := nbFlags - 1;
  end;
  EditRegExpr.Text := regExpr;
end;

procedure TFormRegExpr.ButtonExamineClick(Sender: TObject);
begin
  if length(EditRegExpr.Text) > 0 then
  begin
  if length(RichMemoText.Text) > 0 then
  begin
  //Reinitializing the text format
  RichMemoText.SetRangeColor(0, RichMemoText.GetTextLen, clDefault);
  //Getting the Regular Expression
  RegExpObj.Expression := EditRegExpr.Text;
  //Executing the Regular Expression on all the input text
  if RegExpObj.Exec(RichMemoText.Text) then
  begin
    //If matches :
    StaticTextResult.Color := clLime;
    StaticTextResult.Caption := 'The Regular Expression matches the text !';
    //Selecting the match
    RichMemoText.SetFocus;
    RichMemoText.SelStart :=
      UTF8Length(PChar(RichMemoText.Text), RegExpObj.MatchPos[0] - 1);
    RichMemoText.SelLength := RegExpObj.MatchLen[0];
    RichMemoText.SetFocus;
    //Editing the match's color
    try
      RichMemoText.SetRangeColor(RichMemoText.SelStart,
        RichMemoText.SelLength, clGreen);
    except
    end;
    //if there are more matches..
    while RegExpObj.ExecNext do
    begin
      //Selecting the match
      RichMemoText.SetFocus;
      RichMemoText.SelStart :=
        UTF8Length(PChar(RichMemoText.Text), RegExpObj.MatchPos[0] - 1);
      RichMemoText.SelLength := RegExpObj.MatchLen[0];
      RichMemoText.SetFocus;
      //Editing the match's color
      try
        RichMemoText.SetRangeColor(RichMemoText.SelStart,
          RichMemoText.SelLength, clGreen);
      except
      end;
    end;
  end
  //If no matches are found
  else
  begin
    StaticTextResult.Color := clRed;
    StaticTextResult.Caption := 'The Regular Expression doesn''t match the text !';
  end;
  end
  else
    MessageDlg('Not allowed: Text field empty',mtError,[mbOK],0);
  end
  else
    MessageDlg('Not allowed: RegExpr field empty',mtError,[mbOK],0);
end;

procedure TFormRegExpr.ButtonClearClick(Sender: TObject);
begin
  EditRegExpr.Text := '';
  RichMemoText.Caption := '';
  RichMemoText.SetRangeColor(0, RichMemoText.GetTextLen, clDefault);
  StaticTextResult.Color := clDefault;
  StaticTextResult.Caption := '';
  CheckBox1.State := cbUnchecked;
  CheckBox2.State := cbUnchecked;
  CheckBox3.State := cbUnchecked;
  CheckBox4.State := cbUnchecked;
  CheckBox5.State := cbUnchecked;
  CheckBox6.State := cbUnchecked;
  nbFlags := 0;
end;

procedure TFormRegExpr.FormDestroy(Sender: TObject);
begin
  RegExpObj.Free;
end;

end.
