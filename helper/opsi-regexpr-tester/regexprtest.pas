unit RegExprTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, RichMemo, LazUTF8, SynHighlighterAny, RegExpr;

type
  { TFormRegExpr }

  TFormRegExpr = class(TForm)
    ButtonClear: TButton;
    ButtonExamine: TButton;
    EditRegExpr: TEdit;
    LabelRegExpr: TLabel;
    LabelText: TLabel;
    LabelResult: TLabel;
    RichMemoText: TRichMemo;
    StaticTextResult: TStaticText;
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonExamineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private

  public

  end;

var
  FormRegExpr: TFormRegExpr;
  RegExpObj: TRegExpr;

implementation

{$R *.lfm}

{ TFormRegExpr }

procedure TFormRegExpr.FormCreate(Sender: TObject);
begin
  RegExpObj := TRegExpr.Create;
  RichMemoText.Caption:='';
end;

procedure TFormRegExpr.FormDestroy(Sender: TObject);
begin
  RegExpObj.Free;
end;

procedure TFormRegExpr.ButtonExamineClick(Sender: TObject);
begin
  //Reinitializing the text format
  RichMemoText.SetRangeColor(0, RichMemoText.GetTextLen, clDefault);
  //Getting the Regular Expression
  RegExpObj.Expression := EditRegExpr.Text;
  //Executing the Regular Expression on all the input text
  if RegExpObj.Exec(RichMemoText.Text) then
     begin
        //If matches :
        StaticTextResult.Color := clLime ;
        StaticTextResult.Caption := 'The Regular Expression matches the text !' ;
        //Selecting the match
        RichMemoText.SetFocus;
        RichMemoText.SelStart := UTF8Length(PChar(RichMemoText.Text), RegExpObj.MatchPos[0]-1);
        RichMemoText.SelLength := RegExpObj.MatchLen[0];
        RichMemoText.SetFocus;
        //Editing the match's color
        RichMemoText.SetRangeColor(RichMemoText.SelStart, RichMemoText.SelLength, clGreen);
        //if there are more matches..
        while RegExpObj.ExecNext do
          begin
            //Selecting the match
            RichMemoText.SetFocus;
            RichMemoText.SelStart := UTF8Length(PChar(RichMemoText.Text), RegExpObj.MatchPos[0]-1);
            RichMemoText.SelLength := RegExpObj.MatchLen[0];
            RichMemoText.SetFocus;
            //Editing the match's color
            RichMemoText.SetRangeColor(RichMemoText.SelStart, RichMemoText.SelLength, clGreen);
          end;
     end
  //If no matches are found
  else
  begin
     StaticTextResult.Color := clRed ;
     StaticTextResult.Caption := 'The Regular Expression doesn''t match the text !' ;
  end;
end;

procedure TFormRegExpr.ButtonClearClick(Sender: TObject);
begin
  EditRegExpr.Text:='';
  RichMemoText.Caption:='';
  RichMemoText.SetRangeColor(0, RichMemoText.GetTextLen, clDefault);
  StaticTextResult.Color := clDefault ;
  StaticTextResult.Caption := '' ;
end;

end.

