unit osmessagedialog;
// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel

{$MODE DELPHI}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



interface

uses
{$IFDEF WINDOWS}
{$ENDIF}
  //LCLIntf,
  LResources,
  //RichMemo,
  //SysUtils,
  //Classes, Graphics,
  Forms,
  Controls,
  Buttons,
  ExtCtrls, StdCtrls;
  //ComCtrls;

type
  TModalResults = mrOk .. mrNo;
  TModalSet = set of TModalResults;

type

  { TMyMessageDlg }

  TMyMessageDlg = class(TForm)
   Memo1: TMemo;
    Panel1: TPanel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;


    AbortBtn: TBitBtn;
    YesBtn: TBitBtn;
    NoBtn: TBitBtn;
    procedure FormHide(Sender: TObject);
    function wiMessageSized(const ps: string; Results: TModalSet;
      const w, h: integer): TModalResults;
    function wiMessage(const ps: string; Results: TModalSet): TModalResults;
    function showMessage(const header: string; const ps: string; Results: TModalSet): TModalResults;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;


var
  MyMessageDlg: TMyMessageDlg;

implementation



function TMyMessageDlg.wiMessageSized(const ps: string; Results: TModalSet;
  const w, h: integer): TModalResults;
var
  ResultValue: integer;
begin
  Height := h;
  Width := w;
  //RichEdit1.Left := 10;
  //RichEdit1.width := w - 10;



  for ResultValue := mrNone to mrNo do
    if ResultValue in Results then
      case ResultValue of
        mrOk:
        begin
          OKBtn.Enabled := True;
          OKBtn.Visible := True;
        end;
        mrCancel:
        begin
          CancelBtn.Enabled := True;
          CancelBtn.Visible := True;
        end;
        mrAbort:
        begin
          AbortBtn.Enabled := True;
          AbortBtn.Visible := True;
        end;
        mrYes:
        begin
          YesBtn.Enabled := True;
          YesBtn.Visible := True;
        end;
        mrNo:
        begin
          NOBtn.Enabled := True;
          NoBtn.Visible := True;
        end;
      end;

  //Label1.Caption := ps;
  memo1.Lines.Clear;
  memo1.Lines.Append(ps);

  Result := ShowModal;

  //result := mrOK;
end;

function TMyMessageDlg.wiMessage(const ps: string;
  Results: TModalSet): TModalResults;
begin
  Result := wiMessageSized(ps, Results, 650, 240);
end;

function TMyMessageDlg.showMessage(const header: string; const ps: string; Results: TModalSet): TModalResults;
begin
  Caption:= header;
  Memo1.Font.Style:= [];
  Memo1.Font.Size:= 10;
  Result := wiMessageSized(ps, Results, 650, 240);
end;

procedure TMyMessageDlg.FormHide(Sender: TObject);
begin
  OKBtn.Enabled := False;
  OKBtn.Visible := False;
  CancelBtn.Enabled := False;
  CancelBtn.Visible := False;
  //  AbortBtn.Enabled := false; AbortBtn.Visible := false;
  //  YesBtn.Enabled := false; YesBtn.Visible := false;
  //  NOBtn.Enabled := false; NoBtn.Visible := false;
end;

initialization
 {$i osmessagedialog.lrs}



end.
