unit osshowlog;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}


// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 426 $
// $Author: oertel $
// $Date: 2016-04-28 18:39:56 +0200 (Do, 28 Apr 2016) $
//***************************************************************************



interface

uses
{$IFDEF WINDOWS}
{$ENDIF}
  LResources,
  RichMemo,
{$IFDEF WIN32}
  //RichBox,
{$ENDIF WIN32}
  SysUtils,
  Classes, Forms,
  StdCtrls, ExtCtrls, Buttons, Strutils, Spin, IdCompressorZLib,
  oslog;

type

  { TShowTextFile }

  TShowTextFile = class(TForm)
    IdCompressorZLib1: TIdCompressorZLib;
    lzRichEdit1: TlzRichEdit;
    Panel1: TPanel;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    SpinEdit2: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure setcolor;
    procedure reloadfromfile;
    procedure Button1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    orgLogLines: TStrings;
  public
    { Public-Deklarationen }
  end;

var
  ShowTextFile: TShowTextFile;
  FoundAt: longint;
  StartPos, ToEnd: integer;
  filterlevel: integer;
  selline, startposline, lastfound, lastcolorfound: integer;


implementation


procedure TShowTextFile.reloadfromfile;
begin
  if FileExists(LogDatei.FileName) then
  begin
    logdatei.PartCopyToRead;
    orgLogLines.LoadFromFile(ExpandFileName(LogDatei.PartReadFileName));
    Logdatei.PartCloseFromReading;
    //logdatei.Close;
    //orgLogLines.LoadFromFile(LogDatei.FileName);
    //logdatei.reopen;
    //RichEdit1.Lines.LoadFromFile(LogDatei.FileName);
    filterlevel := SpinEdit2.Value;
    setcolor;
    lzRichedit1.Font.Size := SpinEdit1.Value;
    lzRichedit1.Repaint;
    //Richedit1.Font.Size := SpinEdit1.Value;
    //Richedit1.Repaint;
    //Richedit1.MaxLength:=;
    //setcolor;

  end;
end;

procedure TShowTextFile.setcolor;
var
  i, loglevel: integer;
  colorline: string;
  mystrings: TStrings;
  Source: TStream;
  ///Destination:TStream;

begin
  mystrings := TStringList.Create;
  try
    mystrings.Append('{\rtf1');
    mystrings.Append('{\colortbl ;');
    mystrings.Append('\red0\green0\blue0;');        //LLessential
    mystrings.Append('\red200\green0\blue200;');    //LLcritical
    mystrings.Append('\red200\green0\blue0;');      //LLerror
    mystrings.Append('\red255\green128\blue0;');    //LLwarning
    mystrings.Append('\red0\green200\blue0;');      //LLnotice
    mystrings.Append('\red20\green20\blue20;');     //LLinfo
    mystrings.Append('\red150\green150\blue150;');  //LLdebug
    mystrings.Append('\red150\green150\blue150;');  //LLdebug2
    mystrings.Append('\red150\green150\blue0;');    //LLconfidential
    mystrings.Append('}');
    for i := 0 to orgLogLines.Count - 1 do
    begin
      if ('[' = copy(orgLogLines[i], 1, 1)) and (']' = copy(orgLogLines[i], 3, 1)) then
      begin
        loglevel := StrToInt(copy(orgLogLines[i], 2, 1));
        if loglevel <= filterlevel then
        begin
          colorline := AnsiReplaceStr(orgLogLines[i], '\', '\\');
          colorline := AnsiReplaceStr(colorline, '}', '\}');
          colorline := AnsiReplaceStr(colorline, '{', '\{');
          colorline := '\cf' + IntToStr(loglevel) + ' ' + colorline + '\par';
          mystrings.Append(colorline);
        end;
      end;
    end;
    mystrings.Append('}');

    lzRichEdit1.Lines.Clear;
    //RichEdit1.Lines.Clear;

    Source := TStringStream.Create(mystrings.Text);
    try
      try
        lzRichEdit1.LoadFromStream(Source);
      except
        LogDatei.DependentAdd('failed to load log file to RichMemo', LLError);
      end;
      (*
      if not RichEdit1.LoadRichText(Source) then
      begin
        LogDatei.DependentAdd('failed to load log file to RichMemo', LLError);
      end;
      *)
    finally
      Source.Free;
    end;
  finally
    mystrings.Free;
  end;
end;

procedure TShowTextFile.BitBtn1Click(Sender: TObject);
var
  myText: string;
  searchstr, searchline: string;
  startpos, foundat, toend, foundinline: integer;
begin
  myText := lzrichedit1.Text;
  with lzrichedit1 do
  begin
    if selline >= lzrichedit1.Lines.Count then
    begin
      SelStart := 0;
      SelLength := 1;
    end;
    foundat := -1;
    if SelLength <> 0 then

      StartPos := lastfound + SelLength
    else
    begin
      selline := 0;
      StartPos := 1;
      startposline := 1;

    end;
    { ToEnd ist die Länge von StartPos bis zum Textende im RTF-Eingabefeld }

    ToEnd := Length(Text) - StartPos;

    searchstr := LowerCase(Edit1.Text);
    repeat
      searchline := LowerCase(lzrichedit1.Lines[selline]);
      foundinline := PosEx(searchstr, searchline, StartPosline);
      if foundinline > 0 then
      begin
        StartPosline := foundinline + length(searchstr);
        FoundAt := PosEx(LowerCase(Edit1.Text), LowerCase(myText), StartPos);
      end
      else
      begin
        selline := selline + 1;
        startposline := 1;
      end;
    until (FoundAt > 0) or (selline >= lzrichedit1.Lines.Count);

    if FoundAt <> 0 then
    begin
      SetFocus;
      SelStart := FoundAt - selline - 1;
      SelLength := Length(Edit1.Text);
      lastfound := foundat;
    end;
  end;

  /////////////////////////////////
  (*
  myText := richedit1.Text;
  with richedit1 do
  begin
    if selline >= richedit1.Lines.Count then
    begin
      SelStart := 0;
      SelLength := 1;
    end;
    foundat := -1;
    if SelLength <> 0 then

      StartPos := lastfound + SelLength
    else
    begin
      selline := 0;
      StartPos := 1;
      startposline := 1;

    end;
    { ToEnd ist die Länge von StartPos bis zum Textende im RTF-Eingabefeld }

    ToEnd := Length(Text) - StartPos;

    searchstr := LowerCase(Edit1.Text);
    repeat
      searchline := LowerCase(richedit1.Lines[selline]);
      foundinline := PosEx(searchstr, searchline, StartPosline);
      if foundinline > 0 then
      begin
        StartPosline := foundinline + length(searchstr);
        FoundAt := PosEx(LowerCase(Edit1.Text), LowerCase(myText), StartPos);
      end
      else
      begin
        selline := selline + 1;
        startposline := 1;
      end;
    until (FoundAt > 0) or (selline >= richedit1.Lines.Count);

    if FoundAt <> 0 then
    begin
      SetFocus;
      SelStart := FoundAt - selline - 1;
      SelLength := Length(Edit1.Text);
      lastfound := foundat;
    end;
  end;
  *)
end;

procedure TShowTextFile.Button1Click(Sender: TObject);
begin
  setcolor;
end;

procedure TShowTextFile.SpinEdit1Change(Sender: TObject);
begin
  lzRichedit1.Font.Size := SpinEdit1.Value;
  //Richedit1.Font.Size := SpinEdit1.Value;
  setcolor;
  //reloadfromfile;
end;

procedure TShowTextFile.SpinEdit2Change(Sender: TObject);
begin
  filterlevel := SpinEdit2.Value;
  setcolor;
  //reloadfromfile;
end;

procedure TShowTextFile.FormCreate(Sender: TObject);
begin
  filterlevel := LLconfidential;
  SpinEdit2.Value := filterlevel;
  orgLogLines := TStringList.Create;
  //Richedit1.MaxLength := MaxInt;
  lzRichedit1.MaxLength := MaxInt;
end;

procedure TShowTextFile.FormDestroy(Sender: TObject);
begin
  orgLogLines.Free;
end;

initialization
 {$i osshowlog.lrs}



end.

