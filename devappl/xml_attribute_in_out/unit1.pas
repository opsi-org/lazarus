unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PairSplitter,
  StdCtrls,
  oslog,
  dom,
  xmlread,
  xmlwrite,
  osxmltdom,
  osxml,
  osxmlsections,
  Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  xml: TuibXMLDocument;

implementation

{$R *.lfm}

{ TForm1 }

procedure initLogging;
var
  logfilename: string;
begin
  logdatei := TLogInfo.Create;
  logfilename := 'xml_attribute_in_out.log';
  LogDatei.WritePartLog := False;
  LogDatei.WriteErrFile := False;
  LogDatei.WriteHistFile := False;
  logdatei.CreateTheLogfile(logfilename, False);

  logdatei.LogLevel := 7;
  (*
  for i := 0 to preLogfileLogList.Count-1 do
    logdatei.log(preLogfileLogList.Strings[i], LLessential);
  preLogfileLogList.Free;
  *)
  //logdatei.log('xml_attribute_in_out version: ' + getversioninfo, LLessential);
  //logdatei.log('Called as: ' + ExtractFileNameOnly(reencode(ParamStr(0), 'system')), LLessential);
end;


procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
var
  list: TStringList;
  mynode : TDOMNode;
  myattributes : TDOMNamedNodeMap;
  i : integer;
  str : string;
begin
  list := TStringList.Create;
  mynode := TDOMNode.Create(nil);
  myattributes := TDOMNamedNodeMap.Create(nil);
  list.AddStrings(Memo1.Lines);
  if xml.createXmlDocFromStringlist(list) then
  begin
    //mynode := xml.FirstChild;
    //xml.getNodeByNameAndTextContent(mynode,getDocumentElement(),'node1','');
    xml.makeTopAsActNodeSet;
    xml.logActNodeSet;
    str :=xml.actNodeSet[0].NodeName;
    memo2.Lines.Add(xml.actNodeSet[0].NodeName);
    mynode := xml.actNodeSet[0].FirstChild;
    str := mynode.NodeName;
    memo2.Lines.Add(mynode.NodeName);
    //xml.getNodeByNameAndTextContent(mynode,mynode,'node1','');
    //memo2.Lines.Add(TDOMElement(mynode).ToString);
    memo2.Lines.Add(mynode.NodeName);
    if mynode.HasAttributes then
    begin
    myattributes := mynode.Attributes;
    for i := 0 to myattributes.Length -1 do
       memo2.Lines.Add(myattributes.Item[i].NodeName);
    end;
    memo2.Lines.AddStrings(xml.getXmlStrings);
  end;
  FreeAndNil(list);
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  xml := TuibXMLDocument.Create;
  initLogging;
end;

end.
