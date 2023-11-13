unit osd_md_html_dlg;

//{$mode objfpc}
{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  LCLIntf, IpHtml,
  //HtmlView,
  MarkdownUtils,
  MarkdownProcessor;
  //HtmlGlobals,
  //HTMLUn2;

type

  { TOSD_info }

  TOSD_info = class(TForm)
    BitBtn1: TBitBtn;
    FlowPanel1: TFlowPanel;
    IpHtmlPanel1: TIpHtmlPanel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IpHtmlPanel1HotClick(Sender: TObject);
    (*
    procedure HtmlViewerHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: boolean);
    procedure HtmlViewerHotSpotTargetClick(Sender: TObject;
      const Target, URL: ThtString; var Handled: boolean);
    procedure IpHtmlPanel1HotClick(Sender: TObject);
    *)
    procedure IpHtmlPanel1HotURL(Sender: TObject; const URL: String);

  private

  public
    mdContent: string;
  end;

var
  OSD_info: TOSD_info;


implementation

{$R *.lfm}

const
  CSSDecoration = '<style type="text/css">' + 'code{' +
    '  color: #A00;' + '}' + 'pre{' +
    '  background: #f4f4f4;' + '  border: 1px solid #ddd;' +
    '  border-left: 3px solid #f36d33;' +
    '  color: #555;' + '  overflow: auto;' +
    '  padding: 1em 1.5em;' + '  display: block;' +
    '}' + 'pre code{' +
    '  color: inherit;' + '}' + '</style>';


(*
More decoration:

<style type="text/css">
pre {
  background-color: #eee;
  border: 1px solid #999;
  display: block;
  padding: 10px;
}

Blockquote{
  border-left: 3px solid #d0d0d0;
  padding-left: 0.5em;
  margin-left:1em;
}
Blockquote p{
  margin: 0;
}
</style>
*)

var
  md: TMarkdownProcessor;
  HtmlContent: string;


function Convert_md_to_html(markdowntext: string): string;
var
  md: TMarkdownProcessor;
begin
  md := TMarkdownProcessor.createDialect(mdDaringFireball);
  md.UnSafe := True;
  Result := md.process(markdowntext);
  md.Free;
end;

{ TOSD_info }

procedure TOSD_info.FormShow(Sender: TObject);
var
  fs: TStringStream;
  pHTML: TIpHtml;
begin
  HtmlContent := Convert_md_to_html(mdContent);
  try
    fs := TStringStream.Create( HtmlContent );
    try
      pHTML:=TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanelDesc
      pHTML.LoadFromStream(fs);
    finally
      fs.Free;
    end;
    IpHtmlPanel1.SetHtml( pHTML );
    //Caption := IpHtmlPanelAdvice.Title;
  except
    on E: Exception do begin
      MessageDlg( 'Error: '+E.Message, mtError, [mbCancel], 0 );
    end;
  end;
end;

procedure TOSD_info.IpHtmlPanel1HotClick(Sender: TObject);
var
  NodeA: TIpHtmlNodeA;
  URL: String;
begin
  if IpHtmlPanel1.HotNode is TIpHtmlNodeA then begin
    NodeA:=TIpHtmlNodeA(IpHtmlPanel1.HotNode);
    URL:=NodeA.HRef;
    OpenUrl(URL);
  end;
end;

(*
begin
  //mdContent := mdInstallerInfo_QtInstaller;
  HtmlContent := Convert_md_to_html(mdContent);
  HtmlViewer.LoadFromString(CSSDecoration + HtmlContent);
end;
*)



procedure TOSD_info.IpHtmlPanel1HotURL(Sender: TObject; const URL: String);
begin
  //OpenUrl(URL);
end;

procedure TOSD_info.FormCreate(Sender: TObject);
begin
  md := TMarkdownProcessor.createDialect(mdCommonMark);
  md.UnSafe := False;
  (*
  HtmlViewer.DefBackground := clWhite;
  HtmlViewer.DefFontColor := clBlack;
  HtmlViewer.DefFontName := 'Helvetica';
  HtmlViewer.DefFontSize := 12;
  //  HtmlViewer.DefPreFontName:='Lucida Console';
  HtmlViewer.DefPreFontName := 'Courier';
  //HtmlViewer.ServerRoot:=RootPath;
  HtmlViewer.OnHotSpotTargetClick := HtmlViewerHotSpotTargetClick;
  HtmlViewer.OnHotSpotClick := HtmlViewerHotSpotClick;
  //HtmlViewer.OnImageRequest:=@HtmlViewerImageRequest;
  //MStream := TMemoryStream.Create;
  //B_ConvertClick(Self);
  *)
end;

end.
