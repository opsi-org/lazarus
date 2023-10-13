unit osd_md_html_dlg;

//{$mode objfpc}
{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  LCLIntf,
  HtmlView,
  MarkdownUtils,
  MarkdownProcessor,
  HtmlGlobals,
  HTMLUn2;

type

  { TOSD_info }

  TOSD_info = class(TForm)
    BitBtn1: TBitBtn;
    FlowPanel1: TFlowPanel;
    HtmlViewer: THtmlViewer;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: boolean);
    procedure HtmlViewerHotSpotTargetClick(Sender: TObject;
      const Target, URL: ThtString; var Handled: boolean);

  private

  public
    mdContent: string;
  end;

var
  OSD_info: TOSD_info;

resourcestring
  //************************************************
  //info_message_html.Text
  //************************************************
  mdInstallerInfo_Installshield =
    '## This is a Installshield Installer.' + LineEnding +
    'So it will be perhaps complicated -' + LineEnding + 'because:' +
    LineEnding + '' + LineEnding + '1. Installshield exists since 1993.' +
    LineEnding + 'Over the time some command line parameter have changed' +
    LineEnding + 'and we could not detect the version of the Installshield that was used.'
    + LineEnding + '' + LineEnding +
    '2. Installshield may create two different kinds of Installer:' +
    LineEnding + 'A kind of classic setup and a kind setup as wrapper around msi.' +
    LineEnding + 'We could not detect for sure, which kind of installer we have.' +
    LineEnding + '' + LineEnding + '3. Installshield is flexible.' +
    LineEnding +
    'So in fact, the developer may have changed the command line parameter to a totally different style.'
    + LineEnding + '' + LineEnding +
    'If you have a MSI-Wrapper then we have as cli parameter:' +
    LineEnding + '' + LineEnding + '- silent:' + LineEnding +
    '`/s /v" /qn ALLUSERS=1 REBOOT=ReallySuppress`"' + LineEnding +
    '' + LineEnding + '- unattended:' + LineEnding +
    '`/s /v"/qb-! ALLUSERS=1 REBOOT=ReallySuppress`"' + LineEnding +
    '' + LineEnding + 'If you have a classic setup then we have as cli parameter just:'
    + LineEnding + '' + LineEnding + '- silent:' + LineEnding +
    '`/s`' + LineEnding + '' + LineEnding +
    'If you have a classic setup that is very old (last century or near by), then you perhaps have to add the parameter:'
    + LineEnding + '`/sms`';
  mdInstallerInfo_InstallAnywhere =
    '## This is a InstallAnywhere Installer.' + LineEnding +
    'If the parameter `-i silent` does not work, try the following:' + '' + LineEnding +
    LineEnding + 'Run the installer interactive with the `-r` switch followed by' +
    LineEnding +
    'the path and file name of the response file you want to generate.' +
    LineEnding + '' + LineEnding + 'For example:' +
    LineEnding + '' + LineEnding + '`setup.exe -r "./response.txt"`' +
    LineEnding + '' + LineEnding +
    'Then you have to add at the top of the generated response file the line:' +
    LineEnding + '' + LineEnding + '`INSTALLER_UI=silent`' +
    LineEnding + '' + LineEnding + 'Then run silent by calling:' +
    LineEnding + '`setup.exe -f "./response.txt"`' + LineEnding;
  mdInstallerInfo_PortableApps =
    '## This is not a setup program.' + LineEnding +
    'It is a PortableApps Selfextractor.' +
    LineEnding + 'So there are no unattended / silent modes.' + '' + LineEnding +
    LineEnding + 'Uncompress with 7zip and copy the files';
  mdInstallerInfo_SetupFactory =
    '## This is a Setup Factory Installer.' + LineEnding +
    'Perhaps the parameter `/S` may work for silent mode.' + LineEnding + '' + LineEnding +
    'But often this functionality is not enabled.' + LineEnding + '' + LineEnding +
    'In this case you have extract / install the content and deploy it on an other way.';
  mdInstallerInfo_QtInstaller =
    '## This is a QT Installer.' + LineEnding + '' + LineEnding +
    'Perhaps the standard parameters may work for silent mode.' +
    LineEnding + 'In this case you have to give the **installdir** - it will not work without.'
    + LineEnding + '' + LineEnding +
    'In other cases, you may call an answer script (*.qs) with the parameter `--script` .'
    + LineEnding + 'And you should have a look at the following documentation pages:.' +
    LineEnding + '' + LineEnding +
    '* <https://doc.qt.io/qtinstallerframework/ifw-cli.html> ' +
    LineEnding + '* <https://doc.qt.io/qtinstallerframework/ifw-use-cases-cli.html> ' +
    LineEnding + '* <https://wiki.qt.io/Online_Installer_4.x> ' +
    LineEnding + '* <https://gist.github.com/WindAzure/f3bed9e058cdc81eaa357414610c9125> ';


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
begin
  //mdContent := mdInstallerInfo_QtInstaller;
  HtmlContent := Convert_md_to_html(mdContent);
  HtmlViewer.LoadFromString(CSSDecoration + HtmlContent);
end;

procedure TOSD_info.HtmlViewerHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: boolean);
begin
  Handled := OpenUrl(SRC);
end;

procedure TOSD_info.HtmlViewerHotSpotTargetClick(Sender: TObject;
  const Target, URL: ThtString; var Handled: boolean);
begin
  Handled := OpenUrl(URL);
end;

procedure TOSD_info.FormCreate(Sender: TObject);
begin
  md := TMarkdownProcessor.createDialect(mdCommonMark);
  md.UnSafe := False;
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

end;

end.
