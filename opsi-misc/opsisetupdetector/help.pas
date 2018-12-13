unit Help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, LazHelpHTML, HtmlView;
//, htmlview;

type

  { TFormHelp }

  TFormHelp = class(TForm)
    HtmlViewerHelp: THtmlViewer;
    //HTMLViewerHelp: THTMLViewer;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetHelpFile(myHelpFile: String);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormHelp: TFormHelp;
  FormHelpWidth:  Integer = 800;
  FormHelpHeight: Integer = 600;
  //HTMLViewerHelp: THTMLViewer;


implementation

{ TFormHelp }

procedure TFormHelp.FormCreate(Sender: TObject);
begin
   // HTMLViewerHelp.LoadFromFile('languages\Help.en.html');
   //HTMLViewerHelp:= THTMLViewer.Create(panel1);
   //HTMLViewerHelp.Parent := panel1;
   //HTMLViewerHelp.Align:=alClient;
   FormHelp.Width  := FormHelpWidth;
   FormHelp.Height := FormHelpHeight;
   FormHelp.FormResize(FormHelp);
end;

procedure TFormHelp.FormDestroy(Sender: TObject);
begin
  //HTMLViewerHelp.Free;
end;


procedure TFormHelp.SetHelpFile(myHelpFile: String);
begin
  HTMLViewerHelp.LoadFromFile(myHelpFile);
end;


procedure TFormHelp.FormResize(Sender: TObject);
begin
   Panel1.Width:=ClientWidth;
   Panel1.Height:=ClientHeight;
   HTMLViewerHelp.Width:=ClientWidth;
   HTMLViewerHelp.Height:=ClientHeight;
end;

initialization
  {$I help.lrs}
end.

