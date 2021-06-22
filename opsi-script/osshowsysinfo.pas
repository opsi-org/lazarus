unit osshowsysinfo;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



(* contains the code for showing a system info memo *)
// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel



interface

uses
{$IFDEF WINDOWS}
{$ENDIF}
  LCLIntf,
  Forms,
  SysUtils, Classes, Graphics,
  StdCtrls, Buttons, Controls, ExtCtrls, LResources;

type

  { TSystemInfo }

  TSystemInfo = class(TForm)
    Label1: TLabel;
    LabelWindowId: TLabel;
    Panel1: TPanel;
    Memo1: TMemo;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure ButtonGesehenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  SystemInfo: TSystemInfo;

implementation

uses
osbatchgui;



procedure TSystemInfo.FormCreate(Sender: TObject);
begin
  Visible := False;
  Label1.Caption:='';
  //SystemInfo.Memo1.Color:= clBlack;
  //  Left := (Screen.Width - Width) div 2;
  //  Top  := (Screen.Height - Height) div 2;
  //Left := screen.MonitorFromWindow(Handle).Width - width;
  //Top := screen.MonitorFromWindow(Handle).Height - Height;
  //Left := FBatchOberflaeche.Left + FBatchOberflaeche.Width;
  //Top := FBatchOberflaeche.Top + FBatchOberflaeche.Height;
end;

procedure TSystemInfo.ButtonGesehenClick(Sender: TObject);
begin
  Visible := False;
end;

procedure TSystemInfo.FormShow(Sender: TObject);
begin
  //Left := FBatchOberflaeche.Left + FBatchOberflaeche.Width;
  //Top := FBatchOberflaeche.Top + FBatchOberflaeche.Height;
end;

initialization
 {$i osshowsysinfo.lrs}



end.

