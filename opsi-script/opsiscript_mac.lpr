program opsiscript_mac;

{$MODE Delphi}
  {$H+}
{$IFDEF GUI}
//{$apptype console}
{$ENDIF GUI}

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the General Public License.

// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html

// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/




uses
  //lcltranslator,
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  ///LCLIntf,

  ///indylaz,
  osfunc,
  osparser,
  ldapsend in 'ldapsend.pas',
  osmain in 'osmain.pas',
  oslocale,
  oscalc,
  {$IFDEF GUI}
  Interfaces, // this includes the LCL widgetset
  //Forms,
  osbatchgui {FBatchOberflaeche},
  osmessagedialog,
  osshowsysinfo,
  osinteractivegui,
  osservicepassdlg,
  oslistedit,
  {$ELSE GUI}
  custapp,
  {$ENDIF GUI}
  {$IFDEF UNIX}
  osfunclin, osconf, opsihwbiosinfo, oslindesktopfiles, osdefinedfunctions,
  osparserhelper, OSProcessux,
  {$ENDIF UNIX}
  {$IFDEF WINDOWS}
  packdefs in 'packdefs.pas',
  zipinter in 'zipinter.pas',
  wispecfolder in 'wispecfolder.pas',
  VersionInfoX in 'VersionInfoX.pas',
  DSiWin32 in 'DSiWin32.pas',
  wiswaudit in 'wiswaudit.pas',
  jclexcerpt,
  wilocaladmin,
  osfuncwin,
  osfuncwin2,
  {$ENDIF}
  Forms;

{$IFNDEF GUI}
type

  { Topsiscript }

  Topsiscript = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    //procedure WriteHelp; virtual;
  end;

{ Topsiscript }
constructor Topsiscript.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Topsiscript.Destroy;
begin
  inherited Destroy;
end;

procedure Topsiscript.DoRun;
begin
  osmain.main;
end;

var
  Application: Topsiscript;

{$ELSE GUI}
{$R opsiscript.res}
{$ENDIF GUI}


//{$R manifest.rc}
//{$i winst.lrs}


//{$R *.res}



//{$R *.res}

begin
  {$IFNDEF GUI}
  Application:=Topsiscript.Create(nil);
  {$ENDIF GUI}
  Application.Scaled:=True;
  Application.Title:='opsi-script';
  Application.Initialize;
  {$IFDEF GUI}
  Application.ShowMainForm := False;
  {$ENDIF GUI}
  Application.CreateForm(TCentralForm, CentralForm);
  Application.CreateForm(TDialogServicePassword, DialogServicePassword);
  Application.CreateForm(TFListedit, FListedit);
  Application.run;
  {$IFNDEF GUI}
  Application.Free;
  {$ENDIF GUI}
end.

