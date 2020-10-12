program opsiscriptnogui;

{$MODE Delphi}
  {$H+}
{$apptype console}

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the General Public License.

// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html

// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/




uses
  lazutf8,
  //defaulttranslator,
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  ///LCLIntf,
  ///indylaz,
  osfunc,
  osparser,
  osmain in 'osmain.pas',
  oslocale,
  custapp,
  //unicodestringmanager,
  {$IFDEF UNIX}
  {$ENDIF }
  osconf, oslindesktopfiles, osfunclin, ostxstringlist;


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

var
  Application: Topsiscript;

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
  //Application.;
  //Application.Terminate;
  //exit;
end;




{$R *.res}

begin
  Application:=Topsiscript.Create(nil);
  Application.Title:='opsi-script-nogui';
  Application.Initialize;
  Application.DoRun;
  Application.Free;
end.

