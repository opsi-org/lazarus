// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 408 $
// $Author: oertel $
// $Date: 2016-04-05 18:52:43 +0200 (Di, 05 Apr 2016) $
//***************************************************************************


program opsiclientkiosk1;

{$mode delphi}{$H+}



uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, indylaz, opsiclientkioskgui,
  defaulttranslator, ockdata, oscrypt
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFopsiClientKiosk, FopsiClientKiosk);
  Application.Run;
end.

