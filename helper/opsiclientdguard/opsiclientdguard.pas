// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: detlef oertel
//
// derivated from:
// fpc example cleandirs.lpi (C:\lazarus\examples\cleandir)
// http://wiki.lazarus.freepascal.org/Daemons_and_Services
// https://www.freepascal.org/~michael/articles/daemons/daemons.pdf
// https://www.freepascal.org/~michael/articles/services/services.pdf
// Thanks to Michaël Van Canneyt



Program opsiclientdguard;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}
  daemonapp,
  sysutils,
  eventlog,
  svcmap,
  svcservice;

{$R *.res}

begin
  Application.Title:='opsiclientd guard service';
  with Application do
  begin
    Title:='opsiclientd guard service';
    EventLog.LogType := ltFile;
    EventLog.DefaultEventType := etDebug;
    EventLog.AppendContent := true;
    EventLog.FileName := ChangeFileExt(ParamStr(0), '.log');
    Initialize;
    Log(etInfo, 'Starting .....');
    Run;
  end;
end.
