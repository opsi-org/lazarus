program winst;

{$MODE Delphi}
  {$H+}

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/




{$IFDEF WINDOWS} {$DEFINE GUI} {$ENDIF}

uses
  osencoding,
  lcltranslator, {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  ///LCLIntf,
  packdefs in 'packdefs.pas',
  {$IFDEF WINDOWS}zipinter in 'zipinter.pas', {$ENDIF}
  osconf,
  osshowsysinfo {SystemInfo},
  //wirequlist in 'wirequlist.pas',
  {$IFDEF WINDOWS}wispecfolder in 'wispecfolder.pas',{$ENDIF}
  {$IFDEF WINDOWS}VersionInfoX in 'VersionInfoX.pas',{$ENDIF}
  ldapsend in 'ldapsend.pas',
  osmain ,
  {$IFDEF WINDOWS}{$ENDIF}
  osmessagedialog {OKBottomDlg},
  oslocale in 'oslocale.pas',
  {$IFDEF WINDOWS}osswaudit in 'osswaudit.pas',{$ENDIF}
  osfunc, osparser,
  {$IFDEF GUI}
  Forms,
  osservicepassdlg {DialogServicePassword},
  //osshowlog {ShowTextFile},
  osbatchgui,
  osinteractivegui {CentralForm},
  oslistedit,
  {$ENDIF GUI}
  {$IFDEF LINUX}osfunclin {$ENDIF}
  {$IFDEF WIN32}
  jclexcerpt,
  oslocaladmin,
  {$ENDIF WIN32}
  {$IFDEF WINDOWS}
  osfuncwin,
  opsihwbiosinfo,
  osfuncwin2,
  uCpuUsage,
  SystemCriticalU,
  osfuncwin3,
  osregistry,
  {$ENDIF WINDOWS}
  osinputstring,
  LAZUTF8;



{$R winst.res}
{$R manifest.rc}
//{$i winst.lrs}


begin
  Application.Title:='opsi-winst';
  Application.Initialize;

  Application.CreateForm(TCentralForm, CentralForm);
  Application.CreateForm(TSystemInfo, SystemInfo);
  Application.CreateForm(TFListedit, FListedit);
  //Application.CreateForm(TShowTextFile, ShowTextFile);
  Application.ShowMainForm := False;
  Application.CreateForm(TFinputstring, Finputstring);
  Application.Run;
end.

