program winst;

// This code is part of the opsi.org project
//
// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the General Public License.
//
// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html
//
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/





uses
  packdefs in 'packdefs.pas',
  zipinter in 'zipinter.pas',
  Forms,
  wibtch in 'wibtch.pas' {FBatchOberflaeche},
  wifunc in 'wifunc.pas',
  wiglob in 'wiglob.pas',
  wimemo in 'wimemo.pas' {SystemInfo},
  wirequlist in 'wirequlist.pas',
  wispecfolder in 'wispecfolder.pas',
  wisynt in 'wisynt.pas',
  wixml in 'wixml.pas',
  Unicode in 'unicode.pas',
  synacode in 'synacode.pas',
  synautil in 'synautil.pas',
  synsock in 'synsock.pas',
  blcksock in 'blcksock.pas',
  wiservicepassword in 'wiservicepassword.pas' {DialogServicePassword},
  wishowfile in 'wishowfile.pas' {ShowTextFile},
  VersionInfoX in 'VersionInfoX.pas',
  ldapsend in 'ldapsend.pas',
  windatamodul in 'windatamodul.pas' {DataModule1: TDataModule},
  wimain in 'wimain.pas' {CentralForm},
  DSiWin32 in 'DSiWin32.pas',
  widatamodul in 'widatamodul.pas' {DataModule2: TDataModule},
  wimesg in 'wimesg.pas' {OKBottomDlg},
  wilocale in 'wilocale.pas',
  wiswaudit in 'wiswaudit.pas',
  base64 in 'base64.pas',
  superobject in 'superobject.pas';

{$R *.res}



begin
	Application.Title := '';
	Application.CreateForm(TCentralForm, CentralForm);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TDataModule2, DataModule2);
  Application.CreateForm(TSystemInfo, SystemInfo);
  Application.CreateForm(TShowTextFile, ShowTextFile);
  Application.ShowMainForm := false;
	Application.Run;
end.
