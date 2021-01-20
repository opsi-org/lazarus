program opsiscript;

{$MODE Delphi}
  {$H+}
{$IFDEF GUI}
//{$apptype console}
{$ENDIF GUI}

{$IFDEF DARWIN}
//{$linklib libcrypto.a}
//{$linklib libssl.a}
{$ENDIF DARWIN}


// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the Affero General Public License v3 (AGPL).

// Text of the GPL: http://www.gnu.org/licenses/gpl.html
// Unofficial GPL Translations: http://www.gnu.org/licenses/translations.html

// author: Rupert Roeder, detlef oertel, Jan Werner, Jinene Laajili




uses //lcltranslator,
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  ///LCLIntf,
  packdefs in 'packdefs.pas',
  ///indylaz,
  osfunc,
  osparser,
  ldapsend in 'ldapsend.pas',
  osmain in 'osmain.pas',
  oslocale,
  oscalc,
  osdefinedfunctions,
  osconf,
  ostxstringlist,
  opsihwbiosinfo,
  osparserhelper,
  oslog,
  lazfileutils,
  osprocesses,
  osversioninfo,
  {$IFDEF GUI}
  Interfaces, // this includes the LCL widgetset
  //Forms,
  osbatchgui {FBatchOberflaeche},
  osmessagedialog,
  osshowsysinfo,
  osinteractivegui,
  osservicepassdlg,
  oslistedit,
  Forms,
  {$ELSE GUI}
  custapp,
  oscheck_gui_startable,
  {$ENDIF GUI}
  {$IFDEF UNIX}
  BaseUnix,
  oslindesktopfiles,
  OSProcessux, {$ENDIF UNIX} {$IFDEF DARWIN}
  osfuncmac, {$ENDIF DARWIN} {$IFDEF LINUX}
  osfunclin, {$ENDIF LINUX} {$IFDEF WINDOWS} {$IFDEF WIN32}
  DSiWin32 in 'DSiWin32.pas',
  oslocaladmin,
  jclexcerpt, {$ENDIF WIN32}
  zipinter in 'zipinter.pas',
  wispecfolder in 'wispecfolder.pas',
  VersionInfoX in 'VersionInfoX.pas',
  osswaudit in 'osswaudit.pas',
  osfuncwin,
  osfuncwin2, {$ENDIF}
  lazutf8;



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
    StopOnException := True;
  end;

  destructor Topsiscript.Destroy;
  begin
    inherited Destroy;
  end;

  procedure initLogging;
  var
    logfilename: string;
  begin
    if not Assigned(startupmessages) then
      startupmessages := TStringList.Create;
    startupmessages.Append('startmessage opsi-script created at init logging lpr: ' +
      DateTimeToStr(Now));
    startupmessages.Append('running now gui-test');
    logdatei := TLogInfo.Create;
    //logfilename := ExtractFileNameOnly(ParamStr(0)) + '.log';
    logfilename := logdatei.StandardMainLogPath+'opsi-script-gui-test.log';
    LogDatei.WritePartLog := False;
    LogDatei.WriteErrFile := False;
    LogDatei.WriteHistFile := False;
    logdatei.CreateTheLogfile(logfilename, False);
    logdatei.LogLevel := 8;
    //LogDatei.debug_prog:=true;
    logdatei.log('opsi-script version: ' + getversioninfo, LLessential);
    logdatei.log('Called as: ' + ExtractFileNameOnly(ParamStr(0)), LLessential);
    logdatei.log('Compiled with FPC: '+ {$i %FPCVERSION%} +
    ' for: '+ {$i %FPCTARGETOS%}+'-'+{$i %FPCTARGETCPU%}, LLessential);
  end;

{$IFDEF UNIX}
  procedure Topsiscript.DoRun;
  var
    silent: boolean;
    filepath: string;
    i: integer;
    param: string;
    paramdel: string;
    startgui : boolean;
  begin
    silent := False;
    paramdel := '-';
    startgui := false;
    initLogging;
    filePath := ExtractFilePath(ParamStr(0));
    if filepath = '' then filepath := '/usr/local/bin/';
    if not FileExists(filePath + 'opsi-script-gui') then
      filepath := '/usr/local/bin/';
    if which('opsi-script-gui',filepath) then
       filePath := ExtractFilePath(filepath);
    //logdatei.log('Launch: paramcount ' + Paramcount.ToString, LLessential);
    i := 1;
    while (i <= Paramcount) do
    begin
      param := ParamStr(i);
      if LowerCase(param) = paramdel + 'silent' then
        silent := True;
      Inc(i);
    end;
    //logdatei.log('Found Params: '+executeparamlist.Text,LLnotice);
    if not silent then
    begin
      if check_gui_startable() then
      begin
        logdatei.log('gui ok ... ', LLnotice);
        startgui := true;
        {$IFDEF DARWIN}
        if getLoggedInUser = '' then
        begin
          startgui := false;
          logdatei.log('No logon at macos -  continue with nogui... ', LLnotice);
        end;
        {$ENDIF DARWIN}
        if startgui then
        begin
          if FileExists(filePath + 'opsi-script-gui') then
            logdatei.log('starting opsi-script-gui ... ', LLnotice)
          else logdatei.log('No opsi-script-gui: '+filePath + 'opsi-script-gui'+'  -  continue with nogui... ', LLnotice);
          logdatei.Close;
          if FileExists(filePath + 'opsi-script-gui') then
            fpExecV(filePath + 'opsi-script-gui', argv);
        end;
      end
      else
      begin
        logdatei.log('no gui access - continue with nogui ... ', LLnotice);
        logdatei.Close;
      end;
    end
    else
    begin
      logdatei.log('Parameter silent found - continue with nogui ... ', LLnotice);
      logdatei.Close;
    end;
    osmain.main;
  end;

{$ENDIF UNIX}
{$IFDEF WINDOWS}
  procedure Topsiscript.DoRun;
  begin
    osmain.main;
  end;

{$ENDIF WINDOWS}


var
  Application: Topsiscript;

{$ELSE GUI}

{$ENDIF GUI}

{$IFDEF WINDOWS}
{$R manifest.rc}
{$ENDIF WINDOWS}

  //{$i winst.lrs}


  //{$R *.res}
{$R opsiscript.res}


begin

  {$IFDEF GUI}
  {$IFDEF WINDOWS}
  Application.Scaled := true;
  {$ENDIF WINDOWS}
  Application.Title:='opsi-script-gui';
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TCentralForm, CentralForm);
  Application.CreateForm(TDialogServicePassword, DialogServicePassword);
  Application.CreateForm(TFListedit, FListedit);
  Application.run;
  {$ELSE GUI}
  Application := Topsiscript.Create(nil);
  Application.Title:='opsi-script';
  Application.Initialize;
  Application.DoRun;
  Application.Free;
  {$ENDIF GUI}
end.














