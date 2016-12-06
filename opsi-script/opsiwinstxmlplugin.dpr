program opsiwinstxmlplugin;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  wixml in 'wixml.pas',
	xmlpluginsynth in 'xmlpluginsynth.pas',
  xmlpluginfunc in 'xmlpluginfunc.pas',
  pluginhelper in 'pluginhelper.pas',
  custapp4delphi in 'custapp4delphi.pas',
  pluginlog in 'pluginlog.pas' {DataModule1: TDataModule};

begin
	{ TODO -oUser -cConsole Main : Hier Code einfügen }
	pluginlog.DataModule1 := TDataModule1.Create(nil);
	pluginlog.Logdatei := TLoginfo.Create;
	pluginlog.LogDatei.LogLevel := 9;
	pluginlog.LogDatei.initiate('c:\opsi.org\log\opsiwinstxmlplugin.log', False);
	pluginlog.Logdatei.Close;
	pluginhelper.main;
end.
