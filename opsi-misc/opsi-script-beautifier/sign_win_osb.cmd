rem account data from uib keepass / codesign / signserver
call c:\private\codesignpass.cmd
opsi-dev-tool.exe -l 7 --codesign-server-url=%CODESIGNURL% --codesign-server-password=%CODESIGNPASS% --signserver-sign opsi-script-beautifier.exe
opsi-dev-tool.exe -l 7 --codesign-server-url=%CODESIGNURL% --codesign-server-password=%CODESIGNPASS% --signserver-sign opsi_script_beautifier_gui.exe