rem account data from uib keepass / codesign / signserver
call c:\private\codesignpass.cmd
c:\opsi.org\opsi-dev-cli\opsi-dev-tool.exe -l 7 --codesign-server-url=%CODESIGNURL% --codesign-server-password=%CODESIGNPASS% --signserver-sign opsisetupdetector.exe