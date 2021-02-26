Installation/Testing instructions
- opsi-client-agent had to be installed on the developing/testing environment

[Windows]
These libs had to be in the same folder as the kiosk application:
- sqlite3.dll 

- libeay32.dll
- libssl32.dll
- ssleay32.dll
(or openssl had to be installed and the libssl paths had to be set in the PATH variable)

[Linux]
The following packages had to be installed:
libssl-dev
libsqlite3-dev

[MacOS]
For MacOs 10.15 (Catalina) and higher the kiosk application runs only under root. 
