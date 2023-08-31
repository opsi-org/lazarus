# The Opsi Lazarus Projects

## Table of Contents
1. [General Info](#general-info)
2. [Projects](#projects)
3. [Shared libraries/pascal units](#shared-libraries)
4. [Technologies](#technologies)
5. [Installation](#installation)

## General Info
The Lazarus Projects are part of opsi(open-pc-server-integration, https://www.opsi.org/). 

## Projects
* opsi-script (former opsi-winst). The opsi script interpreter.
* opsi-client-systray. Systray to trigger installations on demand.
* opsi-client-kiosk. Software kiosk allows the user on the client to chose the desired software. Trigger installations on demand 
* opsi-script-beautifier. Increase the readability of opsi scripts by nicely formatting them. 
* opsi-wmi-test. Helper tool for testing wmi commands
* opsi-laz-gui-test. Tool to detect if the OS supports a GUI or not
* opsi-regxepr-tester Helper tool to test regular expressions
* ...

## Shared libraries/pascal units

### Webservice (Unit osWebservice)
The unit osWebservice is the core component to establish a https connection to the opsi-server 
and to perform json-rpc requests and other networking tasks. 
Dependencies: package/library synapse and compiled ssl libraries
Note: The synapse libray is modified (unit ssl_openssl_lib, see function InitSSLInterface and search for SetSSLPath). 
Therefor ssl libraries can be stored in the same folder as the executable is located 
independent of the OS (Windows, Linux, MacOS) except for MacOS in the context of an app bundle. In this case 
the ssl libraries had to be stored in the Frameworks folder. These are the recommended locations for the ssl libraries. 
However other locations are also checked for ssl libraries for backward compatibility. At last try the ssl libraries 
are searched in the standard search path(s) of the OS. The opsi search paths are given in the unit osSSLPaths. 

The ssl libraries (or symlinks) had to be named as follows: 
* WINDOWS: libeay32.dll, ssleay32.dll
* LINUX: libssl.so, libcrypto.so (Linux)
* MACOS: libssl.dylib, libcrypto.dylib (MacOS)

Compilation 
There are two compiler switches which affect the search paths and names for the ssl libraries.
* SSLPATH: if not defined the libraries had to be located in the standard search paths of the OS and had to be named as 
given by the synapse library (original synapse behaviour), except for MacOS here the name of the ssl library is set to 
the ssl library found in /usr/local/lib
* APP_BUNDLE: if defined the search path is the Frameworks folder in the application bundle, otherwise the ssl librariess 
are searched first in the same folder as the executable is located and then will try several fallback options (see source
code of unit osSSLPaths for more details). 


## Technologies
A list of technologies used within the projects:
* [lazarus] (https://www.lazarus-ide.org/)
* [free pascal] (https://www.freepascal.org/)
* [synapse] (http://www.ararat.cz/synapse/doku.php)
* [http(s)] (https://tools.ietf.org/html/rfc7230)
* [json-rpc] (https://www.jsonrpc.org/specification_v1)
* [openssl] (https://www.openssl.org/)
* [sqlite] (https://www.sqlite.org/index.html)
* [...]

## Installation

### Source code
The source code is available from github.com or gitlab.uib.gmbh (only for stuff) and can be cloned with git.

For example: 
```
$ git clone https://github.uib.gmbh/uib/lazarus.git
```

### External libaries (if needed)

#### Windows

Copy the listed libaries to your project folder: 
* [openssl] libeay32.dll, ssleay32.dll
* [sqlite] sqlite3.dll

#### Linux

Copy the listed libaries to your project folder or system search path: 
* [openssl] libssl.so, libcrypto.so
* [sqlite] libsqlite3-0.so

#### MacOS

Copy the listed libaries to your project folder or system search path: 
* [openssl] libssl.dylib, libcrypto.dylib
* [sqlite] libsqlite3-0.dylib



