# opsi-client-kiosk

## Table of Contents
1. [General Info](#general-info)
2. [Technologies](#technologies)
3. [Installation](#installation)

### General Info
***
The opsi-client-kiosk application is part of opsi(open-pc-server-integration, https://www.opsi.org/). It can be installed on clients managed by opsi and allows users to install/uninstall or update software on demand. 

## Technologies
***
A list of technologies used within the project:
* [lazarus] (https://www.lazarus-ide.org/)
* [free pascal] (https://www.freepascal.org/)
* [synapse] (http://www.ararat.cz/synapse/doku.php)
* [http(s)] (https://tools.ietf.org/html/rfc7230)
* [json-rpc] (https://www.jsonrpc.org/specification_v1)
* [openssl] (https://www.openssl.org/)
* [sqlite] (https://www.sqlite.org/index.html)

## Installation
***

The source code is available from gitlab.uib.gmbh and can be cloned with git.

For example: 
```
$ git clone https://gitlab.uib.gmbh/uib/lazarus.git
```

If you not only want to compile the source code but als want to run and debug the code with lazarus some requirements had to be fullfilled. Depending on platform some exteranl libaries need to be copied to your project folder or need to be installed/available on your system. The opsi-client-agent had to be installed for your specific environment/platform (e.g. you need an opsiclientd running on your system). Some settings within lazarus need to be adapted. 

### Needed libaries:

#### Windows
* [openssl] libaries
* [sqlite] libaries

#### Linux
* [openssl] libaries
* [sqlite] libaries

#### MacOS
* [openssl] libaries
* [sqlite] libaries



