# Changelog of opsi-script

All notable changes to opsi-script are documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Each entry ends with the name of the person who made the change.

## [4.12.22.0] - 2026-11-10

### Changed

- oswebservice.pas: service requests are retried with reconnect on transport failures
  (connection loss, timeouts, resets, changing network interfaces); failed attempts are
  logged with socket/TLS error details. TLS protocol and certificate errors are not retried.
  Connect timeout 10 s, receive timeout 60 s (120 s for log upload) unless set by omc.timeout.
  An explicit timeout (e.g. in opsiservicecall) is the total time budget of the call
  including all retries; without it the total budget is 90 s (180 s for log upload). (Jan Schneider <j.schneider@uib.de>)
- osmain.pas: with -opsiservice opsi-script aborts with an error message if the service
  is not reachable at startup (after the retries) or if backend_info fails. (Jan Schneider <j.schneider@uib.de>)
- osmain.pas: with /productid the first connection probe is done without retries so
  the fallback to https://localhost:4441/rpc starts immediately. (Jan Schneider <j.schneider@uib.de>)
- oswebservice.pas: log an error if a product on client update (installationStatus,
  actionProgress, ...) or the log upload to the service fails. (Jan Schneider <j.schneider@uib.de>)
- oswebservice.pas: improved logging of service connection problems: messages name the
  request (RPC method) and the service URL, the error class, the duration of the attempt and
  the reason for giving up (permanent error, no attempts left, retries disabled, time budget
  used up). Giving up is logged as error, a recovered connection as notice. (Jan Schneider <j.schneider@uib.de>)
- log file: the format "[level] [date time] [product id] message" is unchanged, but the
  content is more compact and consistent. Startup messages are written with their original
  time and level (also those from before the log file was created); the log header and footer
  use "Label: value" lines (script, encodings, start time, product, client, user, script mode,
  system info, exit code, duration, number of errors and warnings); the service connection is
  summarized (one "Service call: method(args) -> result, duration" line per call, slow calls
  as notice/warning, server IP logged once and as warning if it changes); the configuration
  read from the service, the log settings and the client info are logged as one line each;
  no plaintext password is written to the log; log level changes, registry variable changes,
  logged-in users and the client FQDN are logged more compactly; several typos were fixed. (Jan Schneider <j.schneider@uib.de>)
- oswebservice.pas: the "Service call" log line shows the parameters in detail
  (object keys with short values, the first items of lists, each parameter limited in
  length; values of keys like password, secret, token, hostkey are masked). (Jan Schneider <j.schneider@uib.de>)

### Fixed

- oswebservice.pas: network failures no longer trigger the HTTP 500 communication mode fallback. (Jan Schneider <j.schneider@uib.de>)
- oswebservice.pas, osmain.pas: with -opsiservice the service connection events before the
  log file is created (retries, failures) are written to the log as startup messages. (Jan Schneider <j.schneider@uib.de>)
- oswebservice.pas: removed the misleading "HTTPSender result: 500" message after a failed
  log upload (no response received). (Jan Schneider <j.schneider@uib.de>)
- osmain.pas: a client name that differs from COMPUTERNAME only by the domain part or
  the case is no longer reported as warning. (Jan Schneider <j.schneider@uib.de>)
- osparser.pas: GetIpByName on macOS returned a name instead of an IP address because a
  reverse lookup (dig -x) was used; it now does a forward lookup (dscacheutil). (Jan Schneider <j.schneider@uib.de>)

## [4.12.21.2] - 2026-11-03

### Fixed

- fixed functions getUsernameBySession and getLoggedinUserList in opsi-script/osbglistwinuser.pas,
  if username and domain was empty the resulting username was "\",
  used in background install (!290) (Jan Werner <j.werner@uib.de>)
- avoid to guess translation on utf8 based Window systems.
  fixes https://zammad.uib.de/#ticket/zoom/226 (!291) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.21.1] - 2026-09-07

### Fixed

- osguicontrol.pas: fix custom path on macOS (ad83fab580c471936361b62a1275c1582cc6e873) (Jan Werner <j.werner@uib.de>)
- fix depotpath parameter on windows (!289) (Jan Werner <j.werner@uib.de>, Detlef Oertel <d.oertel@uib.de>)

## [4.12.21.0] - 2026-06-29

### Changed

- osbatchgui: change that cli param /normalwindow also force a normal window if called
  in system context and run in ProgramMode 'BuildPC' (!285) (Detlef Oertel <d.oertel@uib.de>, Jan Werner <j.werner@uib.de>)
- osparser: function TuibInstScript.executeWith: showcmd := SW_HIDE; // 2026-06-29:
  changed from SW_SHOWMINIMIZED to work like shellcall
  see also: https://mattermost.uib.gmbh/uib/pl/caiyhyfagff1pfzuw1qr4x37oe (!287) (Detlef Oertel <d.oertel@uib.de>)
- switch on long path awareness in project settings (uib#2026061117000111 - Pfadlaengen in opsi-script) (Jan Werner <j.werner@uib.de>)

### Fixed

- fix getFileInfoMap32 in osparser: producestringlist (!285) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.20.2] - 2026-05-07

### Fixed

- ExitScript: does not exit opsi-script anymore, only the relevant product. (Jan Werner <j.werner@uib.de>)
- oswebservice.pas: isModuleActivated: This function was buggy.
  It gives only on the first call the correct result. (Jan Werner <j.werner@uib.de>)

## [4.12.20.1] - 2026-02-10

### Changed

- Revert "Merge branch 'os/fix/wrong_number_of_params_in_defined_func' into 'main'"
  This reverts commit 0e9516e5, reversing changes made to e0e0358c. (Due to failling of Abo+ packages) (Jan Werner <j.werner@uib.de>)

## [4.12.20.0] - 2026-02-10

### Changed

- oshash.pas: function HashFromFile: open file stream with fmOpenRead or fmShareDenyNone (!282) (Detlef Oertel <d.oertel@uib.de>)
- background install (experimental/undocumented feature, Windows only):
  - [new] unit oswinandproclist   This unit handles to make a list of all windows over all desktops
  and put this together with the processlist
  In the case that a process has multiple windows,
  we will get the info of the last visible window.
  If no window is visible,
  we will get the info of the last unvisible window. (d.oertel, !278)
  - [new] oswinandproclist: function getWinProcessListWithPathAndWindowInfo: TStringList; (d.oertel, !278)
  - [new] oswinandproclist: new: function getRunningForegroundProcesses: TStringList;
  // hereby means 'foreground' that a process has a visible window. (d.oertel, !278)
  - [new] osbackgroundinstall:  function getSearchedRunningForgroundProcesses(procList); : TStringList; (d.oertel, !278)
  - [new] osbackgroundinstall: checkAndHandleRunningProcFull : use getSearchedRunningForgroundProcesses (d.oertel, !278)
  - [new] osfuncwin2: function: KillProcessByNameAndUser (allow to restrict killing to processe of given user (d.oertel, !278)
  - [update] osfuncwin2: old function KillProcessByName now wrapper around KillProcessByNameAndUser (d.oertel, !278)
  - [update] osbglistwinuser: function getLoggedInUserList(activeonly : boolean = false): TStringList;
  can now list all logedon users or the active only (d.oertel, !278)
  - [update]osbackgroundinstall: checkAndHandleRunningProcFull kills only user processes
  via getSessionOwner  and getLoggedInUserList(true) (d.oertel, !278)
  - [update] osbackgroundinstall: checkAndHandleRunningProductProcesses: search also
  for all \*.bin files (used by platorm idependent software like libreoffice, nomachine) (!278) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- UEFI detection on Linux systems (commit 8061b98a71f8e1f306fde07772d1bdf648e7a4b7) (Jan Werner <j.werner@uib.de>)
- osdefinedfunctions.pas: fix wrong number of params in defined function (!281) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.19.0] - 2025-11-14

### Added

- osparser.pas: new opsi-script command ExitScript, usage:
  ExitScript \<Status> \<Report> \<ActionRequest>
  example: ExitScript "not_installed" "Script exit" "none"
  ExitScript (equals ExitScript "unknown" "ExitScript" "none"  (default values))
  ExitScript \<Status> (equals ExitScript \<Status> "ExitScript" "none")
  ExitScript \<Status> \<Report> (equals ExitScript \<Status> \<Report> "none")
   (!279) (Jan Werner <j.werner@uib.de>)

## [4.12.18.8] - 2025-09-16

### Changed

- oslog: Sort FConfidentialStrings by length in TLogInfo.AddToConfidentials
  The problem of confidential string parts:
  We replace confidential strings if they are found in the log string.
  But perhaps is the first match is not the best.
  If our confidential strings are:
  123
  123456
  and our log string is '0123456789' than (working with the list above)
  we would replace '123' because this is the first match.
  This problem may appear often, because our splitstring\* functions
  will declare all parts of a string as confidential if the source string is confidential.
  In order to reduce the problem, we sort the confidential list by string length.
  So in this cas we start with the long confidentil strings
  and so the first match should be the longest one.
  Implementation for sort by length:
  [SOLVED] Sorting items by length
  https://forum.lazarus.freepascal.org/index.php?topic=64188.0 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.18.7] - 2025-08-29

### Added

- osparser: osparser.pas: new script constant %installingVersion%
  %installingVersion% = product version without package version, e.g. 4.12.18.7
  needed for background install
  see https://mattermost.uib.gmbh/uib/pl/y96n4fqmrpfqpps8g78a77rtsr
   (!272) (Jan Werner <j.werner@uib.de>)

## [4.12.18.6] - 2025-08-20

### Changed

- osmain: CheckForProcessProduct: use productStopped (instead of scriptstopped)
  to flag prescript product stop (Detlef Oertel <d.oertel@uib.de>)
- osmain / osmeta: CheckForProcessProduct: use productFailed (instead of scriptfailed)
  to flag prescript product failed (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osmain: BuildPC: fix set product on cilent for scriptsuspendstate (Detlef Oertel <d.oertel@uib.de>)

## [4.12.18.5] - 2025-08-13

### Changed

- osmain: CheckForProcessProduct: Abort Installation on no usable installer in meta file
  (scriptstopped=true) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osbackgroundinstall: removeServicesBinariesFromList: fix service list handling (Detlef Oertel <d.oertel@uib.de>)

## [4.12.18.4] - 2025-08-04

### Changed

- osmain: ProcessProdukt: update state and progress not for tacLogin (Detlef Oertel <d.oertel@uib.de>)
- osmain: Statprogrammodes: Loginscripts:
  do not write login script log to  Component log file (lastprodlog) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.18.3] - 2025-07-29

### Fixed

- fix handling install_in_background=false (Detlef Oertel <d.oertel@uib.de>)
- fix reset defaultloglevel for every product (Detlef Oertel <d.oertel@uib.de>)

## [4.12.18.2] - 2025-07-18

### Fixed

- earlier call of retrieveFolders4Linux; fix problems from 4.12.18.1 (Detlef Oertel <d.oertel@uib.de>)
- fix  issue 59 osfuncwin2: KillProcessbyname does not work on french systems
  osfuncwin2: KillProcessbyname
  h := GetShellWindow; work language independent
  if this didn't worked: let us try with process name
  convert detected domain and usernames with WinCPToUTF8 (opsi-script dom\user found: AUTORITE NT\Système)
  some more logging
  osmain: Loginscripts:
  some more logging (Detlef Oertel <d.oertel@uib.de>)

## [4.12.18.1] - 2025-03-24

### Changed

- osfunc: getOSArchitecture:
  windows: use WMI CPU Architecture together with osfunc:Is64BitSystem (OS bitness)
  to dermine the os architecture (now arm / arm64 also supported) (Detlef Oertel <d.oertel@uib.de>)
- osbackgroundinstall: bg-dialog: write: #@ProgramName@# bold (Detlef Oertel <d.oertel@uib.de>)
- osbackgroundinstall: bg-dialog: no double process entries (Detlef Oertel <d.oertel@uib.de>)

## [4.12.17.2] - 2025-02-13

### Fixed

- oswebservice.pas: remove productPropertyState_getObjects used in
  GetProductPropertyList and replace it with productPropertyState_getValues (!256) (Jan Werner <j.werner@uib.de>)

## [4.12.17.1] - 2025-01-22

### Fixed

- osfuncwin.pas: RunCommandAndCaptureOut: fix array size,
  because array starts with 0 it must be 0...ReadBuffersize-1 (!253) (Jan Werner <j.werner@uib.de>)
- osjson.pas: StringListToJsonArray: fix for special chars in json (!254) (Jan Werner <j.werner@uib.de>)
- oswebservice.pas: do not log product property states on lower log level than 9 during startup
  due to potential confidential content. (!255) (Jan Werner <j.werner@uib.de>)

## [4.12.17.0] - 2024-10-28

### Changed

- osfunc: FileGetWriteAccess: additional boolean parameter: handleDir, to fix performance problems in same cases (!252) (Jan Werner <j.werner@uib.de>)

### Fixed

- oslog.pas, opsiscript.lpr: include cwstring, clocale (j.werner. !249) (Jan Werner <j.werner@uib.de>)
- osparser.pas -> execwith powershell: remove empty string from output also for StartProcess
  and not only for RunCommandandCaptureOut, see changelog for opsi.script 4.12.16.1 (!251) (Detlef Oertel <d.oertel@uib.de>, Jan Werner <j.werner@uib.de>)

## [4.12.16.1] - 2024-09-11

### Fixed

- powershellcall/ExecWith powershell:
  output is at least a stringlist with one empty element instead of empty list
  TuibInstScript.executeWith: with powershell:
  clear output list if there is just one empty element (Detlef Oertel <d.oertel@uib.de>)

## [4.12.16.0] - 2024-08-08

### Changed

- remove logging indents, set loglevel for IF-Conditions to "Debug2" (!244) (Jan Werner <j.werner@uib.de>)
- oswebservice.pas: refactor states and actions, cleanup unused variables, remove opsidata
   (!245) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: tssleep - handling of syntax and runtime errors (m.michaelis, !242) (Jan Werner <j.werner@uib.de>)
- powershellcall/ExecWith powershell:  approvments in runnning of powershell
  commands on machines were powershell scripts are restricetd. (!243) (Jan Werner <j.werner@uib.de>)
- HasMinimumSpace for Linux and macOS, issue 38:
  osparser: evaluateBoolean: HasMinimumSpace: Handle first parameter (s1)
  s1 should be a directory at Unix (Linux / macOS)
  s1 = empty = actual dir ".", s1= %SYSTEMDRIVE% = warning and fallback to root dir: "/"
   (!246) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.15.4] - 2024-07-17

### Fixed

- osjson : convert jsonArray from / to stringlist, functions jsonAsArrayToStringList, stringListToJsonArray (Jan Werner <j.werner@uib.de>)
- osmain, oswebservice: fix memory problem in "Produkte" (list of products was empty or wrong)
  when using reload product sequence e.g. in opsi-auto-update (Jan Werner <j.werner@uib.de>)
- osfunc.pas: DeleteFileWithRetries() is only for windows, include compiler switch {$IFDEF WINDOWS} (Jan Werner <j.werner@uib.de>)
- osparser.pas:: sleepseconds (tssleep) - no parameter error message in testsyntax mode (Jan Werner <j.werner@uib.de>)

## [4.12.15.3] - 2024-06-28

### Fixed

- osbatchgui: fix for High DPI, FormShow method was not assigned to the OnShow event (Jan Werner <j.werner@uib.de>)

## [4.12.15.2] - 2024-06-27

### Fixed

- osbatchgui: fix for High DPI (Jan Werner <j.werner@uib.de>)

## [4.12.15.1] - 2024-06-19

### Fixed

- osbatchgui: message in BatchGUI was not displayed correctly due to (#47, , d.oertel 2024-04-30) triggered FormShow,
  which then reloaded the skin and displayed the message "Loadin skin...",
  best solution put all code from FormShow into FormCreate so that
  only once the Form is initialized. (Jan Werner <j.werner@uib.de>)

## [4.12.15.0] - 2024-06-07

### Changed

- used openssl version is logged (Jan Werner <j.werner@uib.de>)

### Fixed

- osbatchgui: batchgui via timer nach vorne schieben, configurierbar ?
  (#47, , d.oertel 2024-04-30)
  call FBatchOberflaeche.SetForceStayOnTop with false before and with true after running a process
  SetForceStayOnTop will also switch the TimerOnTop on / off (Jan Werner <j.werner@uib.de>)
- in some circumstances powershellcalls/ExecWith powershell does not work
  if powershell execution policy was set by "UserPolicy" to "AllSigned" or "Restricted" (Jan Werner <j.werner@uib.de>)
- TOML file handling: now correctly escape backslash (Jan Werner <j.werner@uib.de>)

## [4.12.14.1] - 2024-04-24

### Fixed

- readConfigsFromService in /ServiceBatch: fix for opsi-script running in the context of opsi-template-with-userlogin
  (fqdn/clientdid was empty) (!222) (Jan Werner <j.werner@uib.de>)

## [4.12.14.0] - 2024-03-01

### Changed

- opsiservicecall now specifies its own user agent: ProductID + ' (opsiservicecall:\<typeofcall>)' (!214) (Jan Werner <j.werner@uib.de>)
- set UTF8 filename encoding for getFileListFromZip (m.michaelis, !211) (Jan Werner <j.werner@uib.de>)
- readConfigsFromService in /ServiceBatch (m.michaelis, !218) (Jan Werner <j.werner@uib.de>)

## [4.12.13.0] - 2024-02-09

### Changed

- osmain.pas: update the prodcut status/progress (ProductOnClient_updateObject) for action requests uninstall and always (!199, 68c13bd5a6dac3e28becf21d4d148192fe677914) (Jan Werner <j.werner@uib.de>)
- osparser.pas: isFatalError: additional log the error message in the logfile (8efada26b0380c2e4c382ce711a80dd94be5132a) (Jan Werner <j.werner@uib.de>)
- opsi-script logging at startup now contains compile date information (!200) (Jan Werner <j.werner@uib.de>)
- GetDefaultLang was depricated and is replaced by SetDefaultLang('') (!201, 3daab47afb6470fccd89895255e3c28b32ed7fa5) (Detlef Oertel <d.oertel@uib.de>, Jan Werner <j.werner@uib.de>)
- ossslpaths.pas: also serach on MacOS ssl libs in the program directory (9ce415835a0aca49582832dc421b722da7b1a42c) (Jan Werner <j.werner@uib.de>)
- Revert "Merge branch 'os/feature/multiline_statements' into 'development'" due to problems and it was an hidden feature
  uib#2023112817000102 — Bug in opsiscript ? - Vorsicht mit Sonderzeichen am Ende einer Kommentarzeile (b7ec7afeb19eeec0eafa5cdfa48b90ed767a69d8) (Jan Werner <j.werner@uib.de>)
- Lazarus to Version 3.0 (Jan Werner <j.werner@uib.de>)
- osparser.pas: FileExists-functions: log resulting filename (m.michaelis, !209) (Jan Werner <j.werner@uib.de>)

### Fixed

- [change/fix] Simplify and revise registry section/handling if modifier allntuserdats/allusrclasses are used and/or it is an userlogginscript (!203) (Jan Werner <j.werner@uib.de>)
- osfunclin.pas, osfuncmac.pas: fix for getting IP address under macos, function getMyIpByTarget (!202) (Jan Werner <j.werner@uib.de>)
- Files section: "unzipfile" now handles UTF8 filenames in zip archives correct, new parameter encoding (m.michaelis, !206) (Jan Werner <j.werner@uib.de>)

## [4.12.12.1] - 2023-11-09

### Fixed

- osparser.pas: correct path used to read opsiclientd.conf on Unix systems,
  opsiservicecall modifier opsiclientd/opsiclientd-once (!198) (Jan Werner <j.werner@uib.de>)

## [4.12.12.0] - 2023-11-03

### Changed

- use opsi 4.3 API method "configState_getValues"  to read configs from service (!189) (Jan Werner <j.werner@uib.de>)
- osparser.pas: powershellcall: remove unnecessary policy handling, update AllSigned hack (!196) (Jan Werner <j.werner@uib.de>)
- replace deprecated method getProductProperties_hash with productPropertyState_getValues, opsi4.3 (!197) (Jan Werner <j.werner@uib.de>)
- remove deprecated API method getProdcutIds_list, due to it is not used (j. werner, !195) (Jan Werner <j.werner@uib.de>)
- remove deprecated method getProductInstallationSatus_listOfHashes, due to it is not used (j. werner, !194) (Jan Werner <j.werner@uib.de>)
- fpTOML library (https://github.com/genericptr/fpTOML) (!184) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- patch local NTUSER.dat if logged-in user is not found (!187) (Detlef Oertel <d.oertel@uib.de>)
- show an individual error message in report also for the event silent install (!188) (Jan Werner <j.werner@uib.de>)
- opsi-script will no longer run in an exception, if the setActionProgress command is used and no service is available (!193) (Jan Werner <j.werner@uib.de>)

## [4.12.11.1] - 2023-09-25

### Added

- opsi logo/corporate design (!186) (Jan Schneider <j.schneider@uib.de>, Detlef Oertel <d.oertel@uib.de>)

### Fixed

- oscertificates.pas: correct path for master ca-certificate file on suse distributions (j. werner, !182) (Jan Werner <j.werner@uib.de>)

## [4.12.11.0] - 2023-07-20

### Added

- oswebservice.pas: new functions to get an ordered product sequence from service:
  function TOpsi4Data.getOpsiVersion: string;
  get opsiversion from backend_info
  function TOpsi4Data.getOpsiServiceMethods(allow_depricated : boolean) : TStringlist;
  get available API methods from service
  function TOpsi4Data.isMethodProvided(method : string): boolean;
  checks if the specific method is available
  function TOpsi4Data.GetSortedProductOnClientListFromService: TStringList;
  procedure TOpsi4Data.SetProductOnClientData(const ProductOnClientData: TStringList);
  set the TOpsi4Data fields FProductStates, FProductActionRequests and SortedProductIDsWhereActionIsSet
   (!172) (Jan Werner <j.werner@uib.de>, Detlef Oertel <d.oertel@uib.de>)
- osparser.pas: Give also detailed opsi service version (from getOpsiVersion) at log header  (d.oertel !172) (Jan Werner <j.werner@uib.de>)

### Changed

- oswebservice.pas: update synapse to support openssl version 3 (!173, #4809) (Jan Werner <j.werner@uib.de>, Detlef Oertel <d.oertel@uib.de>)

## [4.12.10.0] - 2023-06-13

### Added

- osfunc.pas: DeleteShellLink: If the file ends on .url, then delete the url link, otherwise (as before) add a .lnk ending and delete the lnk link (#4399, !141, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osfunc.pas: AllDelete: Prevent deletion of program files directories (#3192, !142, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser: allow modifier name '/AllUserProfiles'  in registry sections in addition to '/AllNTUserDats‘ (!158, #4827) (Jan Werner <j.werner@uib.de>)
- unify ShellInAnIcon and ShellBatch, ShellScript as new name for ShellInAnIcon/...-Sections (!152) (Jan Werner <j.werner@uib.de>)
- osparser.pas: multi line statements: a opsi-script line will concatenated with the next line if the line ends with whitespace+backslash
  and not contains EscapeString (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser.pas: Refactor doTextpatch and doInifilePatches in preparation for the new config file section (!153, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osfunclin.pas: getLinuxReleaseInfoFromLSBRelease: Use lazarus RunCommand instead of RunCommandAndCaptureOut to avoid confusing exceptions in the log (!147, a.schmitz) (Jan Werner <j.werner@uib.de>)
- remove old log levels and logging functions (!126) (Jan Werner <j.werner@uib.de>)
- osparser.pas, createAndProcessScript: Logheader show Windows 11 (!156, #4840) (Jan Werner <j.werner@uib.de>)

### Fixed

- osparser: doRegistryHackRegeditFormat / doRegistryHack
  remove condition branch: 'if Is64BitSystem and ('software' = lowerCase(key0)) then'
  (#4839, !155, d.oertel) (Jan Werner <j.werner@uib.de>)
- oslog: includelog with utf16lebom with unix compiler switch (!157, #4841) (Detlef Oertel <d.oertel@uib.de>, Jan Werner <j.werner@uib.de>)
- osfunc.pas: getProfilesDirList: Fix memory issue (!154, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osfunclin.pas: getLinuxReleaseInfo: Fix getting subrelease (#947, !148, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osconf.pas and osparser.pas: set computername on Unix only if it was not set before and is empty,
  set opsiserviceUser to opsiserviceClientId if opsiserviceUser is empty and opsiserviceClientId was set (!150, #4816) (Jan Werner <j.werner@uib.de>)

## [4.12.9.1] - 2023-04-14

### Fixed

- oswebservice.pas: getSubListResult, getProductPropertyList:
  Empty (property) values are now handelt correct and without raising an exception. (Jan Werner <j.werner@uib.de>)

## [4.12.9.0] - 2023-04-11

### Changed

- oswebservice.pas: remove backend_setOptions (!129, j.werner) (Jan Werner <j.werner@uib.de>)
- oswebservice.pas: replace deprecated API method getAndAssignSoftwareLicenseKey (!134, j.werner) (Jan Werner <j.werner@uib.de>)

### Fixed

- remove possible memory leaks (!132, d.oertel, a.schmitz, j. werner) (Jan Werner <j.werner@uib.de>)
- osbatchgui.pas: remove TQProgressBar to fix memory leaks (!133, a.schmitz, j.werner) (Jan Werner <j.werner@uib.de>)
- osfuncwin2: fix in getProfilesDirListWin, avoid empty string in the profilelist  (!136, d.oertel) (Jan Werner <j.werner@uib.de>)

## [4.12.8.1] - 2023-04-04

### Fixed

- osbatchgui.lfm, osbatchgui.pas: remove creation of TQProgressbar from method FormShow,
  move creation of TQProgressbar into the new method FormCreate. (Jan Werner <j.werner@uib.de>)
- osfunc.pas: FileGetWriteAccess now set rigths only for the current file and directory
  but not for any subdirectories and files within. (Jan Werner <j.werner@uib.de>)

## [4.12.8.0] - 2023-03-14

### Added

- osparser.pas: new script statement 'setdebug_lib' to set/change the config opsi-script.global.debug_lib (#4817, !125, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser.pas: new undocumented script function 'ShowMessageBox' to display a message with countdown during the installation (also in silent mode) (#4795, !127, a.schmitz) (Jan Werner <j.werner@uib.de>)

### Changed

- osbatchgui.pas: High DPI support for BatchGUI (#4814, !123, j.werner) (Jan Werner <j.werner@uib.de>)
- opsi-script for aarch64 (raspi 64 bit) Basic additions to make opsi-script, oposi-script-gui and opsi-laz-gui-test compilable and get it running
  Also new build modes for raspi (!112, d.oertel) (Jan Werner <j.werner@uib.de>)
- make synapse compilable for arm (!106, d.oertel) (Jan Werner <j.werner@uib.de>)
- osparser.pas: delete_subfolder, better error reporting on non win32 systems. (!115, j.werner) (Jan Werner <j.werner@uib.de>)
- oswebservice.pas: getMapOfProductActionRequests/productonClients_getObjects__actionrequests:
  the public function getMapOfProductActionRequests just gives back the result of the private
  function productonClients_getObjects__actionrequests thus:
  - I deleted the unecessary implementation of getMapOfProductActionRequests
  - I rename productonClients_getObjects__actionrequests to getMapOfProductActionRequests
  - I deleted the definition of productonClients_getObjects__actionrequests (!120, j.werner) (Jan Werner <j.werner@uib.de>)

### Removed

- osparser.pas: remove dublicate log warnings for empty or inexisting sections (#1254, !109, a.schmitz) (Jan Werner <j.werner@uib.de>)

### Fixed

- osparser.pas: fix in produceStringList: remove creating list, this must be done outside of the function (!124, j.werner) (Jan Werner <j.werner@uib.de>)
- osparser.pas: removeFromListByContaining: correct removing of the elements from the list (!122, j.werner) (Jan Werner <j.werner@uib.de>)
- TOMLTypes.pas: lowercase all booleans in TTOMLArray.AsTOMLString and TTOMLTable.AsTOMLStringList for correct TOML syntax in the output file after modifications (#4806, !130, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser.pas: Throw error if [actions] section is expected but not found, log a warning if deprecated [initial] section is used (#2201, !128, a.schmitz) (Jan Werner <j.werner@uib.de>)

## [4.12.7.5] - 2023-01-04

### Fixed

- osparser.pas: tsIncludeInsert: if include file not found: not failed in testsyntax mode (Detlef Oertel <d.oertel@uib.de>)
- osparser.pas: tsIncludeAppend: if include file not found: not failed in testsyntax mode (Detlef Oertel <d.oertel@uib.de>)
- osparser.pas: parseAndCallWinbatch:
  if timeoutseconds parameter not a integer: not failed in testsyntax mode (Detlef Oertel <d.oertel@uib.de>)
- osparser.pas: parseAndCallWinbatch:
  if ParameterWaitSecs parameter not a integer: not failed in testsyntax mode (Detlef Oertel <d.oertel@uib.de>)
- osparser.pas: execDOSBatch:
  if timeoutseconds parameter not a integer: not failed in testsyntax mode (Detlef Oertel <d.oertel@uib.de>)
- osparser.pas: showbitmap:
  a slash in the parameter string without Number: not failed in testsyntax mode
  because it could be unix code (with a unix path) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.7.4] - 2022-12-16

### Fixed

- osparser.pas: procedure TuibRegistry.WriteEntry: conversion for large values (DWORD) now works correct (!110, j.werner) (Jan Werner <j.werner@uib.de>)
- osparser.pas: registry exportformat: hexdecimal values which can be interpreted as decimal values are now handled correct (!110, j.werner) (Jan Werner <j.werner@uib.de>)

## [4.12.7.3] - 2022-12-14

### Fixed

- osparser.pas: registry exportformat hexdecimal values without leading $ throw no syntax errors anymore (!108, j.werner) (Jan Werner <j.werner@uib.de>)

## [4.12.7.2] - 2022-12-05

### Fixed

- osparser: linkActionsMain: fix default values for multiple links in one LinkFolder section (!105, a.schmitz) (Jan Werner <j.werner@uib.de>)
- testsyntax: exitcode reset to zero on interactive testsyntax or start button; fixes #4808 (!104, d.oertel) (Jan Werner <j.werner@uib.de>)
- testsyntax: fix testsyntax for sleepsecond (tsSleep) (!104, d.oertel) (Jan Werner <j.werner@uib.de>)
- osinteractivegui: fix IFDEF for jedi code formatting, Ctrl-D (!104, d.oertel) (Jan Werner <j.werner@uib.de>)

## [4.12.7.1] - 2022-11-27

### Fixed

- osparser: linkActionsMain: fix multiple links in one LinkFolder section (!103, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osjson.pas: function stringListToJsonArray: remove adding quotes if string is not quoted because this former behaviour is problematic e.g it could lead to invalid json. (!101, j.werner) (Jan Werner <j.werner@uib.de>)

## [4.12.7.0] - 2022-11-17

### Added

- testsyntax: if testsyntax ist set to true only the syntax of the script is tested. The script is not excecuted. (!81, !95, d.oertel) (Jan Werner <j.werner@uib.de>)

### Fixed

- osparser: testsyntax, retrieveSection: use SearchForSectionLines (Jan Werner <j.werner@uib.de>)
- osparser: fix in OpsiServiceCall section (#4790, !91, a.schmitz) (Jan Werner <j.werner@uib.de>)
- xml2 attribute sequence:
  in (common) osxml, osxmlsections, osxmltdom
  instead of (fcl) DOM, XMLRead, XMLWrite
  now (LCL) az2_DOM, laz2_XMLRead, laz2_XMLWrite
  additional changes to make to run the units with these libraries (references: #4791, , d.oertel) (Jan Werner <j.werner@uib.de>)
- xml2: osxmlsections: makeNodePathWithTextContent: additional parameter for strictmode
  xml2: osxmlsections: makeNodePathWithTextContent:
  use strictmode parameter for node creation (references: #4794, , d.oertel) (Jan Werner <j.werner@uib.de>)
- LinkFolder: oslindesktopfiles: In DeleteShellLink use GetSpecialFolderPath instead of hard-coded path to handle all kinds of base folders (#4792, !97, a.schmitz) (Jan Werner <j.werner@uib.de>)

## [4.12.6.1] - 2022-08-31

### Added

- osparser: Implementation of copy -u for Linux (#4739, !71, j.laajili) (Jan Werner <j.werner@uib.de>)
- osregistry, osparser: new data type QWord added as trdInt64 to normal registry section and regedit format (QWord is not supported in AddReg format) (#4746, !67, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser: new script command deleteListEntries to remove entries from list variables in the windows registry (#4541, !57, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osDoRegistryFunctions: new unit to outsource code from the doRegistry functions in the osparser (a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser: New platform-independant HashFromFile function + HashLib4Pascal library supporting 80 hashing algorithms (#4743, !74, j.laajili) (Jan Werner <j.werner@uib.de>)
- osparser: new statement reloadProductList (#4723, !79, j.werner) (Jan Werner <j.werner@uib.de>)

### Changed

- osparser: parse direct initialization of variables (string and stringlist) equal to 'set', i.e. for example logging is the same now (#4747, !65, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osregistry: synchronize data type handling with the documentation, i.e. supp does not change data types (!59, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osprocessux, osfuncwin: revise code of RunCommandAndCaptureOut (#4764, !73, a.schmitz) (Jan Werner <j.werner@uib.de>)
- oscrypt: function md5fromFile, now uses the new function HashFromFile (#4743, !74, j.werner) (Jan Werner <j.werner@uib.de>)

### Removed

- osparser: remove undocumented script function getStringFromListAtIndex since takeString already exists and is more mature (#4738, !64, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser: remove script statement LocalAdmin (tsLocalAdmin) because it is deprecated and has never been documented (!69, a.schmitz) (Jan Werner <j.werner@uib.de>)

### Fixed

- osparser: Adding new optional encoding parameter to function GetValueFromIniFile (#3071, !68, j.laajili) (Jan Werner <j.werner@uib.de>)
- osparser: parse and evaluate generic stringlist equal to createStringList for correct functionality (#4770, !77, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osstartproc_cp: at Ux use waitForProcEndTimer to avoid zombies (15.7.2022) (Detlef Oertel <d.oertel@uib.de>)
- osparser: throw error if a script variable is defined twice (only works outside defined functions so far) (#4769, !76, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osfunc: files section, del command now deletes any file including hidden files (!85, j.werner) (Jan Werner <j.werner@uib.de>)
- osparser: product was successful even if the script could not be loaded (#4763, !75, j.werner) (Jan Werner <j.werner@uib.de>)

## [4.12.5.6] - 2022-07-18

### Fixed

- osfunc: Files section:  del command with emptystring e.g. del -sf "" does not create an exception anymore, gives only a warning (Jan Werner <j.werner@uib.de>)

## [4.12.5.5] - 2022-07-14

### Fixed

- osfunclin: adapted functions getLinuxVersionMap/getLinuxDistroType for new linux distributions where lsb_release does not work anymore (Jan Werner <j.werner@uib.de>)

## [4.12.5.4] - 2022-06-22

### Fixed

- osparser: parsing of powershellcall with no access string given resulted in an empty string instead of the default 'sysnative' (Jan Werner <j.werner@uib.de>)

## [4.12.5.3] - 2022-06-09

### Changed

- osparser: execution of powershellcall with powershell.exe -ExecutionPolicy ByPass (Jan Werner <j.werner@uib.de>)

## [4.12.5.2] - 2022-06-09

### Fixed

- oswebservice: using former openssl pascal units for MacOS due to the new ones are not working on MacOS. (Jan Werner <j.werner@uib.de>)
- oszip: handle division by zero if files to zip are empty (#4748, !66, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser: execution of powershellcall as statement( tsPowershellcall)
  call of parsePowershellCall with remaining (instead of r) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.5.1] - 2022-05-25

### Added

- osparser: doIniFilePatches: support %userprofiledir% inside of patches section (!51, d.oertel) (Jan Werner <j.werner@uib.de>)
- osparser : CHMOD for Linux.
  Adding new function TuibFileInstall.calcMode to handle 'ugo=+-rwx' mode format.
  Adding 'ugo=+-rwx' mode format to CHMOD.
  Adding '-rwxrwxrwx' mode format to CHMOD.
  Adding '/AllSubFiles' parameter to Files_section CHMOD command. (#4559, !48, j.laajili) (Jan Werner <j.werner@uib.de>)
- osparser: New functions for TOML files handling.
  function LoadTOMLFile(TOMLfilePath: String): TStringList,
  function GetTOMLAsStringList(TOMLcontents: String): TStringList,
  function GetTOMLKeys(TOMLcontents: String): TStringList,
  function GetTOMLTableNames(TOMLcontents: String): TStringList,
  function GetTOMLTable(TOMLcontents: String; table : String): TStringList,
  function ReadTOMLFile (TOMLfilePath: String): String,
  function GetTOMLAsString(TOMLcontents: String): String,
  function GetTOMLTableAsString(TOMLcontents: String; table : String): String,
  function GetValueFromTOML(TOMLcontents: String; keyPath: String; defaultValue: String): String,
  function ModifyTOML(TOMLcontents: String; command: String; keyPath: String; value: String): String,
  function DeleteTableFromTOML(TOMLcontents: String; tablePath: String): String,
  function ConvertTOMLtoJSON(TOMLcontents: String): String,
  function SaveToTOMLFile (TOMLcontents: String; TOMLfilePath: String) : boolean,
  function ConvertTOMLfileToJSONfile(TOMLfilePath: String; JSONfilePath: String): boolean. (#4663, !47, j.laajili) (Jan Werner <j.werner@uib.de>)
- fpTOML: new external library for TOML files handling. Contains the following units.
  unit Scanner
  unit TOML
  unit TOMLParser
  unit TOMLTypes (#4663, !47, j.laajili) (Jan Werner <j.werner@uib.de>)
- unit TOMLTypes: new functions in TOMLTypes unit.
  function TTOMLArray.AsStrings: TStringList,
  function TTOMLArray.AsTOMLString: String,
  function TTOMLTable.AsTOMLString: ansistring,
  function TTOMLTable.AsTOMLStringList: TStringList,
  property TTOMLTable.Header : string read GetHeader,
  function TTOMLTable.GetHeader : string,
  procedure TTOMLTable.Insert(const key: String; const data: TTOMLData),
  procedure TTOMLTable.Insert(const key: String; const value: TTOMLValueType),
  procedure TTOMLTable.Put(const key: String; const data: TTOMLData),
  procedure TTOMLTable.Put(const key: String; const value: TTOMLValueType),
  procedure TTOMLTable.PutValue(const keyIndex: integer; const data: TTOMLData),
  procedure TTOMLTable.PutValue(const keyIndex: integer; const value: TTOMLValueType),
  procedure TTOMLTable.Remove(const key: String),
  procedure TTOMLTable.AssignTable(const table: TTOMLTable) //not used. (#4663, !47, j.laajili) (Jan Werner <j.werner@uib.de>)
- unit osTOML, new functions for TOML handling. Based on fpTOML (#4663, !47, j.laajili) (Jan Werner <j.werner@uib.de>)
- unit osGetRegistryFunctions for parsing of GetRegistry functions. (!38, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osGetRegistryFunctions: new summed GetRegistry functions
  getRegistryKeyList(\<regkey>,\<access str>),
  getRegistryVarList(\<regkey>,\<access str>),
  getRegistryVarMap(\<regkey>,\<access str>). (#4720, !38, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osdefinedfunctions: allow string concatenation in call of defined functions. (#4683, a.schmitz) (Jan Werner <j.werner@uib.de>)

### Changed

- osparser: new internal function parsePowershellCall. Implements commen parsing for powershellCall (Jan Werner <j.werner@uib.de>)
- osparser: parse GetRegistry functions together. (a.schmitz) (Jan Werner <j.werner@uib.de>)
- osfuncwin2 : getWinProcessList: use wincp2utf8 to reencode results (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: update synapse openssl units. This enables to use openssl libs in Version 1.1.1 also on Windows (#4724, !43, j.werner) (Jan Werner <j.werner@uib.de>)
- oswebservice: improved debugging and error handling (Jan Werner <j.werner@uib.de>)

### Fixed

- osparser: throw syntax errors in comments. (#3542, !35, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser: fix in parsing errorsOccurredSinceMark. (#4718, !37, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser: sub files: TuibInstScript.LoadValidLinesFromFile does not cut away spaces around lines any more,
  so that e.g. python code in an ExecWith section keeps its syntax. (!61, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osparser: TuibInstScript.execPowershellCall now shows output also for piped commands e.g. powershellCall("Get-hotfix | Select HotfixId").
  Note: Neverthless be carefull using piped commands in powershellCall! (#4736, !52, j.werner) (Jan Werner <j.werner@uib.de>)
- osfunc: "del" in files section now works with wildcards. (#4560, !36, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osregistry: "deletekey" in registry section uses UTF8ToWinCP to handle umlauts correctly. (#3574, !49, a.schmitz) (Jan Werner <j.werner@uib.de>)
- osprocesses: fix in function isprocessChildOf in function isParentByPid.
  repeat.. until loop now stops after 100 iterations when no other conditions were reached before
  to avoid a pontential endless loop. (Jan Werner <j.werner@uib.de>)
- oswebservice: allow newline and tab in opsiservicecall. (#3522, a.schmitz) (Jan Werner <j.werner@uib.de>)
- oswebservice: do not use twisted version but response content type to detect opsi 4.0 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.37] - 2022-02-01

### Added

- osparser: new global constant %opsiUsertmpDir%; implements #4496 (Detlef Oertel <d.oertel@uib.de>)
- new function isCertInstalledInSystem(\<label>): boolean; [WLM] (Detlef Oertel <d.oertel@uib.de>)
- new function importCertToSystem(\<filename>): noresult;  [WLM] (Detlef Oertel <d.oertel@uib.de>)
- new function removeCertFromSystem(\<label>): noresult;  [WLM] (Detlef Oertel <d.oertel@uib.de>)
- new function listCertificatesFromSystem: stringlist;  [WLM] (Detlef Oertel <d.oertel@uib.de>)
- new command ELSEIF \<condition>  [WLM] (Detlef Oertel <d.oertel@uib.de>)
- osnetworkcalculator: new function netmaskToCidr(netmask: string): string; (Detlef Oertel <d.oertel@uib.de>)
- new function cidrToNetmask(\<string>): string;  [WLM] (Detlef Oertel <d.oertel@uib.de>)
- new function netmaskToCidr(\<string>): string;  [WLM] (Detlef Oertel <d.oertel@uib.de>)
- osparser: new global constant %FQDN% for fqdn in network (not opsi service) context; implements #4554 (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: allow json value syntax in params in opsiServiceCall:
  i.e. numeric/boolean values, arrays, objects and null without quotes; implements #4705 (Detlef Oertel <d.oertel@uib.de>)
- oscertificates: finalize function for linux /mac:
  function pemfileToSystemStore(filename: string): boolean;
  function removeCertFromSystemStore(labelstr: string): boolean;
  function listCertificatesFromSystemStore(): TStringList;
  function isCertInstalledInSystemStore(labelstr: string): boolean; (Detlef Oertel <d.oertel@uib.de>)
- opsparser: implement elseif (Detlef Oertel <d.oertel@uib.de>)
- opsparser: A more detailed description how the If / ELSE / ELSEIF / ENDIF parser works (Detlef Oertel <d.oertel@uib.de>)
- interacticve GUI: Button "View Log" now shows the log in the opsi-logviewer [W/L] (Detlef Oertel <d.oertel@uib.de>)
- osstrlistutils: fixing exception with empty list in areStringlistsEqual, references #4717 (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- killtask (fix): a process of another user can be killed if opsi-script is running as SYSTEM [W] (Detlef Oertel <d.oertel@uib.de>)
- osnetworkcalculator: fixes at cidrToNetmask (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix occasionally access violation when using getReturnListFromSection/getOutStreamFromSection (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.36-2] - 2022-02-08

### Removed

- library uib_lin_install: remove "--no-install-recommends" from debinstall (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.36-1] - 2022-01-19

### Changed

- oszip: show progressbar while zipping or unzipping files (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: getOpsiModules: integrate new backend_getLicensingInfo method with fallback to old backend_info (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: retrieveJSONObject: retry on other communication mode only on 400- bad request result (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: retrieveJSONObjectByHttpPost: retry on other communication mode only on 400- bad request result (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: getOpsiModules: Legal notice to our license protection (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: linIsUefi: implemented uefi test for linux (Detlef Oertel <d.oertel@uib.de>)
- boolean function runningOnUefi now also supports linux (Detlef Oertel <d.oertel@uib.de>)
- updated unit uSMBIOS (TSMBIOS) (https://code.google.com/p/tsmbios/) (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: better opsi 4.0 compatibility by investigating the server response (Detlef Oertel <d.oertel@uib.de>)
- osurlparser: createUrl: initialize uriobj.HasAuthority (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- uSMBIOS: with fix https://github.com/RRUZ/tsmbios/issues/15 (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix %Systemroot% and %SystemDrive% for terminal servers ; references #4711 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.35] - 2022-01-04

### Added

- osparser: new function getMSVersionName: string (Detlef Oertel <d.oertel@uib.de>)
- osprocesses: new isProcessChildOf(searchproc,parentproc: string): boolean; (Detlef Oertel <d.oertel@uib.de>)
- new boolean opsi-script function: isProcessChildOf(\<searchprocstr>, \<parentprocstr>): bool;
  implemented for WLM (Detlef Oertel <d.oertel@uib.de>)
- new opsi-script constant %opsiscriptprocname% represents the name of the
  just running opsi-script process (opsi-script.exe, opsi-script, opsi-script-gui)
  changes in opsiconf, osparser, opsiscript.lpr (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: executeWith: AllSigned Hack: use:
  "Get-Content -Path '+ tempfilename +' | Out-String | Invoke-Expression" (Detlef Oertel <d.oertel@uib.de>)
- oslinmount: replace getGeneralConfigValue by configState_getObjects (Detlef Oertel <d.oertel@uib.de>)
- write now component logs to subdir 'lastprodlogs' (Detlef Oertel <d.oertel@uib.de>)
- osprocesses: isProcessChildOf also for win64 (Detlef Oertel <d.oertel@uib.de>)
- osGUIcontrol: skindirectoryDevelopment = '../../../skin'; (Detlef Oertel <d.oertel@uib.de>)
- osGUIcontrol: Linux: skindirectoryCompatibility = '/opt/opsi-script/skin'; (Detlef Oertel <d.oertel@uib.de>)

### Removed

- osparser: getMSVersionMap: remove double ReleaseID= (Detlef Oertel <d.oertel@uib.de>)
- osparser: executewith: powershell: remove a trailing '-file' from programparas (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- oswebservice: sendlog: fix boolean values for append at log_write (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: getProfilesDirListLin fix non standard lines from getent passwd (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.34] - 2021-12-15

### Added

- oslog: new property WriteComponentFile. if true a logfile for every component (product)
  is (re)written. (Detlef Oertel <d.oertel@uib.de>)
- osconf: new config: opsi-script.global.writeProductLogFile (default=false) (Detlef Oertel <d.oertel@uib.de>)
- osparser: opsiservicecall: new line allowed: "timeout" : \<int> (Detlef Oertel <d.oertel@uib.de>)

### Changed

- oszip: catch exceptions (Detlef Oertel <d.oertel@uib.de>)
- powershell AllSigned Hack: type tmpfile | powershell -command -
  will be switched on automatically if AllSigned is detected.
  Additional parameters (to script or powershell) will be ignored in this case. (Detlef Oertel <d.oertel@uib.de>)
- oslog: changed histrory line to:
  'handled: \<productid> Version: \<versiion> Request: \<request> Result: \<result> (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: TOpsiMethodCall has now property timeout (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: fix callingsection parameter in call of SearchForSectionLines
  from producestringlist:getOutstreamFromSection / getReturnlistFromSection (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.33] - 2021-12-03

### Changed

- osparser: more debug logs in getFileInfoMap (Detlef Oertel <d.oertel@uib.de>)
- osparser: getFileInfoMap: catch exceptions (Detlef Oertel <d.oertel@uib.de>)
- versionx: catch exceptions in GetString (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.32] - 2021-11-19

### Added

- added unit oscertificates (Detlef Oertel <d.oertel@uib.de>)
- new void function ImportCertToSystem(\<cert file name>) (Detlef Oertel <d.oertel@uib.de>)
- oscertificates: add functions for linux /mac (Detlef Oertel <d.oertel@uib.de>)
- ostxstinglist: new method: getStringValueWithDefault (Detlef Oertel <d.oertel@uib.de>)
- new: stringlist literals in json style are allowed now (set $list$ = '["he","ho"]') (Detlef Oertel <d.oertel@uib.de>)
- new: allow after DefVar inital value: defvar $str$ = "huhu" (Detlef Oertel <d.oertel@uib.de>)
- new: allow after DefStringlist inital value: defstringlist $list$ = '["he","ho"]' (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: files section: unzip create target dir (Detlef Oertel <d.oertel@uib.de>)
- GetProductProperty: if no service context, tries to find property in %scriptpah%/properties.conf (Detlef Oertel <d.oertel@uib.de>)
- GetProductPropertyList: if no service context, tries to find property in %scriptpath%/properties.conf (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osfunclin: getPackageLock: fix for zypper, general fix (Detlef Oertel <d.oertel@uib.de>)
- fix overwrite GUI messages by standard skinreload from  doAktionen (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: getSubListResult: fix for access violation in getproductpropertylist (Detlef Oertel <d.oertel@uib.de>)
- osjson: jsonAsArrayToStringList: fix '[]' is empty array (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.31] - 2021-11-16

### Changed

- osfunc: AllCopyRecursive: for better performance:
  doNotChangeAccessRights: boolean = True; (Detlef Oertel <d.oertel@uib.de>)
- osparser: executeWith + powershellcall: enable runasloggedonuser (Detlef Oertel <d.oertel@uib.de>)
- osfilehelper: MakeBakFiles: correct format mask for log backups (Detlef Oertel <d.oertel@uib.de>)
- osparser: files section: move / rename: allow also for directories (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.30] - 2021-10-27

### Fixed

- osfuncwin: fixed that opsi-script crashed if Windows API function GetFirmwareType is not available in kernel32.dll
  (for example on Windows 7/ Windows Server 2008) (Jan Werner <j.werner@uib.de>)

## [4.12.4.29] - 2021-10-18

### Changed

- osfuncwin: WinIsUefi uses new function GetBiosMode if Windows Verison >= Windows10.
  This function calls the Windows API function GetFirmwareType to detect Bios mode (Detlef Oertel <d.oertel@uib.de>)
- osparser: doLDAPSearch: callLDAP: if port 636 then use ssl (Detlef Oertel <d.oertel@uib.de>)
- osfilehelper: MakeBakFiles: use two digit rotation numbers if log_rotation_count > 9
  use three digit rotation numbers if log_rotation_count > 99 (Detlef Oertel <d.oertel@uib.de>)
- osconf: configuration: log_rotation_count (default = 8)  (max=999
  will need new config: opsi-script.global.log_rotation_count to change (allowed 0 - 999) (Detlef Oertel <d.oertel@uib.de>)

### Removed

- remove ldapsend.pas from opsi-script dir (use from synapse) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osfuncwin: getW10Release: Fixes detection of w10 release > 2009 (21h1) ; fixes #4665
  getW10Release is now also called from osparser (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.28] - 2021-08-31

### Added

- osparser: do XMLPatch2Main: new xml2-section commands:
  - setNodePair \<keyNodeName> \<keyNodeTextContent> \<valueNodeName> \<valueNodeTextContent>
  used for Apple info.plist (Detlef Oertel <d.oertel@uib.de>)
- osxmlsections: TuibXMLDocument: new:
  setNodePair(keyNodeName: string; keyNodeTextContent: string;
  valueNodeName: string; valueNodeTextContent: string): boolean; (Detlef Oertel <d.oertel@uib.de>)

### Changed

- Execwith: if there is a parameter '/HookScript \<path to script file>',
  then the existing 'script file' will be called with the temporary file
  as parameter before passing it to the interpreter.
  The call of the hookscript has a 40 sec Timeout (Detlef Oertel <d.oertel@uib.de>)
- osfunc: additional log at FileCheckDate (Detlef Oertel <d.oertel@uib.de>)
- DPI awareness True/PM_V2 (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: FBatchOberflaeche.LoadSkin(''); and Processmess to reload DPI scaling (Detlef Oertel <d.oertel@uib.de>)
- osparser: execPowershellCall has now additionl parameter optionstr that may hold additional
  execwith parameters (Detlef Oertel <d.oertel@uib.de>)
- osparser: getDecimalCompareSign (used by compareDotSeperatedNumbers):
  now support leading zeros (then it will handled as float starting with 0.)
  now support a letter as last part of the number (1.0.23b like is used at openssl versions) (Detlef Oertel <d.oertel@uib.de>)
- osparser: executewith: call powershell scripts with -file parameter in order to get the correct exitcode (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: fix Access Violation in CreateAndProcessScript on empty scriptfiles. (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.27] - 2021-08-12

### Added

- new generic section parameter /AllUserProfiles (as alias for /AllNTUserProfiles) (Detlef Oertel <d.oertel@uib.de>)
- new unit oswinacl with function AddAccessRightsToACL (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: doXMLPatcg2Main: check acces rights and for successful savetofile (Detlef Oertel <d.oertel@uib.de>)
- osparser: handleFileExists32: fileexists for win also via shlwapi.PathFileExistsW (Detlef Oertel <d.oertel@uib.de>)
- osfunc: FileGetWriteAccess calls at windows also AddAccessRightsToACL (Detlef Oertel <d.oertel@uib.de>)
- osencoding: saveTextFileWithEncoding: ForceDirectories for output (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osxmlsections: fixes for creating not existing file (Detlef Oertel <d.oertel@uib.de>)
- osfunc: toCopyOrNotToCopy: fix for copy -r (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.26] - 2021-07-27

### Added

- osencoding: new function  isPlainAsciiString(const searchText: string): boolean; (Detlef Oertel <d.oertel@uib.de>)
- osencoding: getFileBom: added fmShareDenyNone to open mode (Detlef Oertel <d.oertel@uib.de>)

### Changed

- Parameter 'pcprofil' not supported any more (Detlef Oertel <d.oertel@uib.de>)
- osencoding: func searchEncoding now detectd also isPlainAscii (Detlef Oertel <d.oertel@uib.de>)
- osparser: supress any encoding Warnings if isPlainAscii (Detlef Oertel <d.oertel@uib.de>)
- osencoding: getFileBom: 3 retries (Detlef Oertel <d.oertel@uib.de>)
- osparser: tsImportLib: do not reload the file at 'Imported all functions from file' (Detlef Oertel <d.oertel@uib.de>)
- osparser: tsecExpected: (external sub sections): try-exept getFileBom +  LoadValidLinesFromFile (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.25] - 2021-07-20

### Changed

- switch to Lazarus 2.2 / FPC 3.2.2 (Detlef Oertel <d.oertel@uib.de>)
- osparser: include_append:  comment out 'encoding=' lines from include_append because
  this line can become a part of a secondary section (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.24] - 2021-07-05

### Added

- new: xml2 sections: /encoding Parameter (Detlef Oertel <d.oertel@uib.de>)
- new: xml2 sections: /AllNTUserProfiles Parameter (Detlef Oertel <d.oertel@uib.de>)
- xml2 sections: new command: rootNodeOnCreate = \<node> (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osfunc: file functions use findfirstUTF8 instead of findfirst (Detlef Oertel <d.oertel@uib.de>)
- xml2 sections: many log warnings lowered to LLInfo (Detlef Oertel <d.oertel@uib.de>)
- xml2 sections: not existing files will be created
  using the value of rootNodeOnCreate, fallback to 'rootnode' if not existing (Detlef Oertel <d.oertel@uib.de>)
- osparser: less encoding reload in LoadValidLinesFromFile (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: include (append / insert) statements fixes on read file with encoding (Detlef Oertel <d.oertel@uib.de>)
- osparser: importlib statements fixes on read file with encoding (Detlef Oertel <d.oertel@uib.de>)
- osencoding: more logging and retries at loadTextFileWithEncoding in order to fix
  'file in use' error. (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.23] - 2021-06-08

### Added

- new constant %opsiserviceClientId% (FQDN in service context, empty without) (Detlef Oertel <d.oertel@uib.de>)
- new parameter /depotpath (only together with /opsiservice) to pass the path to the depot (Detlef Oertel <d.oertel@uib.de>)
- New cli parameter /ServiceBatch  if /serviceBatch then also needed:
  /ClientId \<> /opsiservice \<> /username \<> /password \<> /productId \<> and \<script path> (Detlef Oertel <d.oertel@uib.de>)
- New config opsi-script.global.supressSystemEncodingWarning
  if true then the following warning will be suppressed:
  'Encoding=system makes the opsiscript not portable between different OS' (Detlef Oertel <d.oertel@uib.de>)

### Changed

- parameter /clientid if present wil be ussed as preferred source for %hostid% (Detlef Oertel <d.oertel@uib.de>)
- linux opsi-script library search path:
  %scriptpath%                     // standard by definition
  %ScriptPath%/../lib              // standard by definition
  './lib'                          // new standard directory in opsi-script directory
  '/usr/share/opsi-script/lib'     // backward compatibility directory (Detlef Oertel <d.oertel@uib.de>)
- linux skin directory search path:
  '/usr/share/opsi-script/customskin'  // custom skin directory
  './skin'                          // new standard skin directory in opsi-script directory
  '/usr/share/opsi-script/skin'     // backward compatibility directory (Detlef Oertel <d.oertel@uib.de>)
- linux locale directory search path:
  './locale'                        // new standard locale directory in opsi-script directory
  '/usr/share/locale/....'          // backward compatibility directory (Detlef Oertel <d.oertel@uib.de>)
- removing links to http://www.opsi.org/credits/ (Detlef Oertel <d.oertel@uib.de>)
- osinteractivegui: set copyright tro 1995-2021 (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin2: replace of winst32.exe in funcwin2 by ExtractFileName(ParamStr(0) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osfuncwin2: fix for function resolveWinSymlink (Detlef Oertel <d.oertel@uib.de>)
- Fixes detection of w10 release > 2009 (21h1) (osparser) ; fixes #4665 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.22] - 2021-06-04

### Added

- new (common) unit osfilehelper (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osfunc: function makbakfile with two parameters (name,maxbak number) is moved as makbakfiles
  to osfilehelper (Detlef Oertel <d.oertel@uib.de>)
- oslog: function makbakfile with two parameters (name,maxbak number) is moved as makbakfiles
  to osfilehelper (Detlef Oertel <d.oertel@uib.de>)
- osfunc files section copy: moving some log messages from Loglevel 6 to 7 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.21] - 2021-05-25

### Added

- new function resolveSymlink(\<string>) : string; on error it returns input (Detlef Oertel <d.oertel@uib.de>)
- new constant %realScriptpath% ; if the script is called via symlink
  then it is the resolved version of %scriptpath% (Detlef Oertel <d.oertel@uib.de>)
- new common unit osfuncunix (Detlef Oertel <d.oertel@uib.de>)
- new function forcePathDelims(\<path string>) : \<path string>
  sets all slash and backslash as pathdelim according to the OS (Detlef Oertel <d.oertel@uib.de>)
- new boolean fileIsSymlink(FilePath: string): boolean; (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osfunc: function isSymLink(const filepath : string) : boolean; (Detlef Oertel <d.oertel@uib.de>)
- osfuncunix: function resolveUnixSymlink(const filepath : string; recursive : boolean = true) : string; (Detlef Oertel <d.oertel@uib.de>)
- osparser: function resolveWinSymlink(const filepath : string; recursive : boolean = true) : string; (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.20] - 2021-05-06

### Fixed

- again: fix debug_lib do imported libs added to nologfiles in oslog (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.19] - 2021-05-05

### Fixed

- osparser: fix fileexists, fileOrFolderExists, DirectoryExists with quoted filenames (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.18] - 2021-04-14

### Changed

- osparser: doAktionen: executesection: call sections with catchout=true
  fixes: nooutput in log and showoutput (Detlef Oertel <d.oertel@uib.de>)
- osfuncmac: getProfilesDirListMac implemented (Detlef Oertel <d.oertel@uib.de>)
- osmain: allow /batch with /productid and opsi-service parameters (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osfunc: fix for getOSArchitecture at windows 64 bit (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.17] - 2021-03-31

### Changed

- change getFileBom to fileHasBom(\<filename>) : boolean (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.16] - 2021-02-26

### Added

- new string function: getOSarchitecture (x86_32 / x86_64/ arm_64) (Detlef Oertel <d.oertel@uib.de>)
- new boolean function: runningInWAnMode (true if opsiserver = localhost) default=false (Detlef Oertel <d.oertel@uib.de>)

### Changed

- can now handle \*.opsiscript files with bom (Detlef Oertel <d.oertel@uib.de>)
- call of execwith section with /encoding parameter (by j.laajili) (Detlef Oertel <d.oertel@uib.de>)
- change function asConfidentialList(\<list>) : stringlist
  to function asConfidential(\<list>) : stringlist (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.15] - 2021-02-12

### Added

- new function asConfidentialList(\<list>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- new GUI Interface implementation (Detlef Oertel <d.oertel@uib.de>)

### Changed

- some additional try excet in osjson and load testfile variants (Detlef Oertel <d.oertel@uib.de>)
- osGUIControl: unify skin directories to 'skin' with fallback to custom\winstskin at windows (Detlef Oertel <d.oertel@uib.de>)
- standard out file at windows now opsi-script.exe (Detlef Oertel <d.oertel@uib.de>)
- locale is now opsi-script.po (also at windows) (Detlef Oertel <d.oertel@uib.de>)
- winstxx.exe only as symbolic links to opsi-script.exe (for backward compatibility) (Detlef Oertel <d.oertel@uib.de>)
- osparser: executesection: shellinanIcon: logleveloffset=0 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.14] - 2021-01-29

### Added

- new boolean function: fileorfolderExists(\<file name>, [\<access arch>]) (by j.laajili) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- redesign of TXStringlist file in and out encoding  (by j.laajili) (Detlef Oertel <d.oertel@uib.de>)
- call of patches section with /encoding parameter (by j.laajili) (Detlef Oertel <d.oertel@uib.de>)
- call of patchTextFile section with /encoding parameter  (by j.laajili) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osregistry: open Registry with KEY:READ instead of KEY_ALL_ACCESS. Thanks to Jens Boettge. ; fixes  #4625 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.13] - 2021-01-25

### Added

- osfunc: new: TUibIniFile.ReadRawSection (by j.laajili) (Detlef Oertel <d.oertel@uib.de>)
- new function getSectionFromIniFile based on ReadRawSection (by j.laajili) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osxmlsections: fix for xml2 delnode (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.12] - 2020-12-16

### Changed

- repair center batchgui on screen (Detlef Oertel <d.oertel@uib.de>)
- osregex: another empty string regex test (Detlef Oertel <d.oertel@uib.de>)
- license functions: opsiserviceuser if possible (Detlef Oertel <d.oertel@uib.de>)
- linux: shellBatch implemented as shellInAnIcon (Detlef Oertel <d.oertel@uib.de>)
- macos: osfunclin: getmyipbytarget: use path to ip command (Detlef Oertel <d.oertel@uib.de>)
- macos: runningWithGui also working for macos (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.11] - 2020-12-14

### Changed

- files: copy: deny-list with '.DS_Store' (Detlef Oertel <d.oertel@uib.de>)
- files: on 'permission denied' call handle.exe if existing in opsi-script directory (Detlef Oertel <d.oertel@uib.de>)
- dosInAnIcon: if win7 and no new Arguments then call runAndCaptureOut (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.10] - 2020-11-26

### Changed

- macos: modify locale path for opsi-script-gui.app (Detlef Oertel <d.oertel@uib.de>)
- macos / linux: oscheck_gui_startable: kill opsi-laz-gui-test if not terminated (Detlef Oertel <d.oertel@uib.de>)
- osregex: filter empty input to avoid execeptions (Detlef Oertel <d.oertel@uib.de>)
- macos: shellBatch implemented as shellInAnIcon (Detlef Oertel <d.oertel@uib.de>)
- osnetworkcalculator: net gethostbyname(\<ip>) (Detlef Oertel <d.oertel@uib.de>)
- osparser: GetIpByName: use new gethostbyname (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.9] - 2020-11-25

### Removed

- GetSectionFromIniFile: remove encoding transformation (windows only) (Jan Werner <j.werner@uib.de>)

## [4.12.4.8] - 2020-11-24

### Changed

- isRegexMatch: checks if string is NOT empty before exec is executed
  catchs exceptions (Jan Werner <j.werner@uib.de>)

## [4.12.4.6] - 2020-11-04

### Changed

- import osprocesses (Detlef Oertel <d.oertel@uib.de>)
- call of opsi-laz-gui-test in extra log: ogdatei.StandardMainLogPath+'opsi-script-gui-test.log' (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.5] - 2020-11-04

### Changed

- the one and only project file is now opsi-script.lpi
  you have to switch the build mode for different archtictures (win / mac / lin) (gui) (Detlef Oertel <d.oertel@uib.de>)
- the opsi-script nogui version is now opsi-script (Detlef Oertel <d.oertel@uib.de>)
- the opsi-script gui version is now opsi-script-gui (Detlef Oertel <d.oertel@uib.de>)
- opsi-script tests via opsi-laz-gui-test if a gui can be used
  and if yes calls opsi-script-gui (via execv) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.4] - 2020-10-13

### Added

- new boolean function: isValidFQDN (Detlef Oertel <d.oertel@uib.de>)
- osfunc: new randomstrWithParameters (also called by randomstr (Detlef Oertel <d.oertel@uib.de>)
- new string function randomstrWithParameters(minLength,nLowerCases,nUpperCases,nDigits,nSpecialChars : Integer): string; (Detlef Oertel <d.oertel@uib.de>)

### Changed

- powershellcall and execwith calls at windows RunAndCaptureOut bydefault (Detlef Oertel <d.oertel@uib.de>)
- Linux: osfunc: Findfirst with or faSymlink (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix debug_lib do imported libs added to nologfiles in oslog (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.3] - 2020-09-23

### Added

- new unit osstartprocess_cp (from osfunc, will be used in opsi-script-pilot) (Detlef Oertel <d.oertel@uib.de>)
- new del option -r (retryOnReboot) default false (Windows only)
  If -r is set a missed delete action will be retried on Reboot.
  In this case normally a 'reboot after this script' flag will be set.
  By using the -c option setting this flag will be supressed
  The option -c (continue) make only sense in combination with -r (Detlef Oertel <d.oertel@uib.de>)

### Changed

- macos: integrate to opsiscript and opsiscriptnogui project
  (Remove mac project files) (Detlef Oertel <d.oertel@uib.de>)
- more logging on alldelete (Detlef Oertel <d.oertel@uib.de>)
- repair del -c (Detlef Oertel <d.oertel@uib.de>)
- try to remove dirs also with copy_delay (Detlef Oertel <d.oertel@uib.de>)
- section name now as header of showoutput (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- startprocess_cp fix (jan werner) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.2] - 2020-09-15

### Changed

- macos: try to read mountpoint from opsiclientd.conf
  default to /var/opsisetupadmin/opsi_depot (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.1] - 2020-08-27

### Changed

- some more log on startup (Detlef Oertel <d.oertel@uib.de>)
- oslog: makebakfiles: rename instead of copy (much faster) (Detlef Oertel <d.oertel@uib.de>)
- macos: change mountpoint back to /var/opsisetupadmin/opsi_depot (Detlef Oertel <d.oertel@uib.de>)
- osbatchgui: formresize function disabled for Darwin (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix uefi detection on win10 >= release 1004 (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin: WinIsUefi: the fix is here
  //\* macos: change mountpoint to /Network/opsi_depot
  //\* macos: changeback mountpoint to /Volumes/opsi_depot (Detlef Oertel <d.oertel@uib.de>)

## [4.12.4.0] - 2020-07-30

### Changed

- includes changes from github merge request "Feature / Implementation parity between WinBatch, DosBatch and ExecWith"
  - DosBatch-Sections to be executed with elevated privileges like WinBatch could be using /RunElevated
  - /Run\*-Parameters for ExecWith
  - Output catching, /showouput and getOutStreamFromSection support for WinBatch (Jan Werner <j.werner@uib.de>)
- compiled with lazarus 2.0.10 (Jan Werner <j.werner@uib.de>)
- osparser: execDosBatch: additional parameters now allowed for shellInAnIcon &co:
  /runElevated
  /TimeoutSeconds \<number>
  /WaitForProcessEnding \<string>
  /LetThemGo (Jan Werner <j.werner@uib.de>)

### Fixed

- fix for /showoutput at win64 (Jan Werner <j.werner@uib.de>)
- fix for runningonuefi if w10 release >= 2004 (Jan Werner <j.werner@uib.de>)

## [4.12.3.18] - 2020-07-30

### Changed

- UTF8String replaced by String (Jan Werner <j.werner@uib.de>)
- AnsiToUTF8() removed accordingly (Jan Werner <j.werner@uib.de>)
- compiled with lazarus 2.0.10 (Jan Werner <j.werner@uib.de>)

## [4.12.3.17] - 2020-07-23

### Changed

- osfunc: StartProcess_cp: log message: 'Started process' now in utf8 encoding (Detlef Oertel <d.oertel@uib.de>)
- osfunc: StartProcess_cp: use filename and parameters (Detlef Oertel <d.oertel@uib.de>)
- compiled with lazarus 2.0.10 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.16] - 2020-06-19

### Changed

- oswebservice:sendlog: Log that using default if getLogsize failed (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin: some logprog in WinIsUefi (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin:  getW10Release (Detlef Oertel <d.oertel@uib.de>)
- osparser: good bye to cmd64.exe (Detlef Oertel <d.oertel@uib.de>)
- osfunc: alldelete: change loglevel for not deleted files (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osfuncwin: WinIsUefi: fix uefi detection if w10 release >= 2004 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.15] - 2020-06-09

### Changed

- oswebservice: avoid double /rpc in service url; (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: better getProfilesDirListLin (Detlef Oertel <d.oertel@uib.de>)
- osparser: doTextpatch: Do not crash on not creatable files (Detlef Oertel <d.oertel@uib.de>)
- osprocessess: ProcessIsRunning: more info in warning (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.14] - 2020-06-04

### Added

- osparser: new flag: cmd64checked to avoid repeated checks for cmd64.exe (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osmain: bool var runSilent used in cli parameter check and to call
  FBatchOberflaeche.setVisible(True/False); (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osbatchgui: start FBatchOberflaeche visible=false ; fixes #4485 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.13] - 2020-03-12

### Changed

- osmain: bootmode: now using opsiclientagentconf (Detlef Oertel <d.oertel@uib.de>)
- linux: osprocesses: ProcessIsRunning: look for exact match in 15 char shortcmd (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: os_shutdown() (Detlef Oertel <d.oertel@uib.de>)
- osmain: (linux) try to direct reboot or shutdown (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.12] - 2020-03-10

### Changed

- osmain: /batch + / productid (template+with admin):
  - if no connection to real server, retry with localhost
  - if no connection to localhost - run any way (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.11] - 2020-02-26

### Added

- oswebservice: new function macosAgentActivated: boolean; (Detlef Oertel <d.oertel@uib.de>)
- Info-os.plist: add opsi.icns file (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osmain: on macos: check macosAgentActivated (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.10] - 2020-02-03

### Added

- osfunc / osparser: linkfolder: new link element window_state with the allowed values 'normal', 'min', 'max' (Detlef Oertel <d.oertel@uib.de>)

### Changed

- activate history logging again (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.9] - 2020-01-28

### Added

- new statement: executeSection( \< string expr with section call>)
  can call the following section types:
  - winbatch
  - registry
  - DosBatch, DosInAnIcon, ShellBatch, ShellInAnIcon
  - ExecWith, ExecPython
  - Files (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function in osparser (producestringlist):
  setValueByKey(\<key str>, \<value str>, \<target list> [, \<separator str (char)>]) : stringlist (Detlef Oertel <d.oertel@uib.de>)

### Changed

- winbatch combine /timeoutseconds with WaitForWindowAppearing or WaitForWindowVanished
  done this in osfunc: StartProcess_cp (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix some of the last line problems at defined functions ; fixes #4378 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.8] - 2020-01-27

### Fixed

- fix: osparser: doAktionen: do not double backslashes in sub_sections
  ApplyTextConstantsToString (Remaining, false); (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.7] - 2020-01-14

### Changed

- skiped (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.6] - 2020-01-09

### Added

- new statement: forceLogInAppenMode = (true/false) (Detlef Oertel <d.oertel@uib.de>)
- new boolean function isPingReachable(\<host>) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- RequiredOpsiscriptVersion (Detlef Oertel <d.oertel@uib.de>)
- %OpsiscriptVersion% (Detlef Oertel <d.oertel@uib.de>)
- %OpsiscriptDir% (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.5] - 2020-01-06

### Added

- new procedure reverseProductOrderByUninstall: ProductList is reordered so that
  uninstall actions will be conducted first and in reverse order as the prodcuts were installed (Jan Werner <j.werner@uib.de>)
- new config opsi-script.global.ReverseProductOrderByUninstall (Jan Werner <j.werner@uib.de>)

### Changed

- postinst script was adapted for the new config opsi-script.global.ReverseProductOrderByUninstall (Jan Werner <j.werner@uib.de>)

## [4.12.3.4] - 2019-12-27

### Added

- new unit ostxstringlist (Detlef Oertel <d.oertel@uib.de>)
- new string function which(\<command in path>) : string (command with path) [W/L/M] (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function replaceOpsiConstants(\<list>) : \<list> (Detlef Oertel <d.oertel@uib.de>)
- new string function replaceOpsiConstants(\<string>) : \<string> (Detlef Oertel <d.oertel@uib.de>)
- new boolean function isPingReachable(\<target host>) : bool (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osfunclin: which() now using FindDefaultExecutablePath (Detlef Oertel <d.oertel@uib.de>)
- osfunc: StartProcess_cp: now supports WaitForProcessEnding also for [L/M] (Detlef Oertel <d.oertel@uib.de>)
- Winbatch now supports WaitForProcessEnding also for [L/M] (Detlef Oertel <d.oertel@uib.de>)
- osparser: create cmd64.exe in execshellcall and execdosbatch:
  create also cmd64.exe.mui files (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.2] - 2019-11-19

### Fixed

- osProcessux: getCommandResult: give always the first string ; fixes a getmyip Problem (Detlef Oertel <d.oertel@uib.de>)

## [4.12.3.1] - 2019-11-15

### Changed

- update function listFiles: including switch for disabling redirections under 64bit-Windows (Jan Werner <j.werner@uib.de>)

## [4.12.3.0] - 2019-10-23

### Added

- new StringList function listFiles (Jan Werner) (Detlef Oertel <d.oertel@uib.de>)
- new boolean function: runningWithGui (osparser,osfunc) (at win+Mac always true) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- update oswebservice with gzip support (Jan Werner) (Detlef Oertel <d.oertel@uib.de>)
- osbatchgui: log on setWindowstate (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: move logging to log_prog (Detlef Oertel <d.oertel@uib.de>)
- osparser info to macos version at log start (Detlef Oertel <d.oertel@uib.de>)
- osparser: Linux: GetIpByName: now uses 'getent hosts' (Detlef Oertel <d.oertel@uib.de>)
- macos: osbatchgui: temporary disable setWindowState (Detlef Oertel <d.oertel@uib.de>)

## [4.12.2.3] - 2019-08-29

### Added

- new library uib_state_and_condition:
  Collection of functions that gives info about system states that are relevant for product installations (Detlef Oertel <d.oertel@uib.de>)
- new library uib_state_and_condition:
  Collection of functions that helps to install driver (Detlef Oertel <d.oertel@uib.de>)

### Changed

- feature/macos integrated (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: replace constants for every primary section line to avoid
  replacment in secondary sections (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: differences between %CurrentProfileDir% and %UserProfileDir% fixes: #4118 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.2.2] - 2019-08-06

### Changed

- oswebservice: switch most of log to log_prog (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: code cleanup (remove unused code) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.2.1] - 2019-08-06

### Changed

- oswebservice: sendlog: check max logsize to send via backend_getSystemConfiguration (Detlef Oertel <d.oertel@uib.de>)
- switch to Lazarus 2.0.4 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.2.0] - 2019-07-04

### Changed

- oswebservice: switch from indy to synapse (Detlef Oertel <d.oertel@uib.de>)
- osfunc: modified: ShrinkFileToMB (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: sendlog: shrinking enabled again (Detlef Oertel <d.oertel@uib.de>)
- adding changelog.txt to project (Detlef Oertel <d.oertel@uib.de>)

## [4.12.1.5] - 2019-06-06

### Added

- osconf: new const opsiclientagentconf (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osmain: freeLinuxAgentStart: now using opsiclientagentconf (Detlef Oertel <d.oertel@uib.de>)
- Linux: osparser: CreateAndProcess: more logging around remount (Detlef Oertel <d.oertel@uib.de>)
- Linux: osparser: CreateAndProcess: moved remount (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osdefinedfunctions: delLocalVar : fix removing loopvar after empty loop (Detlef Oertel <d.oertel@uib.de>)
- osmain: fix in GetTempPath: if Univention temppath is /var/tmp (Detlef Oertel <d.oertel@uib.de>)

## [4.12.1.4] - 2019-06-03

### Changed

- osfunc: more logging around gettemppath (Detlef Oertel <d.oertel@uib.de>)
- osparser: enabled getHWBiosInfoMap again
  Linux only:
  - osfunclin: getpackagelock: fix memory leak
  - osfunclin: new: function getLinuxDistroName : string;
  - osfunclin: new: function getLinuxDistroRelease : string;
  - osfunclin: new: function getLinuxDistroDescription
  - osmain: GetTempPath: if Univention temppath is /var/tmp
  - osparser: createandprocessscript: check for depotmount
  - osparser: createandprocessscript: give some distro infos in log (Detlef Oertel <d.oertel@uib.de>)

## [4.12.1.3] - 2019-05-14

### Added

- osfunclin: new function getMyIpDeciceByDefaultRoute (Detlef Oertel <d.oertel@uib.de>)

### Changed

- shellInAnIcon: on loglevel 9: reread teporary files and do not delete (Detlef Oertel <d.oertel@uib.de>)
- oslog: logdatei property usedloglevel (this is real used log level with force_min_loglevel) (Detlef Oertel <d.oertel@uib.de>)
- osparser: GetMACAddress2: use getMyIpDeciceByDefaultRoute (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: more logging in getMyIpDeciceByDefaultRoute, getMyIpByDefaultRoute (Detlef Oertel <d.oertel@uib.de>)
- osparser: demandlicensekey: try 3 ways to get the own fqdn (Detlef Oertel <d.oertel@uib.de>)
- osfunc: createtextfile: Message:'Directory ' + dirname + ' did not exist und was created' now on loglevel 6 (was warning) (Detlef Oertel <d.oertel@uib.de>)
- osfunc: StartProcess_cp_el: minor logging change ; //no {$OVERFLOWCHECKS ON} (Detlef Oertel <d.oertel@uib.de>)
- osfunc / osfuncwin: replace 'exitCode := lpExitCode;' by 'exitCode := longint(lpExitCode);' (Detlef Oertel <d.oertel@uib.de>)

## [4.12.1.2] - 2019-04-12

### Added

- add oslistedit to projectfiles (Detlef Oertel <d.oertel@uib.de>)
- add osinputstring (Detlef Oertel <d.oertel@uib.de>)
- add new string function stringinput(\< message str>,\< boolstr confidential>) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- document randomIntStr(\<>) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fixes in oslistedit (Detlef Oertel <d.oertel@uib.de>)
- fix on gettemppath (Detlef Oertel <d.oertel@uib.de>)

## [4.12.1.1] - 2019-04-04

### Fixed

- fix in oswmi (give stringlist as stringlist) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.1.0] - 2019-01-23

### Added

- new regex functions: (Detlef Oertel <d.oertel@uib.de>)
- new network functions (Detlef Oertel <d.oertel@uib.de>)
- new url related functions (Detlef Oertel <d.oertel@uib.de>)
- osparser: new function GetMacosVersionInfo : string (Detlef Oertel <d.oertel@uib.de>)
- osparser: new function getMacosVersionMap : stringlist (Detlef Oertel <d.oertel@uib.de>)
- osparser: new (undocumented) command 'setDebug_prog = (true/false)' (Detlef Oertel <d.oertel@uib.de>)
- osparserhelper: new function stringlistintersection (get stringlist intersection) (Detlef Oertel <d.oertel@uib.de>)
- new cli parameter /processproducts productId [,productId ]\* (Detlef Oertel <d.oertel@uib.de>)
- added unit oszip (Detlef Oertel <d.oertel@uib.de>)
- new in file sections: zipfile \<file or dir> \<zipfile> (Detlef Oertel <d.oertel@uib.de>)
- new in file sections: unzipfile \<zipfile> \<targetdir> (Detlef Oertel <d.oertel@uib.de>)

### Changed

- integration of new xml functions:
  - new units in common: osxml, osxmltdom, osxmlsections
  - new section type xml2
  - osparser: Tuibinstscript: new method doxml2patch:
  - OpenNode \<node path : string>
  - DeleteNode \<node path : string>
  - setNodeText \<text : string>
  - setAttribute \<name : string>, \<value:string>
  - addAttribute \<name : string>, \<value:string>
  - delAttribute \<name : string>
  - adNewNode \<name : string>
  - gotoParentNode
  - getXml2AttributeValueByKey(\<xml node:stringlist>,\<key:string>): string
  - getXml2Text(\<xml node:stringlist>) : string
  - getXml2DocumentFromFile(\<filename:string>) : \<xml-doc:stringlist>
  - getXml2Document(\<pure xml :stringlist>) : \<xml-doc:stringlist>
  - getXml2UniqueChildnodeByName
  - xml2GetFirstChildNodeByName(\<xml-doc:stringlist>,\<node name:string>) : \<xml node:stringlist>
  - xml2NodeExistsByPathInXMLFile(myfilename, path: string; attributes_strict: boolean): boolean; (Detlef Oertel <d.oertel@uib.de>)
- isRegexMatch(\<string>, \<pattern>) : boolean (Detlef Oertel <d.oertel@uib.de>)
- getSubListByContainingRegex(\<pattern>, \<target list>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- getSubListByContainingRegex(\<pattern list>, \<target list>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- getRegexMatchList(\<pattern>, \<target list>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- getRegexMatchList(\<pattern list>, \<target list>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- removeFromListByContainingRegex(\<pattern>, \<target list>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- removeFromListByContainingRegex(\<pattern list>, \<target list>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- stringReplaceRegex(\<string>, \<pattern>, \<replacement string>) : string (Detlef Oertel <d.oertel@uib.de>)
- stringReplaceRegexInList(\<target list>, \<pattern>, \<replacement string>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- isValidIP4 (\<ip4adr>) : boolean (Detlef Oertel <d.oertel@uib.de>)
- isValidIP4Network (\<ip4adr>, \<netmask>) : boolean (Detlef Oertel <d.oertel@uib.de>)
- isValidIP4Host (\<ip4adr>, \<netmask>) : boolean (Detlef Oertel <d.oertel@uib.de>)
- getIP4NetworkByAdrAndMask (\<ip4adr>, \<netmask>) : string (Detlef Oertel <d.oertel@uib.de>)
- getDefaultNetmaskByIP4adr (\<ip4adr>) : string (Detlef Oertel <d.oertel@uib.de>)
- parseUrl(\<url string>) : stringlist (Detlef Oertel <d.oertel@uib.de>)
- createUrl(\<urlcomponents list>) : string (Detlef Oertel <d.oertel@uib.de>)
- macos related: (Detlef Oertel <d.oertel@uib.de>)
- getos detect macos as 'macos' (Detlef Oertel <d.oertel@uib.de>)
- misc: (Detlef Oertel <d.oertel@uib.de>)
- ExtractFileExtension (`<path>`) : string [W/L] (Detlef Oertel <d.oertel@uib.de>)
- ExtractFileName (`<path>`) : string [W/L] (Detlef Oertel <d.oertel@uib.de>)
- DirectoryExists (`path`[,\<accessstr>]) : boolean [W/L] (Detlef Oertel <d.oertel@uib.de>)
- osshowsysinfo: caption is Systeminfo (instead of SystemInfo) (Detlef Oertel <d.oertel@uib.de>)
- reencode(paramstr(0),'system') (Detlef Oertel <d.oertel@uib.de>)
- replace unit osxbase64 by fpc unit base64 (Detlef Oertel <d.oertel@uib.de>)
- Release now compiled with activated Checks for IO, Range, Overflow and Stack (Detlef Oertel <d.oertel@uib.de>)
- unit oscrypt with {$RANGECHECKS OFF} (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunction: isvisiblelocalvar: serach optimization (Detlef Oertel <d.oertel@uib.de>)
- osfunc: checkfileexists: use widechar/utf16 for filenames (Detlef Oertel <d.oertel@uib.de>)
- osparser: fileActionsMain: zipfile, unzipfile (Detlef Oertel <d.oertel@uib.de>)
- osparser: modified : getListFromWMI
  - `getListFromWMI(`\<wmi namespace str>,\<wmi class str>,\<property list>,\<condition str>`) : stringlist`
  - If \<wmi namespace str> is empty, it defaults to `root\cimv2`. (Detlef Oertel <d.oertel@uib.de>)

### Removed

- remove some svn entries (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparserhelper: fix in geouterfunction (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix in Hasminimumspace (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix searchpath from %opsiScriptHelperPath% to %opsiScriptHelperPath%\lib; fixes #3975 (Detlef Oertel <d.oertel@uib.de>)
- osparser: producestringlist: createstringlist: fix: empty strings as argument allowed
  and createstringlist() gives empty list (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.36] - 2019-02-13

### Changed

- osdefinedfunctions: better errormessages (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: complete redesign of local var (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: now works also with recursive calls (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- oslog: fix: cleanup old partlogs (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix in md5sumfromfile (Linux) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.35] - 2019-01-15

### Changed

- osparser: producestringlist: getsublist: with whitespaces around numbers (Detlef Oertel <d.oertel@uib.de>)
- osmain: Loginscripts: do not break login script loop on reboots (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: getMapOfLoginscripts2Run: allscripts=false:
  we want to run the login script if installed
  or last successful action was uninstall (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.34] - 2019-01-10

### Changed

- osparser: producestringlist: getsublist: with string expression instead of numbers (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: producestringlist: createstringlist: fix: empty strings as argument allowed (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.33] - 2018-12-17

### Added

- osconf: new config opsi-script.global.w10BitlockerSuspendOnReboot (Detlef Oertel <d.oertel@uib.de>)
- osconf: new global config variable: w10BitlockerSuspendOnReboot (default= false) (Detlef Oertel <d.oertel@uib.de>)
- new units oswmi, utilwmi (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osmain: buildPC: >=win 10 and  w10BitlockerSuspendOnReboot and (txrImmediateReboot or txrImmediateReboot)
  then suspend bitlocker for 1 reboot     fixes: #3816 (Detlef Oertel <d.oertel@uib.de>)
- osparser: producestringlist:
  new function getListFromWMI(\<wmi class str>,\<property list>,\<condition str>) : stringlist
  example: set $plist$ = createStringList ('Model','Vendor')
  set $list$ = getListFromWMI('Win32_ComputerSystem',$plist$,'where model like 'Esprimo%')
  if \<property list> is empty all properties of \<wmi class str> will be returned
  on error a empty list will be returned (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.32] - 2018-10-25

### Changed

- osconf: no more OpsiServiceConfigs: in log (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.31] - 2018-10-17

### Added

- new paths for Linux:
  osconf: opsiscriptconfinit = '/etc/opsi-script/opsi-script.conf';
  osbatchgui: skindirectoryDefault = '/usr/share/opsi-script/skin';
  osbatchgui: skindirectoryCustomWin = '/usr/share/opsi-script/customskin';
  osparser: // search in /usr/share/opsi-script/lib
  osparser: '%opsiLogDir%' = '/var/log/opsi-script'
  oslog: StandardLogPath := '/var/log/opsi-script/'; (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osencoding: do not write out supported encodings; fixes #3700 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.30] - 2018-10-15

### Fixed

- osparser: fix for close log file after script finished:
  (only in interactive mode) (Detlef Oertel <d.oertel@uib.de>)
- oslog: all loglines trimright; fixes #3708 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.29] - 2018-09-25

### Removed

- osparser: doRegitryHack\*: remove HKCU on flag_all_usrclass (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.28] - 2018-09-14

### Changed

- osparser: move log message 'line is starting with' to log_prog (Detlef Oertel <d.oertel@uib.de>)
- osinteractivegui: radiobutton 'NewFile' visible (Detlef Oertel <d.oertel@uib.de>)
- osinteractivegui: at execute viewlist: use CreateTheLogfile (Detlef Oertel <d.oertel@uib.de>)
- osinteractivegui: at execute_button: use CreateTheLogfile (Detlef Oertel <d.oertel@uib.de>)
- osparser: close log file after script finished (Detlef Oertel <d.oertel@uib.de>)
- switch to lazarus 1.8.4. (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osconf: (fix): get ScriptErrorMessages, AutoActivityDisplay (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.27] - 2018-09-10

### Fixed

- osparser: (fix) registry: new parameter /AllUsrClassDats
  new function doRegistryAllUsrClassDats ; fix from Terry Strooband (thank you)
  flag_all_usrclass (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.26] - 2018-08-28

### Added

- osparser: registry: new parameter /AllUsrClassDats
  new function doRegistryAllUsrClassDats ; fix from Terry Strooband (thank you) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: more log_prog in folderlink and getstring (Detlef Oertel <d.oertel@uib.de>)
- osparser: reporterror: catch unknown origin and exceptions ; references #3655 (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: endif: catch exception in log message ; references #3655 (Detlef Oertel <d.oertel@uib.de>)
- osparser: execDosBatch: log8 temp file content (Detlef Oertel <d.oertel@uib.de>)
- osparser: executeWith: log8 temp file content (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: createSocket: log openssl version (Detlef Oertel <d.oertel@uib.de>)
- osparserhelper: GetOuterFunctionOrExp (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: execPowershellCall: log executing ; fixes #3621 (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: registry: Ignoring parameter /32bit for /AllNTUserdats. ; fixes #3643 (Detlef Oertel <d.oertel@uib.de>)
- osjson: jsonAsArrayToStringList: "[true]" now results to "true" ; fixes #3627 (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: getLinuxDistroType: read /etc/os-release  ; fixes #3651 (Detlef Oertel <d.oertel@uib.de>)
- OsDefinedFunction.parseCallParameter: handle functions as parameter ; fixes #3623 (Detlef Oertel <d.oertel@uib.de>)
- osparser: registerSectionOrigins: fix origin for external sub sections (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.25] - 2018-07-17

### Changed

- osconf: readConfigFromService: more debug and try-except with (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: function isConnected2(loglist : TStringlist): boolean; (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.24] - 2018-07-16

### Changed

- osconf: readConfigFromService: more debug and try-except (Detlef Oertel <d.oertel@uib.de>)
- update indy 5455 to 5464 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.23] - 2018-07-12

### Changed

- switch High DPI support off for Win7 (did not work correctly) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.22] - 2018-06-06

### Changed

- osparser: doAktionen: importlib; remove double call of
  Script.FSectionNameList.Add(importFunctionName) fixes #3560 (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- OsDefinedFunction.parseCallParameter: fixes #3569 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.21] - 2018-05-17

### Changed

- osconf: default for debuglib = true (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: additional log for indefFunc3 (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: inc/dec indefFunc3 only if keyword is at begining of line (Detlef Oertel <d.oertel@uib.de>)
- oswebservice connection timeout 20 sec not at linux (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.20] - 2018-05-14

### Added

- osparserhelper: GetWordOrStringConstant: new params search backwards=true;backwardfirst=false (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparserhelper: GetWord: search backwards=true;backwardfirst=true : / get from "hu)hu)))" as expr "hu)hu" with remaining ")))" (Detlef Oertel <d.oertel@uib.de>)
- osparserhelper: GetWord: search backwards=true;backwardfirst=false : / get from "hu)hu)))" as expr "hu)hu))" with remaining ")" (Detlef Oertel <d.oertel@uib.de>)
- osparser: producestringlist: do not delete remaining after call of defined function (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: parseCallParameter: better error messages (Detlef Oertel <d.oertel@uib.de>)
- osparser: executeWith: use autoactivitydisplay (Detlef Oertel <d.oertel@uib.de>)
- osparser: execShellCall: use autoactivitydisplay (Detlef Oertel <d.oertel@uib.de>)
- osparser: execPowerShellCall: use autoactivitydisplay (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osdefinedfunctions: parseCallParameter: trim paramstr ; fixes #3526 (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: call: set inDefFuncIndex on leaving function ; fixes #3528 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.19] - 2018-05-08

### Added

- osparser: new SearchForSectionLines (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: SearchForSectionLines used in produceStringlist and doAktionen (Detlef Oertel <d.oertel@uib.de>)
- oswebservice connection timeout 20 sec (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osdefinedfunctions: fix at free the local Vars (no $result$ if dfpVoid (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.18] - 2018-05-03

### Fixed

- oslog: fix logging defined function that are included (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.17] - 2018-04-13

### Added

- new unit osparserhelper in common (Skip / getword /  ...) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: processcall: log Executing on LLNotice (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: int new local string vars (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: isVisibleLocalVar check for empty varname (Detlef Oertel <d.oertel@uib.de>)
- osparser: catch powershellCall at Linux (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- OsDefinedFunction.parseCallParameter: fix handling parameters with ')' (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.16] - 2018-03-14

### Added

- new string function asConfidential:
  \1. backup and set loglevel to warning |2 get the input | 3 make it confidential
  | 4 give it to output |5 restore loglevel           #3441 (Detlef Oertel <d.oertel@uib.de>)
- new boolean function RegKeyExists(\<keystr> [,\<access str>])
  \<access str> = one of 32bit, 64bit, sysnative ; default sysnative   ; fixes #3078 (Detlef Oertel <d.oertel@uib.de>)
- new boolean function RegVarExists(\<keystr>, \<var str> [,\<access str>]) (Detlef Oertel <d.oertel@uib.de>)
- new boolean function isConfidential(\<str>)  for testing (Detlef Oertel <d.oertel@uib.de>)

### Changed

- typo of log messages (Detlef Oertel <d.oertel@uib.de>)
- change to Lazarus 1.8.2 (Detlef Oertel <d.oertel@uib.de>)
- switch HighDPI awareness on (8.1+, per monitor) (Detlef Oertel <d.oertel@uib.de>)
- osparser: reportError: more info on defined functions at syntax error (Detlef Oertel <d.oertel@uib.de>)
- osfunc: getsectionlines: include leading comments and empty lines (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions, osparser: allow defined functions with data  type void (Detlef Oertel <d.oertel@uib.de>)
- activate normal debug_lib again (see 4.12.0.13 temporary always true) (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: tsDefineFunction: try-excepts (Detlef Oertel <d.oertel@uib.de>)
- redesign of origin detection (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin2: reimplementation of getProfileImagePathfromSid
  https://forum.opsi.org/viewtopic.php?f=7&t=9941 (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin2: reimplementation of  delUserProfile (Detlef Oertel <d.oertel@uib.de>)
- osparser: more subsub search (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osdefinedfunctions, osparser: allow defined functions with no parameters; fixes #3182 (Detlef Oertel <d.oertel@uib.de>)
- oslog: fixed check for library logging (Detlef Oertel <d.oertel@uib.de>)
- osparser: reread opsi.conf parameters for every script; fixes #3337 (Detlef Oertel <d.oertel@uib.de>)
- osparser: splitstring: if string is confidential all parts are confidential as well; fixes #3186 (Detlef Oertel <d.oertel@uib.de>)
- osparser: splitStringOnWhiteSpace: if string is confidential all parts are confidential as well; fixes #3186 (Detlef Oertel <d.oertel@uib.de>)
- osfunc: FSaveToFile new param raise_on_error ; fixes #3253 (Detlef Oertel <d.oertel@uib.de>)
- found undocumented string function upper(\<str>); fixes #3283 (Detlef Oertel <d.oertel@uib.de>)
- no warning for not existing releaseid  ; fixes #2595 (Detlef Oertel <d.oertel@uib.de>)
- initvars in CreateAndProcessScript ; hopefully fixes errors on restart modified scripts (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: getpackagelock: look also for UCS ; fixes #3189 (Detlef Oertel <d.oertel@uib.de>)
- oslindesktopfile:  DeleteShellLink: del hard in /usr/share/applications/ ; fixes #3402 (Detlef Oertel <d.oertel@uib.de>)
- osparser: getregistryvalue(\<keystr>, \<varstr> [, \<access str>]) : string ; fixes #3279 (Detlef Oertel <d.oertel@uib.de>)
- osparser: powershellcall(\<commandstr> [,\<access str>='sysnative' [,\<policy bool str>='true']]) : [void,str,stringlist] ; fixes #3442 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.15] - 2018-03-09

### Changed

- oswebservice: getMapOfLoginscripts2Run: change logging to log_prog (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- oswebservice: getMapOfLoginscripts2Run: fix: check not only for productId but also for productVersion and PackageVersion (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.14] - 2018-02-13

### Added

- new function : getSubListByMatch(\<search string>, \<target list>) :stringlist (Detlef Oertel <d.oertel@uib.de>)
- new function : getSubListByMatch(\<search list>, \<target list>) :stringlist (Detlef Oertel <d.oertel@uib.de>)
- new function : getSubListByContaining(\<search string>, \<target list>) :stringlist (Detlef Oertel <d.oertel@uib.de>)
- new function : getSubListByContaining(\<search list>, \<target list>) :stringlist (Detlef Oertel <d.oertel@uib.de>)
- new function : getSubListByKey(\<search string>, \<target list>) :stringlist (Detlef Oertel <d.oertel@uib.de>)
- new function : getSubListByKey(\<search list>, \<target list>) :stringlist (Detlef Oertel <d.oertel@uib.de>)
- new function : getKeyList(\<list>) :stringlist (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: evaluateBoolean: sendlog: switch log append mode on
  in order to avoid that a final log removes this one (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.13] - 2017-12-27

### Added

- new function getIndexFromListByContaining(\<list> : stringlist,\<search string> : string) : \<number> : string (Detlef Oertel <d.oertel@uib.de>)
- new boolean function: runningInPE (osparser,osfunc,osfuncwin) (at linux always false) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- oslog: includelogtail: now log includelog at log level 7 (Detlef Oertel <d.oertel@uib.de>)
- temporary set debug_lib always to true (Detlef Oertel <d.oertel@uib.de>)
- osparser / osdefinedfunctions: call from a local function an other local function
  using a local variable as call by value parameter (Detlef Oertel <d.oertel@uib.de>)
- osparser: Ifelseendiflevel : longint = 0; // global nestlevel store
  used to pass nestlevel to evaluatestring and producestringlist  --> local functions (Detlef Oertel <d.oertel@uib.de>)
- osjson: jsonAsArrayToStringList: allow strings as valid array components (Detlef Oertel <d.oertel@uib.de>)
- osparser: indeffunc3 : integer = 0; // we are between deffunc and endfunc line (even in a not active code) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: for-to loop: fix failure at for x to x loop (Detlef Oertel <d.oertel@uib.de>)
- osparser: more fixes for nestinglevel (if then else through localfunctions and sub) (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: fixes on parsing paramtype (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: fix in isVisibleLocalVar (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.12] - 2017-12-13

### Added

- New constant %opsidata%, %opsiapplog% (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix sections in local functions: osparser: doAktionen: (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.11] - 2017-12-12

### Fixed

- Additional fix for Support brackets '[]' in Registry keys
  - osfunc: function posFromEnd(const substr : string; const s : string) : integer;
  - osparser: doRegistryHack: openkey, deletekey in Registry sections
  - osparser: evaluatestring: GetRegistryStringValue\* (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.10] - 2017-12-06

### Changed

- Encoding Parameter for sections which modify files (patches,Patchtext,...); references #3140
  - osencoding: LoadFromFileWithEncoding
  - osparser: doIniFilesPatchesMain: shortcut for utf8 (Detlef Oertel <d.oertel@uib.de>)
- other encoding Problems:
  - osencoding: reencode: encode by Default to utf8 which is the internal encoding
  - osencoding: LoadFromFileWithEncoding (more changes) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osdefinedfunctions: TOsDefinedFunction.call: free local Vars on leaving the function; fixes #3230 (Detlef Oertel <d.oertel@uib.de>)
- Fix call of defined functions inside if statement ; fixes #3232
  - osdefinedfunctions: call: new param Nestlevel
  - osparser: evaluatestring: new param Nestlevel  (to use while call defined function)
  - osparser: producestringlist: new param Nestlevel  (to use while call defined function)
  - osparser: doSetVar: new param Nestlevel  (to use while call evaluatestring/producestringlist) (Detlef Oertel <d.oertel@uib.de>)
- Support brackets '[]' in Registry keys ; fixes #2825
  - osparser: doRegistryHack: openkey, deletekey in Registry sections
  - osparser: evaluatestring: GetRegistryStringValue\* (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.9] - 2017-11-06

### Changed

- osmain: log command line parameter of opsi-script call (Detlef Oertel <d.oertel@uib.de>)
- osmain: 'Wait for ending' while running now log level 8 (Detlef Oertel <d.oertel@uib.de>)
- product sorting logging moved to log_prog (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osmain: startprogrammodes: fix for /productid (bug created with /logproductid) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.8] - 2017-11-06

### Changed

- start to work on new modifier '/encoding \<encoding>' for patches sections (Detlef Oertel <d.oertel@uib.de>)
- osparser:  tsImportLib: replace constants (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: fix Message 'ReportMessages' changed to 'ScriptErrorMessages' (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix Message 'ScriptErrorMessages' 'is set to' true/false was wrong (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.7] - 2017-10-10

### Added

- new config: opsi-script.global.ScriptErrorMessages : boolean ; default=true (created as false) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- default loglevel is now 7 (was 6) (osconf) (Detlef Oertel <d.oertel@uib.de>)
- config: opsi-script.global.default_loglevel will be created as 7 (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.6] - 2017-09-27

### Added

- new stringlistfunction editMap(list) : list; gets a key=value list and allows the user to modify the values (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osconf: readConfigFromService reads configstates:
  opsi-script.global.ScriptErrorMessages : boolean ; default=true (created as false)
  opsi-script.global.AutoActivityDisplay : boolean ; default=false  (created as true) (Detlef Oertel <d.oertel@uib.de>)
- general: adjusting log levels
  5:LLNotice: comments,messages, Execution of sections
  6:LLInfo; Statements, New values for stringvars , complete boolean expression
  7: new values for stringlist vars, Output from sections,
  shellcall if the output is not written to a stringlist, parts of a boolean expression
  8: other stringlist output
  9: content of saved temporary files (Detlef Oertel <d.oertel@uib.de>)
- osdefinedfunctions: TOsDefinedFunction.call: move inc(inDefinedFuncNestCounter)
  below parseCallParameter (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.5] - 2017-09-11

### Added

- osmain / osinteractivegui: new cli argument /logproductid
  just used for logging, do not try to get data from service (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osbatchGUI: TFBatchOberflaeche.FormActivate also for Linux (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.4] - 2017-09-04

### Changed

- osconf: readConfigFromService: protect from wrong data types from service (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.3] - 2017-09-01

### Changed

- global: rising the level of a lot of loging (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.2] - 2017-08-24

### Changed

- osbatchgui: ForceStayOnTop modified (Detlef Oertel <d.oertel@uib.de>)
- osbatchgui: ForceStayOnTop in form.activate (Detlef Oertel <d.oertel@uib.de>)
- osconf: default_loglevel = 6; (Detlef Oertel <d.oertel@uib.de>)
- osfunc: alldelete: del command: allow also partly wildcards (\*.yxz or xyz.\*) (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.1] - 2017-06-27

### Changed

- oswebservice: changed loglevels in retrieveJSONObject (Detlef Oertel <d.oertel@uib.de>)
- osfunc: FindLocalIPData / getMyHostEnt : better Error handling (Detlef Oertel <d.oertel@uib.de>)

## [4.12.0.0] - 2017-06-19

### Added

- new unit osdefinedfunctions (Detlef Oertel <d.oertel@uib.de>)
- osparser: new functions: boolToString(\<bool expression>) and stringToBool(\< string>) (Detlef Oertel <d.oertel@uib.de>)
- osparser: new statement: ImportLib (Detlef Oertel <d.oertel@uib.de>)
- oslog: new method log_prog for program debugging (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: support for defined functions (Detlef Oertel <d.oertel@uib.de>)
- osparser: moved sync between FExtremeErrorLevel and extremeErrorLevel before final output (Detlef Oertel <d.oertel@uib.de>)
- osconf: readConfigFromService reads configstates:
  opsi-script.global.debug_prog : boolean ; if true log log_prog if > LLWarning
  opsi-script.global.debug_lib : boolean ; if true log libraries if > LLWarning
  opsi-script.global.default_loglevel : intstring ; set the default loglevel
  opsi-script.global.force_min_loglevel : intstring ; set the minimal loglevel (Detlef Oertel <d.oertel@uib.de>)
- osmain: if /productid is set then use it also for the log (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- opsihwbiosInfo: typo Manufacturer ; fixes #2994 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.15] - 2017-08-25

### Changed

- osfunc: alldelete: del command: allow also partly wildcards (\*.yxz or xyz.\*)
  (backport from 4.12.0.2) (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.14] - 2017-06-29

### Added

- osparser: new file section copy option -h (follow symlinks; default=false) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: isNumber: now check against int64 (Detlef Oertel <d.oertel@uib.de>)
- osfunc: support for followsymlinks:
  FileInstall.AllCopy: AllCopy Recursive: ToCopyOrNotCopy
  filecopy  : fixes #2537
  Files section copy: -s : does not follow symlinks but copy the symlink files
  copy -h : follow symlinks (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.13] - 2017-05-29

### Changed

- oswebservice: typo: lastcation (Detlef Oertel <d.oertel@uib.de>)
- osbatchgui: use myStringToTColor to interpret rgb colors from ini file (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.12] - 2017-01-03

### Changed

- osjson: escape chars in jsonAsArrayToStringList (Detlef Oertel <d.oertel@uib.de>)
- osparser: Logging at errorsOccurredSinceMark (Detlef Oertel <d.oertel@uib.de>)
- some changes for 64 Bit Version (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: move/rename/symlink/hardlink: no Syntax error on wrong parameters ; fixes #2036 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.11] - 2016-12-06

### Added

- osparser: doOpsiServiceCall: new Parametzer opsiclientd-once (=tscOpsiclientdOnce)
  resets local_opsidata after servicecall (Detlef Oertel <d.oertel@uib.de>)

### Changed

- source migrated from svn to git and direcories restructured (Detlef Oertel <d.oertel@uib.de>)
- compiled with Lazarus 1.6.2 (Detlef Oertel <d.oertel@uib.de>)
- osparser: CreateAndProcessScript: resets local_opsidata before script (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: Statement: 'opsiservicecall /preloginservice' (without sektion) allowed (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: sendLog: shrinking temporary disabled until size can be asked via service call (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin: StrSIDToName: do not try empty string (Detlef Oertel <d.oertel@uib.de>)
- osparser: use 'on e: exception do' in exception handling (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin2: updateenvironment: use lazarus sendmessage (Detlef Oertel <d.oertel@uib.de>)
- osregistry: WriteEntry: unicode / utf8 Support (Detlef Oertel <d.oertel@uib.de>)
- osparser: Resourcestring for translation: rsReadyToContinue (Detlef Oertel <d.oertel@uib.de>)
- osparser: fileactionsmain: GetString with true (Detlef Oertel <d.oertel@uib.de>)
- osfunc: TPatchList.FindFirstItemWith: more logging (Detlef Oertel <d.oertel@uib.de>)
- osinteracticegui: message for not working logview (Detlef Oertel <d.oertel@uib.de>)
- osparser: symlink: check via cmd.exe after create symlink (Detlef Oertel <d.oertel@uib.de>)
- osparser: success or failed after 'script finished' in log footer (Detlef Oertel <d.oertel@uib.de>)
- osparser: dosbatch: retry on erroer 19 (WRITE_PROTECT) (Detlef Oertel <d.oertel@uib.de>)
- updateted lang (po) files (Detlef Oertel <d.oertel@uib.de>)
- updated indy lib 10-rev5391 (Detlef Oertel <d.oertel@uib.de>)

### Removed

- osparser: removed cmd.exe call from fileexists% (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: fix in getstring call chain (Detlef Oertel <d.oertel@uib.de>)
- osfunc: fix in chmod error handling (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.10] - 2016-11-09

### Added

- osparser: execShellCall: new parameter FatalOnFail : boolean = true (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osfuncwin: RunCommandAndCaptureOut: transfer exitcode if process could not start (Detlef Oertel <d.oertel@uib.de>)
- osparser: execShellCall: returns exitcode on fail (Detlef Oertel <d.oertel@uib.de>)
- osparser: evaluateBoolean: FileExists\* : call execshellCall with FatalOnFail=false (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.9] - 2016-11-04

### Changed

- osregistry: TuibRegistry.ReadEntry: use RegQueryValueExW on trdString, trdExpandString (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osregistry: fix encoding in deletekey (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.8] - 2016-10-28

### Changed

- osconf: global bool variable readconfig_done which is set by readconfig to true (Detlef Oertel <d.oertel@uib.de>)
- osparser: CreateAndProcessScript: do not call readconfig if  readconfig_done (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- should fix: uib#2016101810000104 — Installation per WAN schlägt auf zufälligen Clients fehl (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.7] - 2016-10-21

### Added

- new unit oskeyboard (Detlef Oertel <d.oertel@uib.de>)

### Changed

- linkfolder knows now parameter shortcut for key like Shift-Alt-O (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.6] - 2016-10-20

### Changed

- osmain: GetParameter: log error if readconfig failed (Detlef Oertel <d.oertel@uib.de>)
- osparser: CreateAndProcessScript: log error if readconfig failed (Detlef Oertel <d.oertel@uib.de>)
- osparser: CreateAndProcessScript: backup depot path and restor if rad config failed (Detlef Oertel <d.oertel@uib.de>)
- osparser: CreateAndProcessScript: backup depot path and restor if rad config failed (Detlef Oertel <d.oertel@uib.de>)
- osparser: evaluateBoolean: Fileexists\* calling execShellCall architecture dependent (Detlef Oertel <d.oertel@uib.de>)
- osparser: produceStinglist: getFileInfoMap32, getFileInfoMap64, getFileInfoMapSysnative (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.5] - 2016-10-10

### Added

- osencoding: new overloaded version of reencode with destEncoding (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osjson: fix at jsonAsObjectGetKeyList (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix at jsonAsObjectDeleteByKey (Detlef Oertel <d.oertel@uib.de>)
- osfunc: fix TXStringlist.savetofile (use new reencode verion) (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix reencodestr (use new reencode verion) (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.4] - 2016-10-05

### Added

- osjson: new functions: jsonAsObjectDeleteByKey, jsonAsArrayDeleteObjectByIndex (Detlef Oertel <d.oertel@uib.de>)
- osparser: new functions: jsonAsObjectDeleteByKey, jsonAsArrayDeleteObjectByIndex (Detlef Oertel <d.oertel@uib.de>)
- osparser: new boolean command savetextfilewithencoding(\<list>,\<filename>,\<endoding>) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osjson: jsonAsObjectGetKeyList: also true on empty keylist (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: sendlog: no more reencoding to uft8 ; Perhaps Fehler #2408 (Detlef Oertel <d.oertel@uib.de>)
- osregistry: readentry: trdString, trdExpandString: expicit use empty value if 0 bytes (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osencoding:  : trim of found encoding ; fixes #2411 ;  https://forum.opsi.org/viewtopic.php?p=38236#p38236 (Detlef Oertel <d.oertel@uib.de>)
- osparser: getMSVersionMap: Do not try to read ReleaseID if below win10; fixes #2316 (Detlef Oertel <d.oertel@uib.de>)
- osparser / osmain: use LLCritical in fatal situations; fixes #2263 (Detlef Oertel <d.oertel@uib.de>)
- osparser: CompareDotSeparatedNumbers (str/bool): better wrong parameter handling fixes #2045  #2369 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.3] - 2016-09-08

### Added

- osfunc: new functions: isValidUtf8String(str:string) : boolean;
  getFixedUtf8String(str:string) : string; (Detlef Oertel <d.oertel@uib.de>)
- osparser : new opsiscript functions:
  isValidUtf8String(str:string) : boolean;
  getFixedUtf8String(str:string) : string; (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.2] - 2016-09-05

### Changed

- more Logging for GetProductPropertyList (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: getProductPropertyList(myproperty: string;
  defaultlist: TStringList,
  myClientId,
  myProductId): TStringList;  overload; (Detlef Oertel <d.oertel@uib.de>)
- osparser: GetProductPropertyList overload 2 and 4 arguments (Detlef Oertel <d.oertel@uib.de>)

## [4.11.6.1] - 2015-09-22

### Added

- osfunclin: new: getMyIpByTarget(target): implemented as: ip route get \<target> (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: new: getMyIpByDefaultRoute: implemented as: ip route get 255.255.255.255 (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin2: getPid4user: add logging on process found with wrong user (Detlef Oertel <d.oertel@uib.de>)
- osmain: new command line parameter: 'credentialfile' for username, password and sessionid in format:
  username=\<username>
  password=\<password>
  sessionid=\<sessionid>
  If the parameter 'credentialfile' is used all thre parameter will be used from the file and
  the parameters : username, password and sessionid will be ignored (Detlef Oertel <d.oertel@uib.de>)
- osmain: (GUI) GetParameter: new parameter: lang \<2 letter lang code>
  is set to lcltranslator.SetDefaultLang (Detlef Oertel <d.oertel@uib.de>)
- new unit osjson (Detlef Oertel <d.oertel@uib.de>)
- new json functions:
  - jsonIsValid(\<jsonstr>) : boolean
  - jsonIsArray(\<jsonstr>) : boolean
  - jsonIsObject(\<jsonstr>) : boolean
  - jsonAsObjectHasKey(\<jsonstr>,\<keystr>) : boolean
  - jsonAsArrayCountElements(\<jsonstr>) : intstr
  - jsonAsObjectCountElements(\<jsonstr>) : intstr
  - jsonAsArrayGetElementByIndex(\<jsonstr>, \<indexstr>) : jsonstring
  - jsonAsObjectGetValueByKey(\<jsonstr>, \<keystr>) : valuestring
  - jsonAsObjectSetValueByKey(\<jsonstr>, \<keystr>,\<valuestring>) : jsonstring
  - jsonAsObjectSetStringtypeValueByKey(\<jsonstr>, \<keystr>,\<valuestring>) : jsonstring
  - jsonAsArrayPutObjectByIndex(\<jsonstr>, \<indexstr>, \<objectstr>) : jsonstring
  - jsonAsArrayToStringList(\<jsonstr>) : stringlist
  - jsonAsObjectGetKeyList(\<jsonstr>) : stringlist
  - jsonStringListToJsonArray(\<strlist>) : jsonstr (Detlef Oertel <d.oertel@uib.de>)
- new function: setStringInListAtIndex(\<newstring>,\<list>,\<indexstr>) : list (Detlef Oertel <d.oertel@uib.de>)
- new string function md5sumFromFile(\<path to file>) (Detlef Oertel <d.oertel@uib.de>)
- new string function timeStampAsFloatStr (floattostrF(now,ffFixed,15,3)) (Detlef Oertel <d.oertel@uib.de>)
- new bool function processIsRunning(\<process string>) (Detlef Oertel <d.oertel@uib.de>)
- new variant of shellcall as procedure (no result) (Detlef Oertel <d.oertel@uib.de>)
- new variant of shellcall with result string which have the exitcode (Detlef Oertel <d.oertel@uib.de>)
- new string function encryptStringBlow(\<keystring>,\<datastring>) (Detlef Oertel <d.oertel@uib.de>)
- new string function decryptStringBlow(\<keystring>,\<datastring>) (Detlef Oertel <d.oertel@uib.de>)
- new String function startProcess(\<string>) returns exitcode from sysnative started process (Detlef Oertel <d.oertel@uib.de>)
- new String function processCall(\<string>) returns exitcode from sysnative started process (Detlef Oertel <d.oertel@uib.de>)
- new bool function waitForPackageLock(\<seconds timeout string>,\<bool should we kill>) (Linux only) (Detlef Oertel <d.oertel@uib.de>)
- new unit osfuncwin3 with fuctions to windows version (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function removeFromListByMatch(\<searchstring>,\<list>) (Detlef Oertel <d.oertel@uib.de>)
- new string function getStringFromListAtIndex(\<list>,\<indexstring>) (Detlef Oertel <d.oertel@uib.de>)
- new unit: osregistry: All Tuibregistry stuff moved here from osfunc. (Detlef Oertel <d.oertel@uib.de>)

### Changed

- Merge from (4.11.5.14):
  - oswebservice: assume opsiservice version=4 in retrieveJSON\*
  to avoid access to opsidata which fails if we use local_opsidata
  fixes Access Violations in initial opsi-client-agent installation
  - osfunc: FuncSaveToFile / SaveToFile: logging content on Loglevel 9 ; references #1679
  - osfuncwin: fix broken Shellbatch /showoutput (Detlef Oertel <d.oertel@uib.de>)
- Merge from opsi-winst/opsi-script (4.11.5.13) stable; urgency=low
  - do not try to run non existing external sub sections (Detlef Oertel <d.oertel@uib.de>)
- Merge from  (4.11.5.12) stable; urgency=low
  - Bugfix: osparser: doAktionen: fullfilename not initialized:
  causes causes double load of include or sub if searched file is not existing ; fixes #1656
  - Bugfix: osparser: Filexists\*: do not check for existing file with dir/ls if trim(filename)=''
  fixes: https://forum.opsi.org/viewtopic.php?f=6&t=7697
  (FileExists auf leere Variable gibt wahr zurück) (Detlef Oertel <d.oertel@uib.de>)
- osfunc: rewrite of FindLocalIPData for linux (Detlef Oertel <d.oertel@uib.de>)
- osparser:  getMyIpByTarget now also for linux (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: getCommandResult: Exitcode on higher (+2) loglevel (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin2: Impersonate2User: 5 retrys of getPid4user and explorer.exe (Detlef Oertel <d.oertel@uib.de>)
- osconf,osmain: change depotdrive for linux from /mnt to /media/opsi_depot (Detlef Oertel <d.oertel@uib.de>)
- osmain: BuildPC: create flag files in /var/run/opsiclientd for shutdown or reboot
  execute reboot only if no opsiclientd in processlist (Detlef Oertel <d.oertel@uib.de>)
- osparser: savetextfile: call FuncSaveToFile  (merge from 4.11.5.17) (Detlef Oertel <d.oertel@uib.de>)
- opsiscript.lpr / winst.lpr replace defaulttranslator by lcltranslator (Detlef Oertel <d.oertel@uib.de>)
- osmain: BuildPC: termonate after exception in BuildPC: final ; references #1820 (Detlef Oertel <d.oertel@uib.de>)
- changes for lazarus 1.6 / FPC 3.0 (Detlef Oertel <d.oertel@uib.de>)
- Port from opsi-script 4.11.5.20:
  - More logging to find bug: loginscrpits do not always can Impersonate (uib#2015121410000075):
  - osfuncwin2: Impersonate2User: change parameter / add usercontextsid
  - osfuncwin2: Impersonate2User: more logging if usercontext not found
  - osparser: doLinkFolderActions: if runLoginScripts: call Impersonate2User with usercontextsid (Detlef Oertel <d.oertel@uib.de>)
- osparser: at getMSVersionMap list.add ('ReleaseID=' + GetRegistrystringvalue("HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion" "ReleaseID",true)); (Detlef Oertel <d.oertel@uib.de>)
- DosBatch /showoutput:
  - osfuncwin: switch main window to left/top corner and output window to right/down corner
  - osfunclin: the same and fixes #1936 (Detlef Oertel <d.oertel@uib.de>)
- osparser: at getLinuxVersionMap list.add ('SubRelease=' (patchlevel from SLES) (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: function getPackageLock(timeoutsec : integer; kill : boolean) : Boolean; (Detlef Oertel <d.oertel@uib.de>)
- Linux: check for free start count (Detlef Oertel <d.oertel@uib.de>)
- osparser:doAktionen:getWord use WorddelimiterSet4 before FindKindOfStatement (needed for 'shellCall(' statement) (Detlef Oertel <d.oertel@uib.de>)
- osconf: Linux: reading winstversion via fileinfo.TFileVersionInfo (Detlef Oertel <d.oertel@uib.de>)
- use LazFileUtils in different units (may replace fileutil ?) (Detlef Oertel <d.oertel@uib.de>)
- osfunclin/osfunc: use FPCProcess.ExitCode instead of ExitStatus to get the final exitcode
  //\* oswebservice: retrieveJSONObject: // test memorystream to raise exception here (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: productonClients_getObjects__actionrequests: use getProductOrdering (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: productOnClient_getobject_actualclient: use getProductOrdering (Detlef Oertel <d.oertel@uib.de>)
- osparser: at getMSVersionMap new key prodInfoNumber from win32 GetProductInfo (Detlef Oertel <d.oertel@uib.de>)
- osparser: at getMSVersionMap new key prodInfoText from getProductInfoStrByNum from win32 GetProductInfo (Detlef Oertel <d.oertel@uib.de>)
- osmain: initialization:  GetDefaultLang; (Detlef Oertel <d.oertel@uib.de>)
- do not delete temporary cmd files on Loglevel LLconfidential (Detlef Oertel <d.oertel@uib.de>)
- Logging of productordering to LLDebug2 (8) (Detlef Oertel <d.oertel@uib.de>)
- Windows: writing ReleaseId and ProductId to log file header (Detlef Oertel <d.oertel@uib.de>)
- osparser: md5sumFromFile: use ExpandFileName (Detlef Oertel <d.oertel@uib.de>)
- osparser: getValueFromFile / getValueFromFileBySeparator : empty string on failure (Detlef Oertel <d.oertel@uib.de>)
- osencoding: internal Defaultencoding=utf8
  //\* oslog: DependentAdd: reencode log strings from utf-8 to system (Detlef Oertel <d.oertel@uib.de>)
- oslog: Logfile now utf-8 encoded (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: productonClients_getObjects__actionrequests: fallback to old sorting method
  if exception in new method (eg. getProductOrdering not reachable) (Detlef Oertel <d.oertel@uib.de>)
- wispecfolder, osfunc: replace SHGetPathFromIDList by SHGetPathFromIDListW (Detlef Oertel <d.oertel@uib.de>)
- osfunc: MakeShellLink: for LinkFile.Save filename in system encoding (Detlef Oertel <d.oertel@uib.de>)
- osparser: takestring: to large index Message is changed from Errorr to Warning (Detlef Oertel <d.oertel@uib.de>)
- osfunc: more logging in LoadNTUserDat (Detlef Oertel <d.oertel@uib.de>)
- makebakfile: change from old style (name.ext.num) to new style (name_num.ext) (Detlef Oertel <d.oertel@uib.de>)
- osfunc: encoding of cmd in StartProcess_cp (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin: encoding of cmd and output in RunCommandAndCaptureOut (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: getCommandResult overload wirh exitcode (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osfuncwin2: getPid4user: fix logging on not found (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: finishOpsiConf: catch exception at backend_exit ; fixes #1820 (Detlef Oertel <d.oertel@uib.de>)
- fix: strip newline from strLoadTextfile  and strLoadTextFileWithEncoding (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin3: fix GetWinSystemDirectory (Detlef Oertel <d.oertel@uib.de>)
- osparser: getJsonUrlString: fix for json array as parameter (Detlef Oertel <d.oertel@uib.de>)
- utf8 - system encoding fixes in GetRegistryString\*, Patches, .... (Detlef Oertel <d.oertel@uib.de>)
- osfunclin: fix getHostnameLin (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.14] - 2015-09-10

### Changed

- oswebservice: assume opsiservice version=4 in retrieveJSON\*
  to avoid access to opsidata which fails if we use local_opsidata
  fixes Access Violations in initial opsi-client-agent installation (Detlef Oertel <d.oertel@uib.de>)
- osfunc: FuncSaveToFile / SaveToFile: logging content on Loglevel 9 ; references #1679 (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osfuncwin: fix broken Shellbatch /showoutput (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.13] - 2015-08-21

### Changed

- do not try to run non existing external sub sections (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.12] - 2015-08-20

### Fixed

- Bugfix: osparser: doAktionen: fullfilename not initialized:
  causes causes double load of include or sub if searched file is not existing ; fixes #1656 (Detlef Oertel <d.oertel@uib.de>)
- Bugfix: osparser: Filexists\*: do not check for existing file with dir/ls if trim(filename)=''
  fixes: https://forum.opsi.org/viewtopic.php?f=6&t=7697
  (FileExists auf leere Variable gibt wahr zurück) (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.11] - 2015-08-13

### Fixed

- Bugfix: (FileExist call) changes (GetLastExitCode Result Value):
  (because it call execshellCall to detect symlinks correctly):
  osparser: new parameter for execshellCall: FetchExitCodePublic
  if false result is on FLastPrivateExitCode
  if false true is on FLastExitCodeOfExe (GetLastExitCode) (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.10] - 2015-07-21

### Changed

- oswebservice: retreiveJSONObject
  - start with (new) correct Mimetype and AcceptEncoding
  and fall back to old (wrong) parameters (Detlef Oertel <d.oertel@uib.de>)
- osfunc: FileCopyWin: more log on file in use (Detlef Oertel <d.oertel@uib.de>)
- osfunc: TuibRegistry.Create(myforce64: boolean; readonly: boolean); (Detlef Oertel <d.oertel@uib.de>)
- osmain: BuildPC: speed up: only ask for changed action request if there was one (uib#2015080710000015) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osfunc: FileCopyWin: fix for Err 32 on setTimeForFile (Detlef Oertel <d.oertel@uib.de>)
- osparser: GetRegistrystringvalue\* works now with readonly registry ; fixes #1647 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.9] - 2015-07-17

### Added

- oswebservice: new procedure TOpsi4Data.setActualProductActionRequest (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osmain: initproduct before run update script: refresh getProductMap (Detlef Oertel <d.oertel@uib.de>)
- osmain: but still use setup as lastaction even if was setup+update (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.8] - 2015-07-09

### Added

- new statement LoadProductProperties \<clientId> \<productId> (re-loads productproperty list from Server if possible) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- GetProductpropertyList: do not try to use also local_opsidata or opsidata (Detlef Oertel <d.oertel@uib.de>)
- GetProductproperty: do not try to use also local_opsidata or opsidata (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.7] - 2015-07-07

### Changed

- GetProductproperty: try / except added (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.6] - 2015-07-02

### Changed

- GetProductpropertyList: try to use also local_opsidata, log using default (Detlef Oertel <d.oertel@uib.de>)
- GetProductproperty: if empty try to use also local_opsidata or opsidata, log using default (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- GetProductpropertyList: also with list as second (default) parameter ; fixes #1602 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.5] - 2015-06-25

### Changed

- osmain: BuildPC: process product only if we have a original action request wich is still set (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- upgrade to ssl Libraries 1.0.2c (libeay32.dll, ssleay32.dll); fixes #1586 (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin2: updateEnvironment: do not try to run updateEnvironment on windows \< 6; fixes #1591 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.4] - 2015-06-02

### Changed

- Working with ssl protocol: sslvSSLv23 - auto negotation (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.3] - 2015-06-01

### Changed

- (Security Update) oswebservice: use TLS1.2 instead of SSLv3 and fallback downto SSLv3 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.2] - 2015-04-01

### Added

- osfunc: new function opsiunquotestr2 (Detlef Oertel <d.oertel@uib.de>)
- new string function unquote2(s1,s2): like unquote but:
  removes only quotes if they found at start and end
  s2 may be two chars long. Then the first char is the start mark
  and the second char is the end mark (Detlef Oertel <d.oertel@uib.de>)
- osparser: GetString: new boolean switch requote (Detlef Oertel <d.oertel@uib.de>)
- new boolean function CompareDotSeparatedNumbers(\<str1>,\<relation str>,\<str2>)
  \<relation str> may be: one of [\<,\<=,=,>=,>] (Detlef Oertel <d.oertel@uib.de>)
- new boolean function CompareDotSeparatedStrings(\<str1>,\<relation str>,\<str2>)
  \<relation str> may be: one of [\<,\<=,=,>=,>] (Detlef Oertel <d.oertel@uib.de>)

### Changed

- upgrade to ssl Libraries 1.0.2a (libeay32.dll, ssleay32.dll) (Detlef Oertel <d.oertel@uib.de>)
- if move / rename fails (file is in use) we retry it with MOVEFILE_DELAY_UNTIL_REBOOT
  So the move will be done at the next reboot. Reboot will be set automatically if not Option -c is set. (Detlef Oertel <d.oertel@uib.de>)
- osparser: GetString requote=true: change double quoted (""str"") to alternative double quoted ('""');  references #1284 (Detlef Oertel <d.oertel@uib.de>)
- osfunc: startprocess_cp: translate windows showWindowFlag to TProcess TShowWindowOptions (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: more log for "unexpected Result from webservice in retrieveJSONObject, number of lines: 0" (Detlef Oertel <d.oertel@uib.de>)
- hide non visible processes (SW_HIDE) like winbatch or shellInAnIcon (Detlef Oertel <d.oertel@uib.de>)
- (Security Update) oswebservice: use TLS1.2 instead of SSLv3 (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- files commands move / rename / hardlink / symlink now work with overwrite; fixes #1435 (Detlef Oertel <d.oertel@uib.de>)
- file command del / delete now with option -c and retry to delete on next boot if file in use ; fixes # 788 (Detlef Oertel <d.oertel@uib.de>)
- new string function: GetShortWinPathName(longpath) ; fixes #581
  Windows only
  Gives short path (8.3) from a long path e.g.
  GetShortWinPathName("C:\Program Files (x86)") gives "C:\PROGRA~2"
  In case of Error the result is an empty string (Detlef Oertel <d.oertel@uib.de>)
- osparser: doRegistryHack\* : fix for not HKCU reg enties in loginscripts; fixes #1538 (Detlef Oertel <d.oertel@uib.de>)
- new string function: GetConfidentialProductProperty : like GetProductProperty but handles the result as confidential string ; fixes #462 (Detlef Oertel <d.oertel@uib.de>)
- fixes at GetNTVersion for Win10 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.5.1] - 2014-12-22

### Added

- osparser: New files commands hardlink, symlink, rename, move (Detlef Oertel <d.oertel@uib.de>)
- New loop statement: for %s% = $start$ to $stop$ do \<statement> ;  references #407
  - $start$ and $stop$ has to be integer strings
  - $start$ \< $stop$ is required
  - in fact a stringlist $start$ to $stop$ is generated over which we iterate while loop (Detlef Oertel <d.oertel@uib.de>)
- osparser: produceStringList: New (overloaded) stringlist function (references #1110):
  - removeFromListByContaining(\<search string>, \<target list>) removes  \<search string> from \<target list>
  - removeFromListByContaining(\<search list>, \<target list>) removes  \<search list> from \<target list>
  - both functions search case insensitive for a matching substring (Detlef Oertel <d.oertel@uib.de>)
- new Statement: UpdateEnvironment : Triggers Windows to reload Environment from Registry; Win only; references #1364 (Detlef Oertel <d.oertel@uib.de>)
- new Control Statement: Switch  ; references  #740
  - osparser: Switch \<string expr ; normally variable>
  Case \<string expr; normally const>
  statments
  EndCase
  [Defaultcase
  statments
  EndCase]
  EndSwitch (Detlef Oertel <d.oertel@uib.de>)
- New Statement: MaximizeWinst at osparser (Detlef Oertel <d.oertel@uib.de>)
- New Functions LoadTextFileWithEncoding(file,encoding) strLoadTextFileWithEncoding(file,encoding) at osparser (Detlef Oertel <d.oertel@uib.de>)

### Changed

- port of the changes since 4.11.4.6 to Linux (Detlef Oertel <d.oertel@uib.de>)
- osparser: execDosBatch: The path to temporary file is only quoted if it contains spaces (Detlef Oertel <d.oertel@uib.de>)
- osparser: executeWith: Variables are now allowed as Parameters (but no functions) (Detlef Oertel <d.oertel@uib.de>)
- osparser: tsLoopForTo  for %s% = $start$ to $stop$ do \<statement> (Detlef Oertel <d.oertel@uib.de>)
- replacing opsi-winst constants by opsi-script (Detlef Oertel <d.oertel@uib.de>)
- upgrade to ssl Libraries 1.0.1L (libeay32.dll, ssleay32.dll) (Detlef Oertel <d.oertel@uib.de>)
- osmain: GetParameter + StartProgramModes: more try - except : port from 4.11.4.16 (Detlef Oertel <d.oertel@uib.de>)
- osparser: doOpsiServiceCall: more try - except : port from 4.11.4.16 (Detlef Oertel <d.oertel@uib.de>)
- osparser: doTextPatchMain: do not use CheckFileExists in SaveToFile ; references #171 (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: finishOpsiConf: if runproductlist then Logg-Appendmode := true; references #1379 (Detlef Oertel <d.oertel@uib.de>)
- LinkFolder for Linux activated: No BaseFolder, No SubFolder, No Iconindex
  but link_categories: 'AudioVideo',
  'Audio', 'Video', 'Development', 'Education', 'Game', 'Graphics',  'Network',
  'Office', 'Settings', 'System', 'Utility'
  // from http://standards.freedesktop.org/menu-spec/menu-spec-1.0.html#category-registry
  changes in osparser, osfunclin and new unit oslindesktop (Detlef Oertel <d.oertel@uib.de>)
- some more ExpandFilename calls to handle incorrect path delimiter (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: sendlog: replace old writeLog by new log_write method (Detlef Oertel <d.oertel@uib.de>)
- osencoding: LoadFromFileWithEncoding (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: sendlog: at Linux do not convert logstrings to uft8 (Detlef Oertel <d.oertel@uib.de>)
- oslog: dependentadd: more try except (Detlef Oertel <d.oertel@uib.de>)
- osparser: sleepseconds (tsSleep): more try except and more logging (Detlef Oertel <d.oertel@uib.de>)
- osparser: winbatch: trim targetfilename (Detlef Oertel <d.oertel@uib.de>)
- osparser: shellBatch: Parameter at Linux (wrong bash parameter) (Detlef Oertel <d.oertel@uib.de>)
- osmain: User Profile Management (runloginscripts) is now free (Detlef Oertel <d.oertel@uib.de>)
- osmain: do not switch opsi-script to setup (even if it is opsi-winst); references # 1437 (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: commandline := 'cmd64.exe /c "reg delete ""'+key_completepath+'"" /f'; (fixes #1299) (Detlef Oertel <d.oertel@uib.de>)
- osfunc: new tuibinstall methods: hardlink, symlink, rename (Linux / Windows ; fixes #1319, #362) (Detlef Oertel <d.oertel@uib.de>)
- osparser: execDosBatch: Variables are now allowed as Parameters (but no functions) fixes #1312 (Detlef Oertel <d.oertel@uib.de>)
- osparser: doAktionen: Variables and functions are now allowed as sleepsecond Parameters; fixes #1193 (Detlef Oertel <d.oertel@uib.de>)
- osparser: %SriptPath%/../lib as additional Path in Search path for includes; fixes #1201 (Detlef Oertel <d.oertel@uib.de>)
- osparser: searchpath for includes are now works also for sub \<file> ; fixes #1201 (Detlef Oertel <d.oertel@uib.de>)
- osparser: activating winbatch for linux ; fixes #1170 (Detlef Oertel <d.oertel@uib.de>)
- ospparser: fix for filexists\* to detect symlinks (Detlef Oertel <d.oertel@uib.de>)
- HowToBuild.txt ; fixes #1372 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.14] - 2014-12-04

### Changed

- less (only 512 chars) debug output: oswebservice: retrieveJSONObject: log ResultLines.Strings[0] if Result = nil (Detlef Oertel <d.oertel@uib.de>)
- oswebservice: retrieveJSONObject: FillChar(buffer^, 655360,' '); before read to buffer (Detlef Oertel <d.oertel@uib.de>)
- manifest extended for win 10: \<supportedOS Id="{8e0f7a12-bfb3-4fe8-b9a5-48fd50a15a9a}"/> (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.13] - 2014-11-12

### Changed

- more debug output: oswebservice: retrieveJSONObject: log ResultLines.Strings[0] if Result = nil (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.12] - 2014-10-24

### Changed

- Logdevel while writing log to Service decreased (Detlef Oertel <d.oertel@uib.de>)
- bakup files of log now are opsi-script.log.x where x is 0 - 8 (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix: osmain: StartProgramModes: correct Path to the opsiclientd.conf in batch mode (Detlef Oertel <d.oertel@uib.de>)
- fix: command typo: ErrorsOccuredSinceMark is wrong, now also allowed: ErrorsOccurredSinceMark  fixes #1121 (Detlef Oertel <d.oertel@uib.de>)
- do not run login scripts if user is 'opsisetupadmin' fixes #1248 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.11] - 2014-10-09

### Added

- new unit: SystemCriticalU (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osmain: in StartprogramModes set SystemCritical.IsCritical := true; (Detlef Oertel <d.oertel@uib.de>)
- osmain: in BuildPC set SystemCritical.IsCritical := true; (Detlef Oertel <d.oertel@uib.de>)
- osmain: in Terminateapp set SystemCritical.IsCritical := false; (Detlef Oertel <d.oertel@uib.de>)
- osmain: in different execept set SystemCritical.IsCritical := false; (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.10] - 2014-10-01

### Added

- osparser: new bool function: LineContaining_ExistsIn(\<string>, \<file name>) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: doAktionen: tsIncludeInsert: debug outputs for 'index out of range' bug (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: LoadValidLinesFromFile : fix add loaded sub lines to FLinesOriginList
  to fix 'index out of range' at tsIncludeInsert (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.9] - 2014-08-20

### Added

- osbatchgui: new var SavedBatchWindowMode (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osparser: on NormalizeWinst and IconizeWinst save BatchWindowMode to SavedBatchWindowMode
  and set BatchWindowMode to new value.
  on RestoreWinst set to BatchWindowMode and mode to SavedBatchWindowMode
  Fixes changing the window mode by every sub section call (do_actions) (Detlef Oertel <d.oertel@uib.de>)

### Removed

- osfunc: StartProcess_cp: remove wrong setProgress which may lead to division by zero (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.8] - 2014-08-07

### Changed

- osparser: more loggin on LLDebug2 in fileActionsMain: used source and target (Detlef Oertel <d.oertel@uib.de>)
- osparser: more loggin on LLDebug2 in fileActionsMain: additional trim() an target (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.7] - 2014-07-23

### Added

- new unit cCPUusage (Detlef Oertel <d.oertel@uib.de>)
- new switch: AutoActivityDisplay = (true/false) default=false; if true shows marquee while winbatch/dosbatch (Detlef Oertel <d.oertel@uib.de>)
- new manifest: with assemblyIdentity (needed for maquee style in Activitybar (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osfunc: StartProcess_cp: if timeout is given: use Progressbar to show time progess until Timeout (Detlef Oertel <d.oertel@uib.de>)
- skin.ini; new section [Activitybar] (Detlef Oertel <d.oertel@uib.de>)
- osfuncwin: RunCommandAndCaptureOut: non blocking readfile with PeekNamedPipe (so Activitybar may run in Background) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser : Line 'Looping through' append trailing '''' fixes # 1116 (Detlef Oertel <d.oertel@uib.de>)
- osparser: typo double instead of doubble  fixes #1117 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.6] - 2014-05-27

### Added

- osparser/oslog: new log file .history which always appends just the timestamp, name and version of the installed product. (Detlef Oertel <d.oertel@uib.de>)
- osparser: PatchTextFile: new command: searchAndReplace \<search str> \<replace str> (works global and not case sensitive) (Detlef Oertel <d.oertel@uib.de>)
- osparser: new stringfunction: strLoadTextFile (\<filename>) (Detlef Oertel <d.oertel@uib.de>)
- osparser: new bool function: sendlog(\<clientId>,\<filename>,\<append>) (Detlef Oertel <d.oertel@uib.de>)
- new parameter for dosinanicon: /showoutput : shows a windows with the continuous output of the command while running (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osconf: changed version to 4.11.4.6 (Detlef Oertel <d.oertel@uib.de>)
- osparser: getsublist: trim parameters before strtoint (Detlef Oertel <d.oertel@uib.de>)
- set StandardPartLogPath to Path of the given MainLogFile if given by parameter or interactive (Detlef Oertel <d.oertel@uib.de>)
- experimental: starting of win64 PE version (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osmain: typo (dependenproducts) fixes   #1078 (Detlef Oertel <d.oertel@uib.de>)
- osparser: typo (Windowds) fixes   #1078 (Detlef Oertel <d.oertel@uib.de>)
- osparser: fix: RegDeleteKey (https://forum.opsi.org/viewtopic.php?p=27061#p27061) #1086 (Detlef Oertel <d.oertel@uib.de>)
- osparser: set Fatal if shellbatch/execwith could not be executed. fixes #1074 (Detlef Oertel <d.oertel@uib.de>)
- The temporary file names have now a random component in order to prevent conflicts between different instances of opsi-script. fixes #774 (Detlef Oertel <d.oertel@uib.de>)
- subsub bug fix: help to find sub sections in external sub section files (calling section as additional parameter in doAktionen) (Detlef Oertel <d.oertel@uib.de>)
- osfunc: fix in existsession linux: retry reboot with some hardcoded path variants (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.5] - 2014-05-19

### Changed

- osparser: doRegistryAllNtUserDats: additional check if we can patch via HKU (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- unquote fix:
  - osparser: 'unquote' function details moved to osfunc: 'opsiunquotestr' function
  (because synautil.unquotestr is buggy: "tes't" gives "test"
  - osfunc: new function  opsiunquotestr
  - osfunc: use of opsiunquotestr in fileexists, chmod
  - osparser: use of opsiunquotestr in fileactionsmain
  - osmain: use of opsiunquotestr in getparameter (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.4] - 2014-03-20

### Added

- new commands in PatchTextFile (osparser:doTextpatchMain):
  'setKeyValueSeparator char' sets Separator for key value pairs (default is '=')
  'setValueByKey keystr valuestr' search exitsing key and sets the given value.
  If the key not exists, the key/value pair will be created below the line cursor. (Detlef Oertel <d.oertel@uib.de>)
- new function: getValueFromFile (\<key string>,\<file name> ) : string (Detlef Oertel <d.oertel@uib.de>)
- new function: getValueFromFileBySeparator (\<key string>,\<separator string>,\<file name> ) : string (Detlef Oertel <d.oertel@uib.de>)
- new function: saveTextFile(\<list>, \< filename>) : bool (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osfunclin: RunCommandAndCaptureOut: Log exitcode on Loglevel LLInfo (Detlef Oertel <d.oertel@uib.de>)
- osparser: scriptstopped := true on ExitWindows /ImmediateReboot and  /ImmediateLogout (Detlef Oertel <d.oertel@uib.de>)
- osfunc: enhanced: TXStringList.getStringValue (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- osparser: linkActionsMain: fix for index out of bound (while (i \<= Sektion.count))  fixes: #958 (Detlef Oertel <d.oertel@uib.de>)
- osfunc: StartProcess_cp: log error if time out is reached (inc's error counter)  fixes #506 (Detlef Oertel <d.oertel@uib.de>)
- osparser: TuibInstScript.doAktionen: output initalized (created) fixes #974 (Detlef Oertel <d.oertel@uib.de>)
- osparser: produceStringlist: fix in emptylist() function (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.3] - 2013-12-09

### Added

- osparser: new constant %opsiTmpDir% (c:\opsi.org\tmp ; /tmp ) (Detlef Oertel <d.oertel@uib.de>)
- osparser: new constant %opsiLogDir% (c:\opsi.org\log ; /var/log/opsi-client-agent/opsi-script ) (Detlef Oertel <d.oertel@uib.de>)
- osparser: new command: FatalOnRuntimeError (default = false) (Detlef Oertel <d.oertel@uib.de>)
- osparser: new function doRegistryNTUserDat (Detlef Oertel <d.oertel@uib.de>)
- new unit opsihwbiosinfo based on tsmbios (Detlef Oertel <d.oertel@uib.de>)
- new infomap getHWBiosInfoMap (Detlef Oertel <d.oertel@uib.de>)
- new bool function isDriveReady(str) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- osfunc: wait for parent ending also if waitforprocessending is true (Detlef Oertel <d.oertel@uib.de>)
- osfunc: FileCopyWin: use of windows.copyFile instead of fileutils.copyFile (Detlef Oertel <d.oertel@uib.de>)
- osparser: takeString: LogError if index out of bounds; Fatal if FatalOnSyntaxError (Detlef Oertel <d.oertel@uib.de>)
- oslog: escape control chars (Detlef Oertel <d.oertel@uib.de>)
- osfunc: allcopy/execdelete: handle directories with additional attributes (e.g. hidden) (Detlef Oertel <d.oertel@uib.de>)
- osmain: ProcessProduct: looks before processing a product if there is still a action request (Detlef Oertel <d.oertel@uib.de>)
- osparser: for fileExists: replace GetFileInfo by  "FileExists() or DirectoryExists()" (Detlef Oertel <d.oertel@uib.de>)
- doFileActions: disable windows critical Error Message boxes (SetErrorMode(SEM_FAILCRITICALERRORS)) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- opsparser: doFileActions: fix for mixed File section modifiers (Detlef Oertel <d.oertel@uib.de>)
- osfunc: TuibRegistry.WriteEntry: fix logging if regtype is changed: fixes #780 (Detlef Oertel <d.oertel@uib.de>)
- osparser: Stingreplace with IgnoreCase for %userprofiledir%  fixes #877 (Detlef Oertel <d.oertel@uib.de>)
- new Registry Parameter /NTuserDat: \<path to ntuser.dat>  fixes #302 (Detlef Oertel <d.oertel@uib.de>)
- fixes to really stop the script interpretation after isfatalerror, issuccess, issuspended (Detlef Oertel <d.oertel@uib.de>)
- new bool function runningOnUefi      fixes #908 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.2] - 2013-09-23

### Added

- osencoding: searchencoding: new methode to read encoding (Detlef Oertel <d.oertel@uib.de>)
- new string function reencodestr(str, from, to) (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function reencodestrlist(strlist, from, to) (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function shellCall( cmd ) ; returns the output of (sysnative) shell with cmd (Detlef Oertel <d.oertel@uib.de>)

### Changed

- make 4.11.4 working under windows (Detlef Oertel <d.oertel@uib.de>)
- osinteractivegui: opendialog knows now \*.ins and \*.opsiscript (Detlef Oertel <d.oertel@uib.de>)
- getipByName now uses resolveip at linux (Detlef Oertel <d.oertel@uib.de>)
- manifest extended for win 8.1: \<supportedOS Id="{1f676c76-80e1-4239-95bb-83d0f6d0da78}"/>:
  http://msdn.microsoft.com/en-us/library/windows/desktop/dn302074%28v=vs.85%29.aspx (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix: use of sysnative with winbatch (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4.1] - 2013-07-30

### Added

- new unit osencode which contain encoding functions
  known encodings are
  UTF-8      utf8
  UTF-8BOM   utf8bom
  Ansi       ansi
  CP1250     cp1250
  CP1251     cp1251
  CP1252     cp1252
  CP1253     cp1253
  CP1254     cp1254
  CP1255     cp1255
  CP1256     cp1256
  CP1257     cp1257
  CP1258     cp1258
  CP437      cp437
  CP850      cp850
  CP852      cp852
  CP866      cp866
  CP874      cp874
  CP932      cp932
  CP936      cp936
  CP949      cp949
  CP950      cp950
  ISO-8859-1 iso8859-1
  ISO-8859-2 iso8859-2
  KOI-8      koi8
  UCS-2LE    ucs2le
  UCS-2BE    ucs2be (Detlef Oertel <d.oertel@uib.de>)
- new command 'IsSuspended' interupts script execution and leave productOnClient unchanged   Feature #614 (Detlef Oertel <d.oertel@uib.de>)

### Changed

- splitted project to opsi-script and opsi-script-nogui (Detlef Oertel <d.oertel@uib.de>)
- splitted unit osmain to osmain and osinteractivegui (Detlef Oertel <d.oertel@uib.de>)
- changed unit names:  \* wi\* -> os\* (Detlef Oertel <d.oertel@uib.de>)
- command IncludeLog has now an optional 3rd parameter sourceEncoding which may be 'auto', 'system' or a valid encoding. (Detlef Oertel <d.oertel@uib.de>)
- nearly all loadfromfile now with reencode (Detlef Oertel <d.oertel@uib.de>)
- script encoding can now be given with for example
  encoding=utf8 (Detlef Oertel <d.oertel@uib.de>)
- shellbatch output is now captured (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fixed: Feature  491 : real origin file and line number on syntax error (also on included parts) (Detlef Oertel <d.oertel@uib.de>)
- fixed: Bug 554 : includelogtail calls reencode (Detlef Oertel <d.oertel@uib.de>)
- loglevel of if/else/endif changed to LLinfo (6) fixes 539 (Detlef Oertel <d.oertel@uib.de>)
- loglevel of setLoglevel  changed to LLinfo (6) fixes 535 (Detlef Oertel <d.oertel@uib.de>)
- fix in linux getprocesslist: output now: short binary name;pid;user;full command line (Detlef Oertel <d.oertel@uib.de>)

## [4.11.4] - 2012-04-30

### Added

- new string function: getLinuxDistroType (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function: getLinuxVersionMap (Detlef Oertel <d.oertel@uib.de>)

### Changed

- port to Linux (Detlef Oertel <d.oertel@uib.de>)
- %opsiDepotId% (Detlef Oertel <d.oertel@uib.de>)
- changed unit names:
  - wisynth -> osparser
  - wibatch -> osbatchgui
  - wimain -> osmain
  - wifunc -> osfunc
  - wiwin32 -> osfuncwin2 (Detlef Oertel <d.oertel@uib.de>)

### Removed

- removed logginig via syslog (Detlef Oertel <d.oertel@uib.de>)

## [4.11.3.8] - 2013-08-01

### Changed

- changed TOpsiMethodCall.getJsonHashListString: converting string with SO before assingment (Detlef Oertel <d.oertel@uib.de>)

## [4.11.3.7] - 2013-07-29

### Added

- new command: isSuccess : Terminates Script execution with out error (Detlef Oertel <d.oertel@uib.de>)
- new command: noUpdateScript : Do not run a update after setup script even if it is there and we are not failed (Detlef Oertel <d.oertel@uib.de>)
- new function emptylist (Detlef Oertel <d.oertel@uib.de>)
- new function getlistcontainingList (Detlef Oertel <d.oertel@uib.de>)

### Changed

- changed DsiWin32 to work with lazarus 1.0.8/fpc 2.6.2 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.3.6] - 2013-03-11

### Fixed

- fix: no [productId] while listing of products (Detlef Oertel <d.oertel@uib.de>)

## [4.11.3.5] - 2012-12-05

### Added

- new: new winst cli parameter '/productlist \<productid>[,\<productid>]\* as sub parameter to /opsiservice
  try to run the setup-scripts of the given products, change installation states, send log files (Detlef Oertel <d.oertel@uib.de>)
- new: string function calculate(\<string expr>) ; calculates the mathematical expression and returns the result as string.
  on error the result is a empty string and the errorcounter is increased (Detlef Oertel <d.oertel@uib.de>)
- new command 'setConfidential \<confidential string>' ; adds the given string to a list of strings which are replaced with '\*\*\*(confidential)\*\*\*' while logging (Detlef Oertel <d.oertel@uib.de>)

### Changed

- del / delete will not delete if the given Filename starts with '\' (eg. '\Dummy') (Detlef Oertel <d.oertel@uib.de>)
- danish localization added (Detlef Oertel <d.oertel@uib.de>)
- italien localization added (Detlef Oertel <d.oertel@uib.de>)
- extended LDAPSearch (username/password) (Detlef Oertel <d.oertel@uib.de>)
- execwith: winst (/32bit|/64bit|/sysnative) no allowed (Detlef Oertel <d.oertel@uib.de>)
- execwith: if 'powershell.exe' is part of the interpreter string the script extension is '.ps1' (Detlef Oertel <d.oertel@uib.de>)
- winbatch: /32bit|/64bit|/sysnative no allowed (Detlef Oertel <d.oertel@uib.de>)
- switch to Lazarus 1.0 / fpc 2.6.0 (Detlef Oertel <d.oertel@uib.de>)
- wisynt: doAktionen, fileActionsMain: do ApplyTextConstants for filesections in fileActionsMain.
  fixes wrong resolution of %CurrentProfileDir% in Files /AllNTuserProfiles (Detlef Oertel <d.oertel@uib.de>)
- wisynt: doLinkFolderActions: linkfolder section now  usable in userLoginScripts:
  the predefined basfolders:
  'desktop', 'sendto', 'startmenu', 'programs', 'startup', 'desktopdirectory'
  are pointing to the loggedin user directories if called from userLoginScript (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix: do not store isfatalerror in registry if nor running as admin (Detlef Oertel <d.oertel@uib.de>)
- wilog: replace ' -->' by ' ==>' because the opsi web service get in trouble because it is interpreted as xml comment, fixes #350 (Detlef Oertel <d.oertel@uib.de>)
- wisynt: doLinkFolderActions: fix: now comments allowed between set_link and end_link (Detlef Oertel <d.oertel@uib.de>)
- wimain: ProcessNonZeroScript: fix: do not run update after setup if setup is failed, fixes #432 (Detlef Oertel <d.oertel@uib.de>)
- fix: widatamodul: opsidata4.initproduct now boolean function: false = error (Detlef Oertel <d.oertel@uib.de>)

## [4.11.3.4] - 2012-10-26

### Added

- new winbatch parameter /RunAsLoggedOnUser (Detlef Oertel <d.oertel@uib.de>)

### Changed

- PatchTextFile sections may now called with  %userprofiledir% as part of the file name and /AllNTuserProfiles
  so it will run on all profiles.
  If running in Machine mode, the '/AllNTuserProfiles' is implizit in the [ProfileActions].
  If running in LoginScript mode, the '%userprofiledir%' is the %CurrentProfileDir%. (Detlef Oertel <d.oertel@uib.de>)
- will now look for skins (more exactly: skin.ini) in the following sequence:
  \1) by parameter
  \2) new custom directory of opsi-client-agent ../custom/winstskin
  \3) own winstskin: winstskin (Detlef Oertel <d.oertel@uib.de>)

## [4.11.3.3] - 2012-09-13

### Added

- new command setActionProgress \<string> (Detlef Oertel <d.oertel@uib.de>)

### Changed

- commandline parameter /productid \<string>
  This parameter forces the winst to try to start a service connection using the
  local opsiclientd.conf. This will be used in the opsi-template-with-admin
  to run the local script in opsi context (Detlef Oertel <d.oertel@uib.de>)
- using getHashes instead of getObject if possible (Detlef Oertel <d.oertel@uib.de>)
- Versioninfo: try to find informations also on other than the stored codepage (Detlef Oertel <d.oertel@uib.de>)
- wisynth: doRegistryAllNTUserDats: use getProfilesDirList (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: retrieveJSONObject: JSON Bench Log entry for every service call (Detlef Oertel <d.oertel@uib.de>)
- wimain: saveVersionToProfile/readVersionFromProfile :
  if there is no access to Appdata, we use the profile directory for the .opsi.org\userLoginScripts.ini (Detlef Oertel <d.oertel@uib.de>)

## [4.11.3.2] - 2012-07-30

### Added

- new command FatalOnSyntaxError (default= true) (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function: getProfilesDirList: All existing Profiles dirs except:
  'localservice','networkservice','systemprofile' (Detlef Oertel <d.oertel@uib.de>)
- new stringfunction: GetIpByName(\<ip addr / ip name>) : string (Detlef Oertel <d.oertel@uib.de>)
- new stringlist commands to retrieve maps from the registry:
  \*\* getRegistryVarMap32(\<regkey>)
  \*\* getRegistryVarMap64(\<regkey>)
  \*\* getRegistryVarMapSysnative(\<regkey>) (Detlef Oertel <d.oertel@uib.de>)
- new string function: createNewOpsiHostKey : generates a random 32 chars long hex string (Detlef Oertel <d.oertel@uib.de>)

### Changed

- every script syntax error is logged (Detlef Oertel <d.oertel@uib.de>)
- every script syntax error increases the errorcounter (Detlef Oertel <d.oertel@uib.de>)
- if FatalOnSyntaxError then every script syntax error is a fatal error
  and 'Syntax Error' is set as actionProgress (Detlef Oertel <d.oertel@uib.de>)
- now isFatalError accepts a string which is stored to the server as actionprogress and showed in the opsi-configed (Detlef Oertel <d.oertel@uib.de>)
- includeLogFile now works in head mode if you give a negative number as line count
  \*\* changed in TLogInfo.includelogtail (Detlef Oertel <d.oertel@uib.de>)
- Patches sections may now called with  %userprofiledir% as part of the file name and /AllNTuserProfiles
  so it will run on all profiles.
  If running in Machine mode, the '/AllNTuserProfiles' is implizit in the [ProfileActions].
  If running in LoginScript mode, the '%userprofiledir%' is the %CurrentProfileDir%. (Detlef Oertel <d.oertel@uib.de>)
- Files sections changed: wisynth: doFileActions:
  Modifier /AllNtUserProfiles and /AllNtUserSendto now based on getProfilesDirList (Detlef Oertel <d.oertel@uib.de>)
- winbatch: (Startprocess\*) catching exceptions if execute bit is missing on samba4 shares (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix for win2k: remove all GetprocessId (Detlef Oertel <d.oertel@uib.de>)
- TJsonThroughHTTPS.retrieveJSONObject fix runtime error 210 / retry (Detlef Oertel <d.oertel@uib.de>)

## [4.11.3.1] - 2012-01-11

### Added

- new stringfunction: getDiffTimeSec (Time in seconds since last marktime) (Detlef Oertel <d.oertel@uib.de>)
- new stringfunction: stringReplace(string, oldPattern,newPattern) : string ;
  this function replaces all occurrences of one substring with another in a string
  it works not case sensitive , fixes #328 (Detlef Oertel <d.oertel@uib.de>)
- new stringfunction: GetMyIpByTarget(\<target ip addr>) : string (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function: getProductPropertyList(\<propname>,\<default value>) (Detlef Oertel <d.oertel@uib.de>)
- new stringfunction SidToName(\<well known sid>) : gives localized name of the sid (Detlef Oertel <d.oertel@uib.de>)
- new commands:
  include_insert "\<file name>"   : Replaces the command with a empty line
  and then inserts the content of "\<file name>" after this line
  include_append "\<file name>"   : Replaces the command with a empty line
  and then appends the the content of "\<file name>"
  to the end of the script
  \*\*  \<file name> is either:
  \*\*\* a complete valid Path
  \*\*\* The name of a file in %WinstDir%\lib
  \*\*\* The name of a file in %ScriptPath%
  \*\* should be placed after requiredWinstVersion (Detlef Oertel <d.oertel@uib.de>)
- new stringlist commands to retrieve lists from the registry:
  \*\* getRegistryKeyList32(\<regkey>)
  \*\* getRegistryKeyList64(\<regkey>)
  \*\* getRegistryKeyListSysnative(\<regkey>)
  \*\* getRegistryVarList32(\<regkey>)
  \*\* getRegistryVarList64(\<regkey>)
  \*\* getRegistryVarListSysnative(\<regkey>) (Detlef Oertel <d.oertel@uib.de>)
- new stringfunction strLength(\<string>) (Detlef Oertel <d.oertel@uib.de>)
- new stringfunction strPos(\<string>, \<substr>) (Detlef Oertel <d.oertel@uib.de>)
- new stringfunction strPart(\<string>, \<start pos>, \<number of chars>) (Detlef Oertel <d.oertel@uib.de>)
- new function: LocalAdmin \</Create | /Delete> (Detlef Oertel <d.oertel@uib.de>)

### Changed

- winbatch: /timeoutseconds now allowed without /waitforprocessending (not with /waitsec) (Detlef Oertel <d.oertel@uib.de>)
- winbatch /RunElevated (Detlef Oertel <d.oertel@uib.de>)
- manifest added: \<requestedExecutionLevel level="highestAvailable" />:
  http://www.lazarus.freepascal.org/index.php?topic=14976.0
  http://lazarus.freepascal.org/index.php/topic,11659.msg69861.html
  http://msdn.microsoft.com/en-us/library/bb756929.aspx (Detlef Oertel <d.oertel@uib.de>)
- Exitcode of DosBatch, Execwith is now logged and can be retrieved by getLastExitcode
  \*\* be aware: this is the exitcode of the called program so use something like:
  exit %ERRORLEVEL% , fixes #161 (Detlef Oertel <d.oertel@uib.de>)
- on userLoginScript: retry if there no user could be detected (Detlef Oertel <d.oertel@uib.de>)
- wisynth: TuibInstScript.doInfo: optimal fill for LabelInfo (Detlef Oertel <d.oertel@uib.de>)
- old boolean function: isNumber(\<str>) : boolean (Detlef Oertel <d.oertel@uib.de>)
- wisynt: try-except in doRegistryHackRegeditFormat (Detlef Oertel <d.oertel@uib.de>)
- logfiles now again not locked against reading while opsi-winst is running
  fixes: https://forum.opsi.org/viewtopic.php?f=7&t=3655 (Detlef Oertel <d.oertel@uib.de>)
- winbatch: test if extension of exitsting file is exe or com now not case sensitive.
  fixes: https://forum.opsi.org/viewtopic.php?f=7&t=3592#p18127
  not documented (Detlef Oertel <d.oertel@uib.de>)
- CPAU: using CREATE_NO_WINDOW (Detlef Oertel <d.oertel@uib.de>)
- winbatch /RunAsLoggedOnAdmin (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- Winbatch: The parameter to /timeoutseconds may now be a variable fixes #352 (Detlef Oertel <d.oertel@uib.de>)
- patchhosts sections: 64 bit compatibility, fixes #327 (Detlef Oertel <d.oertel@uib.de>)
- name of installing product shown in inital info block. fixes #367 (Detlef Oertel <d.oertel@uib.de>)
- wisynth: executeWith: use extension '.ps1' if interpreter = powershell, fixes #382 (Detlef Oertel <d.oertel@uib.de>)
- new function: NormalizeWinst (set normal window state), fixes #346 (Detlef Oertel <d.oertel@uib.de>)
- old function IconizeWinst  (set minimized window state), fixes #346 (Detlef Oertel <d.oertel@uib.de>)
- old function RestoreWinst  (set maximized window state), fixes #346 (Detlef Oertel <d.oertel@uib.de>)
- new boolean function: contains(\<str>, \<substr>) : boolean ; case sensitive, fixes #365 (Detlef Oertel <d.oertel@uib.de>)
- wispecfolder: fix using winapi call GetDefaultUserProfileDirectory for %DefaultUserProfileDir% (Detlef Oertel <d.oertel@uib.de>)
- wispecfolder: fix using winapi call GetAllUsersProfileDirectory for %CommonProfileDir% (Detlef Oertel <d.oertel@uib.de>)
- new control command: BlockInput \<true|false> which (un)blocks mouse and keyboard, fixes #244
  todo
  - jsonstr2str
  - wisynth: runtime error 219 at tsOpsiServiceHashList if  {$OBJECTCHECKS ON}
  - configuration of send behavior
  - opsi4servicecall getObjects
  - new function: LocalAdmin \</Create | /Delete> (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.12] - 2012-07-31

### Fixed

- fix for getproductmap widatamodul: TOpsi4Data.initProduct: reset FProductOnClient_aktobject with nil (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.11] - 2012-06-03

### Fixed

- wispecfolder: fix for the fix using winapi call GetProfilesDirectory for %profiledir% (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.10] - 2012-06-02

### Fixed

- wispecfolder: fix using winapi call GetProfilesDirectory for %profiledir% (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.9] - 2012-05-15

### Fixed

- wispecfolder: fix %CurrentProfileDir% in User Profile Management
  https://forum.opsi.org/viewtopic.php?f=7&t=3578&p=17148#p17148 (Detlef Oertel <d.oertel@uib.de>)
- getMapOfLoginscripts2Run: fix: avoid duplicate scripts (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.8] - 2012-04-26

### Changed

- update openSSL to 1.0.0i (http://www.openssl.org/news/secadv_20120419.txt)) (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.7] - 2012-03-27

### Changed

- Registry: set: Do not change data typ of reg_sz to reg_expand_sz if reg_sz is explict given
  even if there is a % char in the string.
  If no data typ is given reg_sz will be used as default
  but if there is a % char in the string, reg_expand_string will be used. (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.6] - 2012-02-13

### Added

- new winst function: ChangeDirectory \<dir>
  - set the working directory of the winst to \<dir>
  - will be reset to the start value at the end of every script (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.5] - 2012-01-20

### Changed

- update openSSL to 1.0.0g (http://www.openssl.org/news/secadv_20120118.txt) (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.4] - 2012-01-11

### Added

- added: wisynth: getProductMap: actionrequest (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.3] - 2011-12-27

### Added

- wiwin32: new: BlockInput (Detlef Oertel <d.oertel@uib.de>)
- wiwin32: new: getProfileImagePathfromSid (Detlef Oertel <d.oertel@uib.de>)

### Changed

- wisynth: doRegistryAllNTUserDats: work via sid, if found dir =  getProfileImagePathfromSid (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix: widatamodul: TOpsi4Data.initProduct: no crash if product_hash is empty (Detlef Oertel <d.oertel@uib.de>)
- fix: widatamodul: TOpsi4Data.getMapOfLoginscripts2Run: only products which on the used depot (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.2] - 2011-12-08

### Changed

- change: wilog: if a log file coud not be opened,
  try with extension 0 - 9 and at least continue without logging
  do no user interaction (Detlef Oertel <d.oertel@uib.de>)
- wisynth: TuibInstScript.doRegistryAllNTUserDats: do not to try the temporary pcpatch account or SYSTEM
  - sense less and may fail (Detlef Oertel <d.oertel@uib.de>)
- gives mswindows version and achitecture at the beginning of the logfile (Detlef Oertel <d.oertel@uib.de>)
- Winbatch: 'waiting for ending' messages at loglevel 7 (Detlef Oertel <d.oertel@uib.de>)
- registry /allntuserdats: if NT5 and ntuser.dat could not be loaded then try to patch via SID (except Localservice or networkservice) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fix: widatamodul: TOpsi4Data.getMapOfLoginscripts2Run: get also scripts which are not in productsonclient (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: fix getActualProduct\* : works if product is not in productOnClient (Detlef Oertel <d.oertel@uib.de>)
- fix (crashed WAN-Extension): TJsonThroughHTTPS.makeURL + wisynth: TuibInstScript.doOpsiServiceCall :
  - makeUrl: not laways use /opsiclientd on calling localhost
  - include /opsiclientd in serviceurl on 'opsiservicecall /opsiclientd' (Detlef Oertel <d.oertel@uib.de>)

## [4.11.2.1] - 2011-11-01

### Added

- new parameter /allloginscripts (and /loginscripts) may be used with /opsiservice (Detlef Oertel <d.oertel@uib.de>)
- new parameter /silent may be used with /opsiservice (Detlef Oertel <d.oertel@uib.de>)
- new parameter /sessionid may be used with /opsiservice (Detlef Oertel <d.oertel@uib.de>)
- new primary section [ProfileActions]: if running in LoginScript mode and ProfileAction exists,
  it will be executed instead of [actions]
  - wisynth: look for NameProfileActionsSection
  if ProfileActions is called as sub in machine mode all Files or Registry calls in this sub
  are implicit called with /AllNTUserDats resp. /AllNTUserProfiles
  - wisynth: TuibInstScript.doFileActions and look for NameProfileActionsSection in TuibInstScript.doAktionen (Detlef Oertel <d.oertel@uib.de>)
- wimain: new: TCentralForm.Loginscripts; (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: new: TOpsi4Data.getMapOfLoginscripts4InstalledProducts (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: new TOpsi4Data.withRoamingProfiles : boolean; (Detlef Oertel <d.oertel@uib.de>)
- new boolean winst function isLoginScript (Detlef Oertel <d.oertel@uib.de>)
- new string winst function: GetScriptMode , possible values 'Machine','Login' (Detlef Oertel <d.oertel@uib.de>)
- wispecfolder: new getUserProfilePath and getProfilesPath (Detlef Oertel <d.oertel@uib.de>)
- new winst constant %CurrentProfileDir% which points to the loggedin users profile dir (Detlef Oertel <d.oertel@uib.de>)
- new winst function: includelog \<file name> \<tail size> (Detlef Oertel <d.oertel@uib.de>)
- new opsiservicecall parameter: /opsiclientd : calls the localhosts opsiclientd (Detlef Oertel <d.oertel@uib.de>)
- new winst file command del : works like delete but on "del -s -f c:\not-exists"
  if c:\not-exists not exists it do not search complete c:\ for "not-exits" (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: new: sendlog(type) ; type = instlog / userlogin (Detlef Oertel <d.oertel@uib.de>)
- new winst stringlist function getProductMap in wisynth
  keys are: id, name, description, advice, productversion, packageversion, priority,
  installationstate, lastactionrequest, lastactionresult, installedversion,
  installedpackage, installedmodificationtime (Detlef Oertel <d.oertel@uib.de>)
- new winst string function getValueBySeparator (\<key>,\<separator>,\<list>) (Detlef Oertel <d.oertel@uib.de>)
- new winst string function unquote (\<string,\<quote-string>) (Detlef Oertel <d.oertel@uib.de>)
- new winst commands:
  - saveVersionToProfile - save productversion-packageversion to local profile
  - readVersionFromProfile : string - read productversion-packageversion from local profile
  - scriptWasExecutedBefore : boolean - is true if saved and running productversion-packageversion are identical (Detlef Oertel <d.oertel@uib.de>)
- new undocumented winst function: ShrinkFileToMB \<file name> \<new size in MB> (Detlef Oertel <d.oertel@uib.de>)

### Changed

- earlier visibility of FBatchOberflaeche (Detlef Oertel <d.oertel@uib.de>)
- wimain new boolean flag: runloginscripts (winst called with /loginscripts or /allloginscripts) (Detlef Oertel <d.oertel@uib.de>)
- wimain new boolean flag: inUsercontext (is true if called with /allloginscripts or /loginscripts but may be switched in the script) (Detlef Oertel <d.oertel@uib.de>)
- code cleanup (Detlef Oertel <d.oertel@uib.de>)
- Temporary files (from e.g. Dosbatch) are now for security reasons at c:\opsi.org\tmp (Detlef Oertel <d.oertel@uib.de>)
- wisynth: doRegistryHack: HKCU removed from key on (flag_all_ntuser or runLoginScripts) (Detlef Oertel <d.oertel@uib.de>)
- wisynth: doRegistryHackRegeditFormat: HKCU removed from key on (flag_all_ntuser or runLoginScripts) (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: finishOpsiConf: if isLoginScript then Log is send via webservice as type 'userlogin' (Detlef Oertel <d.oertel@uib.de>)
- on loginscripts the Filesection constant %userprofiledir% is replaced by %CurrentProfileDir% (Detlef Oertel <d.oertel@uib.de>)
- on loginscripts in Filesections %CurrentProfileDir% is the preset targetdir (Detlef Oertel <d.oertel@uib.de>)
- in files sections with /Allntuserprofiles %CurrentProfileDir% may now used instead of %userprofiledir% (Detlef Oertel <d.oertel@uib.de>)
- code cleanup: windatamodule (datamodule1): completly removed (Detlef Oertel <d.oertel@uib.de>)
- logs of the xml-patch sections are now included by file (Detlef Oertel <d.oertel@uib.de>)
- TOpsiDataJSONService.finishOpsiConf: close session  (winst now closes the session) (Detlef Oertel <d.oertel@uib.de>)
- wilogsrv removed (Detlef Oertel <d.oertel@uib.de>)
- wiglob renamed to wilog (because it contains only logging functions) (Detlef Oertel <d.oertel@uib.de>)
- wilog: writes now simultan to the main log file a partlog to c:\opsi.org\log
  which will be not appended and removed at the end (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: changed: sendlog now works allways in append mode (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: changed: sendlog send partlog not full log and shrink it to 5 MB if needed (Detlef Oertel <d.oertel@uib.de>)
- wimain: Button_ExecuteClick: TLogInfo.Create only if nil (avoids blocked logfiles while ineractive working) (Detlef Oertel <d.oertel@uib.de>)
- cleanup TOpsiData (remove Classic and JSONService) (Detlef Oertel <d.oertel@uib.de>)
- sending userlogin logs allways in append mode, instlog only if needed (Detlef Oertel <d.oertel@uib.de>)
- use /usercontext if given - if not, try to detect logged in user (Detlef Oertel <d.oertel@uib.de>)
- cleanup log messages (Detlef Oertel <d.oertel@uib.de>)

### Removed

- code cleanup: wibtch: removed : IViewService, TTestView, TInfoView (Detlef Oertel <d.oertel@uib.de>)
- remove all winst.bat directly after use (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- fixed stringlist function loadunicodetextfile (Detlef Oertel <d.oertel@uib.de>)
- fixed Registry: deletekey with /AllNTuserDats
  (fixed at wifunc: TuibRegistry.OpenKey / TuibRegistry.OpenExistingKey
  and at wisynth: doRegistryHack : deletekey) (Detlef Oertel <d.oertel@uib.de>)
- fixed parser error for stringlist functions without arguments (like getMSVersionMap) (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.8-1] - 2011-10-31

### Changed

- extended string replace for control characters in TOpsi4Data.sendLog and TOpsiMethodCall.Create (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.7-1] - 2011-10-21

### Fixed

- wisynth: fix in doRegistryHackRegeditFormat: close key before opening a new one (Detlef Oertel <d.oertel@uib.de>)
- wisynth: fix in doRegistryAllNTUserDats: check if branch = logedon user before working on HK_USERS\sid (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.6-1] - 2011-09-09

### Fixed

- wifunc: fix in startprocess: go back to the 4.10 Delphi code without TProcess (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.5-1] - 2011-09-09

### Fixed

- wiwin32: KillProcessbyname: fix: UpperCase(exename) (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.4-1] - 2011-08-31

### Added

- wifunc: new boolean function FuncSaveToFile (Detlef Oertel <d.oertel@uib.de>)

### Changed

- wisynth: execDOSBatch / execPython : use tempfile name with incremented number (Detlef Oertel <d.oertel@uib.de>)
- wifunc: TXStringList.SaveToFile: 5 retries (Detlef Oertel <d.oertel@uib.de>)
- wisynth: execDOSBatch / execPython / ExecWith : FuncSaveToFile instead of proc saveToFile (Detlef Oertel <d.oertel@uib.de>)
- wisynth: execDOSBatch / execPython / ExecWith : switch to fatal if FuncSaveToFile returns false (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.3] - 2011-08-26

### Changed

- wibtch: setPicture: optimalfill for Labelproduct only if label is large (Detlef Oertel <d.oertel@uib.de>)
- typo in german po file (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.2] - 2011-06-09

### Added

- new: TuibInstScript.doRegistryAllNTUserDats: patches logged in user by HKUSER/SID (Detlef Oertel <d.oertel@uib.de>)
- new Registry modifier: /usercontext:
  - wispecfolder: initializeFoldersForUser(domain,user)
  - used in wimain: TCentralForm.FormCreate
  - uses wiwin32: Impersonate2User, getPid4user (Detlef Oertel <d.oertel@uib.de>)
- new stringlist function: getProcessList gives List of exename;pid;dom/user (Detlef Oertel <d.oertel@uib.de>)
- new function GetLoggedInUser (Detlef Oertel <d.oertel@uib.de>)
- new function GetUsercontext (Detlef Oertel <d.oertel@uib.de>)
- new function SetUsercontext (needed for testing) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- removing FPC ifdefs (it is a FPC only source now) (Detlef Oertel <d.oertel@uib.de>)
- adding IFDEF WINDOWS to start the way towards linux (Detlef Oertel <d.oertel@uib.de>)
- code cleanup (Detlef Oertel <d.oertel@uib.de>)
- wishowfile: Richedit1.MaxLength:=MaxInt; to display long logs (Detlef Oertel <d.oertel@uib.de>)
- killtask now kill also tasks which are started by other (Detlef Oertel <d.oertel@uib.de>)
- default depotdir does not contain 'install' anymore (Detlef Oertel <d.oertel@uib.de>)
- wifunc: FileCopy: retries if setTimeForFile fails because file in use (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.1] - 2011-04-07

### Added

- wifunc: new expandfilename(): wrapper to sysutils.filename which expands '' to the actual drive (no good idea) (Detlef Oertel <d.oertel@uib.de>)
- wifunc: new internal function: runningasadmin (Detlef Oertel <d.oertel@uib.de>)

### Changed

- wifunc: TuibFileInstall.AllDelete: filename no more expanded (can lead to dangerous results) (Detlef Oertel <d.oertel@uib.de>)
- wifunc: TuibFileInstall.AllDelete: denied delete of c:, C:\, \, \\, c:\windows, c:\windows\system32 (Detlef Oertel <d.oertel@uib.de>)
- wisynth: TuibInstScript.doXMLPatch: xmlfilename quoted (Detlef Oertel <d.oertel@uib.de>)
- error handling in wisynth: doOpsiServiceCall: checkAndRetrieveMap and checkAndRetrieveStringList (Detlef Oertel <d.oertel@uib.de>)
- error handling in wiglob: DependentAddStringList (Detlef Oertel <d.oertel@uib.de>)
- translation via i18n (Detlef Oertel <d.oertel@uib.de>)
- TFBatchOberflaeche.FormClose: prevent closing via Alt-F4 (Detlef Oertel <d.oertel@uib.de>)
- wisynth: execDOSBatch: now (winst /62Bit) or (winst /sysnative) or (winst /32Bit) allowed
  this will execute the c:\windows\cmd64.exe. If this file not exists, it will be created
  by copying the real 64Bit cmd.exe to c:\windows as cmd64.exe (Detlef Oertel <d.oertel@uib.de>)
- upgrade to ssl Libraries 1.0.0.d (libeay32.dll, ssleay32.dll) (Detlef Oertel <d.oertel@uib.de>)
- now trunk (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- ErrorNumberMarked will no more set to NumberOfErrors at every section start fixes #219 (Detlef Oertel <d.oertel@uib.de>)
- wisynth: CreateAndProcessScript: no interactive message if script not found. fixes #245 (Detlef Oertel <d.oertel@uib.de>)
- wimain: TCentralForm.ProcessProdukt: Actionrequest to update after setup. fixes #191 (Detlef Oertel <d.oertel@uib.de>)
- wispecfolder + wisynth: new constant %DefaultUserProfileDir%  fixes #232 (Detlef Oertel <d.oertel@uib.de>)
- wisynth: new boolean function runningAsAdmin and Warning in the log if not running as admin , fixes #93 (Detlef Oertel <d.oertel@uib.de>)

## [4.11.1.0] - 2011-03-10

### Changed

- xmlpatch plugin integrated (Detlef Oertel <d.oertel@uib.de>)
- switch to Lazarus 0.9.30 (Detlef Oertel <d.oertel@uib.de>)
- change: wisynth: full section name now displayed in Labeldetail (Detlef Oertel <d.oertel@uib.de>)
- change widata: retrieveJSONObject: unexpected result only if ResultLines.Count \< 1 (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- dynamic size of LabelProduct fixes #239 (Detlef Oertel <d.oertel@uib.de>)
- change: wimain: readConfig: utilsurl and configurl removed, fixes #235 (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: some fixes to avoid access violations in swaudit (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: fixes in withLicenceManagement (Detlef Oertel <d.oertel@uib.de>)

## [4.11.0.0] - 2010-12-30

### Changed

- very experimental: lazarus compiled winst (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.15] - 2011-04-08

### Changed

- wisynth: TuibInstScript.executeWith: waitsec initialized with 0 (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.14] - 2010-12-28

### Changed

- widatamodul: TOpsi4Data.getOpsiModules : ISuperObject; with opsi 4 methods (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.13] - 2010-12-28

### Changed

- widatamodul: TOpsiMethodCall.getJsonHashListString:
  to avoid datatype conversion values can be prefixed with:
  'opsistring:' (Detlef Oertel <d.oertel@uib.de>)

## [4.10.9.10-1] - 2010-11-19

### Added

- new constants: %CurrentAppDataDir%, %ScriptDir%, %CommonProfileDir%, (Detlef Oertel <d.oertel@uib.de>)

### Changed

- (4.10.9.0a) (Detlef Oertel <d.oertel@uib.de>)
- wimain: ProcessNonZeroScript: logmessage 'no script file name given' now LLNotice instead of warning (Detlef Oertel <d.oertel@uib.de>)
- widatamodul: getOpsiServerVersion: avoid check for serverversion 3 if we get a 'HTTP/1.1 401 Unauthorized' at the first try (Detlef Oertel <d.oertel@uib.de>)
- wispecfolder: get folder locations now from API instead from Registry (Detlef Oertel <d.oertel@uib.de>)
- constant names changed:  %StartmenuDir% -> %CurrentStartmenuDir%,
  %DesktopDir% -> %CurrentDesktopDir%,
  %StartupDir% -> %CurrentStartupDir%,
  %ProgramsDir% -> %CurrentProgramsDir%,
  %SendToDir% -> %CurrentSendToDir% (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- wimemo: window in screen center, larger font, no more autosize -> for showmessagefile, fixes #184 (Detlef Oertel <d.oertel@uib.de>)
- wisynth:  product / version at the end of the log , fixes #110 (Detlef Oertel <d.oertel@uib.de>)
- wisynth: getLocaleInfoMap: system_default_language_id now from  HKLM\System\CurrentControlset\Control\Nls\Language','InstallLanguage', fixes #113 (Detlef Oertel <d.oertel@uib.de>)
- wimain: StartProgramModes: No interactive message if no connection to service, fixes #117 (Detlef Oertel <d.oertel@uib.de>)
- wiglob: DependentAdd: possibility to abort trace mode, fixes #129 (Detlef Oertel <d.oertel@uib.de>)
- wisynth: No registry redirection for win2k even at /64Bit /Sysnative    (win2k can't handle this), fixes #197 (Detlef Oertel <d.oertel@uib.de>)
- widata: TOpsiMethodCall.getJsonUrlString : String quoted if not in {} (a JSON Object), fixes #198 (Detlef Oertel <d.oertel@uib.de>)
- new constants: %CommonStartmenuDir% ,%CommonDesktopDir%, %CommonStartupDir%, %CommonProgramsDir%, %AppDataDir%, %StartmenuDir%, %DesktopDir%, %StartupDir%, %ProgramsDir%, %SendToDir%, fixes #175
  (4.10.9.0b) (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.10-1] - 2010-11-25

### Added

- new function: TOpsi4Data.setProductProgress (myprogres : String); (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- bugfix: progress was not set on running setup script. fixed in widatamodul:TOpsi4Data.setProductState (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.9-1] - 2010-11-09

### Changed

- (4.10.8.9a) (Detlef Oertel <d.oertel@uib.de>)
- uJson removed and replaced by superobject
  (4.10.8.9c) (Detlef Oertel <d.oertel@uib.de>)
- TJsonThroughHTTPS.retrieveJSONObjectByHttpPost uses at opsi4 compressed stream (sendlog) (part of #114) (Detlef Oertel <d.oertel@uib.de>)
- TJsonThroughHTTPS.retrieveJSONObject  uses at opsi4 compressed streams (Detlef Oertel <d.oertel@uib.de>)
- TJsonThroughHTTPS.retrieveJSONArray  uses at opsi4 compressed streams (Detlef Oertel <d.oertel@uib.de>)

### Removed

- cleanup: Topsi4data: remove not used functions
  (4.10.8.9b) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- bugfix: at getOpsiServerVersion: if not opsi 4, try to ask opsi3 server (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.8-1] - 2010-10-26

### Fixed

- bugfix: at getOpsiServerVersion reduce errorcounter by 1 if not opsi 4 (bugfix silent installation) (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.7-1] - 2010-10-19

### Fixed

- bugfix: SSL connection via SSLv3    fixes #165 (Rupert Roeder <r.roeder@uib.de>)

## [4.10.8.6-1] - 2010-09-17

### Fixed

- bugfix: writing log file only once (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.5-1] - 2010-09-15

### Fixed

- bugfix: Registry deletekey /64 does not work with win 5.2, fixes #131 (Detlef Oertel <d.oertel@uib.de>)
- bugfix: widatamodule: getOpsiServerVersion defaults to '3' (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.4-1] - 2010-09-01

### Fixed

- bugfix: LogError and LogWarning wasn't loged if Loglevel was \<=4, fixes #115 (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.3-1] - 2010-08-23

### Added

- added: new constant %WinstVersion% (Detlef Oertel <d.oertel@uib.de>)
- new (documented):  CompareDotSeparatedStrings(\<string1>, \<string2>) (Detlef Oertel <d.oertel@uib.de>)
- new (documented):  CompareDotSeparatedNumbers(\<string1>, \<string2>) (Detlef Oertel <d.oertel@uib.de>)
- new: winst write the real start time to the first log entry (Detlef Oertel <d.oertel@uib.de>)
- new: convert2Jsonstr (Detlef Oertel <d.oertel@uib.de>)
- new: writing message 'Sending log file to server ...' on sendlog (Detlef Oertel <d.oertel@uib.de>)

### Changed

- readded: constant %installingProduct% is valid again but deprecated (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- added: support for actionrequest: 'custom', fixes #71 (Detlef Oertel <d.oertel@uib.de>)
- bugfix: if requiredWinstversion fails installation state is set to failed and shows no more interactive message, fixes #80 (Detlef Oertel <d.oertel@uib.de>)
- bugfix: in find&retry in winbatch: call with a not heading '"' will now treated correctly, fixes #104 (Detlef Oertel <d.oertel@uib.de>)
- bugfix: getOpsiServerVersion: defaulting to opsi 3 (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.2-1] - 2010-08-11

### Fixed

- bugfix: opsi service Version 3: Product installation sequence is calculated by winst (Detlef Oertel <d.oertel@uib.de>)
- bugfix: winbatch: if the targefile dont contain path separators we assume
  it is a command in the path and we don't try to check the existing of the target file, fixes #92 (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.1-1] - 2010-08-09

### Fixed

- bugfix: winbatch: no retry to find if there is no parameter; fixes #87 (Detlef Oertel <d.oertel@uib.de>)

## [4.10.8.0-1] - 2010-06-22

### Added

- added support for jpeg pictures at 'Showbitmap' (Detlef Oertel <d.oertel@uib.de>)
- new Modifier for File and Registry Sections and GetRegistry and FileExists Functions: /32Bit /SysNative (Detlef Oertel <d.oertel@uib.de>)
- new unit: locale.pas (Detlef Oertel <d.oertel@uib.de>)
- new: LangCodeByHex  (gives de-DE by 0x0407 , gives xx-Null by 0x0000) (Detlef Oertel <d.oertel@uib.de>)
- new: getLocaleInfoMap: new key: system_default_language_id (Detlef Oertel <d.oertel@uib.de>)
- new: getLocaleInfoMap: new key: system_default_posix (Detlef Oertel <d.oertel@uib.de>)
- new: getSlowInfoMap(productkey,noredirect):
  --> reads slowinfocache; productkey is the key below ARPCache;
  --> noredirect is 'on'/'off' for 64 Bit Registry redirection (not implemented yet)
  --> hash key may be: hasname, installsize, lastused, usagefrequency, binaryname (Detlef Oertel <d.oertel@uib.de>)
- new getFileInfoMap: 'file version with dots' (Detlef Oertel <d.oertel@uib.de>)
- new getFileInfoMap: 'Language name \<index>' (Detlef Oertel <d.oertel@uib.de>)
- new getFileInfoMap: 'Language ID \<index>' (Detlef Oertel <d.oertel@uib.de>)
- new getFileInfoMap: 'file version' (Detlef Oertel <d.oertel@uib.de>)
- new getFileInfoMap: 'product version' (Detlef Oertel <d.oertel@uib.de>)
- new: tries to get product sequence from server and if this fails falls back to own sorting (Detlef Oertel <d.oertel@uib.de>)
- new: in wisynth,TuibInstScript.doLogEntries labelDetail is set to |/-\ instead of if,then,else,endif (Detlef Oertel <d.oertel@uib.de>)
- new unit wiswaudit with functions for swaudit (Detlef Oertel <d.oertel@uib.de>)
- new: wisynth: addlisttolist(list,list) (Detlef Oertel <d.oertel@uib.de>)
- new: wisynth: getSwauditInfoList(uninstallkey,listtype,archticture) (Detlef Oertel <d.oertel@uib.de>)
- new unit base64 (Detlef Oertel <d.oertel@uib.de>)
- new: wisynth: SetOutputLevel(int) 3=full, 2=no Activity Label, 1=no Detail and no Activity Labels (Detlef Oertel <d.oertel@uib.de>)
- new: wibtch: Output to activity-,detail- and command-label  not more than 5 per second (Detlef Oertel <d.oertel@uib.de>)
- added: widatamodul: opsidata: actualProductVersion calls (Detlef Oertel <d.oertel@uib.de>)
- new: wisynth: constants: %installingProdName% and %installingProdVersion% (Detlef Oertel <d.oertel@uib.de>)
- new: widatamodul: getOpsiServerVersion (to decide which methods should be used) (Detlef Oertel <d.oertel@uib.de>)
- new: widatamodul: getOpsiServiceVersion (to decide which methods should be used) (Detlef Oertel <d.oertel@uib.de>)
- new: widatamodul: new opsidata: opsi4data for opsi4 service methods (Detlef Oertel <d.oertel@uib.de>)
- new: wisynt: new constants %installingProdName% %installingProdVersion% (Detlef Oertel <d.oertel@uib.de>)
- new: Warning if a script try to reopen a existing service connection (Detlef Oertel <d.oertel@uib.de>)
- new: widatamodule: on bootmode REINS Product sequence calculated with dependencies by the opsi-server (Detlef Oertel <d.oertel@uib.de>)
- new: show batch screen from the start not as Icon (while talking to the server) (Detlef Oertel <d.oertel@uib.de>)
- new: argument of commandline parameter /opsisevice may have a trailing '/rpc' (Detlef Oertel <d.oertel@uib.de>)
- new: wisynt: In winbatch sections we look for the target file and retry to find it 10 times.
  This should prevent Errors at unsteady network connections. (Detlef Oertel <d.oertel@uib.de>)
- new: superobject JSON library from http://www.progdigy.com/?page_id=6 (Detlef Oertel <d.oertel@uib.de>)
- new: FProductOnClient_aktobject is now from type ISuperobject (Detlef Oertel <d.oertel@uib.de>)
- new: widatamodul: opsi4data: using now opsi4 method productOnClient_update no set product states (Detlef Oertel <d.oertel@uib.de>)

### Changed

- changed: widatamodul: opsidata: all actualProduct calls to actualProductName calls (Detlef Oertel <d.oertel@uib.de>)
- change: set, if,then,else,endif now loglevel LLnotice (Detlef Oertel <d.oertel@uib.de>)
- change: get sorted product list by productOnClient_getObjects (Detlef Oertel <d.oertel@uib.de>)
- Config path and utils path is not supported anymore (Detlef Oertel <d.oertel@uib.de>)
- change: wifunc: CheckFileExists we look for the target file and retry to find it 10 times.
  This should prevent Errors at unsteady network connections. (Detlef Oertel <d.oertel@uib.de>)
- change: wisynt: set command logs now varname and new value (Detlef Oertel <d.oertel@uib.de>)

### Removed

- removed: wisynth: constants: %installingProduct% (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- New Constants: %ProgramFiles32Dir%, %ProgramFiles64Dir%, %ProgramFilesSysnativeDir%, fixes #25 (Detlef Oertel <d.oertel@uib.de>)
- new: getLocaleInfoMap: new key: system_default_lang_region,  fixes #30 (Detlef Oertel <d.oertel@uib.de>)
- new: wisynth: functions base64EncodeStr, base64DecodeStr, fixes #38 (Detlef Oertel <d.oertel@uib.de>)
- fixed: GPL Header with corrected links in the source code, fixes #32 (Detlef Oertel <d.oertel@uib.de>)
- fixed: instlog.err entries only if loglevel \<= LLwarning (Detlef Oertel <d.oertel@uib.de>)
- fixed: wibtch: InnerWidth and InnerHeight little bit larger (Detlef Oertel <d.oertel@uib.de>)

## [4.10.7.4-1] - 2010-05-29

### Added

- added GPL (Detlef Oertel <d.oertel@uib.de>)
- added: widatamodul: IdHTTP.Request.UserAgent:= 'opsi-winst '+wimain.WinstVersion; (Detlef Oertel <d.oertel@uib.de>)
- added: wibatch: ImageProduct.Repaint; (Detlef Oertel <d.oertel@uib.de>)
- new function addtolist(list,string) (Detlef Oertel <d.oertel@uib.de>)
- new section OpsiServiceHashList to send list of hashes (Detlef Oertel <d.oertel@uib.de>)

### Changed

- moved history from winst.dpr to changelog.txt (Detlef Oertel <d.oertel@uib.de>)
- Change: opsisevicecall now via post: widatamodul: TJsonThroughHTTPS: methodGet set to false. Using IdHTTP.Post(Furl,sendstream, mymemorystream) (Detlef Oertel <d.oertel@uib.de>)
- Enhancement: try to reopen log file if writing to log file fails (Detlef Oertel <d.oertel@uib.de>)

### Removed

- removed all kylix port units and compiler directives (Detlef Oertel <d.oertel@uib.de>)
- removed all dll compiler directives (Detlef Oertel <d.oertel@uib.de>)
- removed all 16 Bit parts (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- Bugfix: FormCreate from SystemInfo and Showtextfile in winst.dpr (Detlef Oertel <d.oertel@uib.de>)
- Bugfix: now also the first load of log file is colored (Detlef Oertel <d.oertel@uib.de>)
- Bugfix: Fonts / Passwordchar in wiservicepassword (Detlef Oertel <d.oertel@uib.de>)
- BugFix: wisynth: in TuibInstScript.doAktionen: UpdateMemo Block replaced by ProcessMess. This was a strong performance break. (Detlef Oertel <d.oertel@uib.de>)
- BugFix: wifunc: registry read strings: If StrLen() gives a smaller value then the Winapi StrLen is used. (Detlef Oertel <d.oertel@uib.de>)

## [4.10.6.4-1] - 2010-04-29

### Changed

- widatamodul new recreated (Detlef Oertel <d.oertel@uib.de>)
- wimesg new created (Detlef Oertel <d.oertel@uib.de>)
- creation of MyMessageDlg now in wimain.form.create (avoids crash at syntax error in opsi mode) (Detlef Oertel <d.oertel@uib.de>)

## [4.10.5.0-1] - 2010-04-21

### Changed

- TOpsiDataJSONService.sendLog send log as UTF-8 (Detlef Oertel <d.oertel@uib.de>)
- start messages at log level LLessential (Detlef Oertel <d.oertel@uib.de>)
- name for main section changed from 'aktionen' to 'actions'
  -> 'aktionen' still works but is deprecated from now on (Detlef Oertel <d.oertel@uib.de>)

## [4.10.4.0-1] - 2010-04-20

### Fixed

- bug fix in randomstr: now contains always 2 lower chars, 2 upper chars, 2 special chars, and 4 numeric chars (Detlef Oertel <d.oertel@uib.de>)

## [4.10.3.0-1] - 2010-04-07

### Added

- new loglevels
  (\* new Loglevels since opsi 3.5:
  0 = nothing (absolute nothing)
  1 = essential ("unverzichtbares")
  2 = critical (unexpected errors that my cause a program abort)
  3 = error (Errors that don't will abort the running program)
  4 = warning (you should have a look at this)
  5 = notice (Important statements to the program flow)
  6 = info (Additional Infos)
  7 = debug (important debug messages)
  8 = debug2 (a lot more debug informations and data)
  9 = confidential (passwords and other security relevant data)
  \*) (Detlef Oertel <d.oertel@uib.de>)
- new command \<setLogLevel=> (Detlef Oertel <d.oertel@uib.de>)
- New: Parameter '/64Bit' for Files-Sections (disables file redirection)
  using: DSiDisableWow64FsRedirection and DSiRevertWow64FsRedirection
  see http://msdn.microsoft.com/en-us/library/aa365745%28v=VS.85%29.aspx (Detlef Oertel <d.oertel@uib.de>)
- New: Funktion FileExists64 with disabled file redirection at 64 Bit (Detlef Oertel <d.oertel@uib.de>)
- New: Registry: For 64Bit and AllNtUserDats own redirection (Detlef Oertel <d.oertel@uib.de>)
- new: takeFirstStringContaining(\<list>,\<search string>) (Detlef Oertel <d.oertel@uib.de>)
- new: getlistcontaining(\<list>,\<search string>) (Detlef Oertel <d.oertel@uib.de>)

### Changed

- code cleanup (Detlef Oertel <d.oertel@uib.de>)
- \<LogLevel=> is deprecated and now implemented as loglevel := loglevel+4 for backward compatibility (Detlef Oertel <d.oertel@uib.de>)
- GetSystemtype now using windows api wrapper DSiIsWow64 (Detlef Oertel <d.oertel@uib.de>)
- Log-Level and timestamp is shown at the start of every Log-Line (Detlef Oertel <d.oertel@uib.de>)
- LogView with search, color, font size regulation, loglevel filter (Detlef Oertel <d.oertel@uib.de>)
- Const %ProfileDir% via DSiGetFolderLocation(CSIDL_PROGRAM_FILES) (Detlef Oertel <d.oertel@uib.de>)
- wimesg with Richedit component (gives the possibility for copy the messages, and bette encoding than memo) (Detlef Oertel <d.oertel@uib.de>)

### Fixed

- bug fix in postinst (']' missing; also fixed in 4.10.1.0-2) (Detlef Oertel <d.oertel@uib.de>)

## [4.10.2.1-1] - 2010-04-01

### Added

- new interactive GUI (Detlef Oertel <d.oertel@uib.de>)

### Changed

- All clx components removed (Detlef Oertel <d.oertel@uib.de>)
- Message also writes to log (Detlef Oertel <d.oertel@uib.de>)
- Log-Level is show at the start of every Log-Line (Detlef Oertel <d.oertel@uib.de>)

## [4.10.1.0-1] - 2010-03-23

### Added

- New: DecStrToHexStr (Detlef Oertel <d.oertel@uib.de>)
- New: HexStrToDecStr (Detlef Oertel <d.oertel@uib.de>)

## [4.10.0.0-1] - 2010-03-23

### Added

- New: getMSVersionMap (Detlef Oertel <d.oertel@uib.de>)
- New: GetRegistryStringValue64 (Detlef Oertel <d.oertel@uib.de>)
- New: Parameter '/64Bit' for Registry-Sections (Detlef Oertel <d.oertel@uib.de>)

### Changed

- Background infos for getMSVersionMap
  - http://msdn.microsoft.com/en-us/library/ms724385%28VS.85%29.aspx
  - http://www.delphi-treff.de/tipps/system/wiki/Windows-Version%20ermitteln/
  - http://www.tek-tips.com/viewthread.cfm?qid=1573774&page=6
  - http://msdn.microsoft.com/en-us/library/dd419805.aspx
  - http://msdn.microsoft.com/en-us/library/ms724833%28VS.85%29.aspx (Detlef Oertel <d.oertel@uib.de>)
- Background infos for 64 Bit Registry access:
  - http://msdn.microsoft.com/en-us/library/aa384129%28VS.85%29.aspx
  - http://msdn.microsoft.com/en-us/library/ms724878%28VS.85%29.aspx
  - http://www.delphipraxis.net/topic103630_registry+wow6432node.html (Detlef Oertel <d.oertel@uib.de>)
- postinst: copy files only to utils dir if it exists (Detlef Oertel <d.oertel@uib.de>)

## [4.9.4.2-1] - 2009-01-12

### Changed

- improved focus handling in interactive login to configuration
  service (Rupert Roeder <r.roeder@uib.de>)

## [4.9.4.1-1] - 2009-01-11

### Changed

- in interactive mode, the batch gui is only shown when activated by
  a button (Rupert Roeder <r.roeder@uib.de>)

## [4.9.4.0-1] - 2009-01-07

### Added

- new experiment for getting a second entry in the taskbar (Rupert Roeder <r.roeder@uib.de>)

## [4.9.3.5-1] - 2009-12-14

### Changed

- increased logging for setting product action requests on reinstall (Rupert Roeder <r.roeder@uib.de>)

## [4.9.3.4-1] - 2009-11-20

### Added

- new algorithm for catch output (Rupert Roeder <r.roeder@uib.de>)

## [4.9.3.3-1] - 2009-11-13

### Fixed

- bugfix (Rupert Roeder <r.roeder@uib.de>)

## [4.9.3.2-1] - 2009-10-26

### Added

- new String function getUserSID (Rupert Roeder <r.roeder@uib.de>)
- new locations for logfile (Rupert Roeder <r.roeder@uib.de>)

### Changed

- fatal error handling also for if clauses (Rupert Roeder <r.roeder@uib.de>)
- command line option usercontext (Rupert Roeder <r.roeder@uib.de>)
- patch text file commands set_mozilla_pref, addstringlistelement_to_mozilla_pref (Rupert Roeder <r.roeder@uib.de>)

### Fixed

- bugfix for trace mode (Rupert Roeder <r.roeder@uib.de>)

## [4.9.3.1-3] - 2009-10-26

### Fixed

- bugfix for fatal error (Rupert Roeder <r.roeder@uib.de>)

## [4.9.3.1-2] - 2009-10-26

### Changed

- on fatal error, winst stops execution also with if (Rupert Roeder <r.roeder@uib.de>)
- no more trace mode when log file cannot be written, instead message
  box (Rupert Roeder <r.roeder@uib.de>)
- introduction of usercontext in command line and batch mode, new
  String function getUserSid (Rupert Roeder <r.roeder@uib.de>)

## [4.9.3.0-2] - 2009-10-26

### Changed

- setacl.exe removed bardo wolf (Rupert Roeder <r.roeder@uib.de>)

## [4.9.3] - 2009-10-26

### Changed

- for temporary batch files the logging directory is used (Rupert Roeder <r.roeder@uib.de>)

## [4.9.2] - 2009-10-26

### Added

- new command in PatchTextFile section: set_mozilla_pref (Rupert Roeder <r.roeder@uib.de>)

### Changed

- ensuring that opsi-winst does not set itself to update on reinstallation (Rupert Roeder <r.roeder@uib.de>)

## [4.9.1.1] - 2009-10-08

### Changed

- user account name on top of winst log (Rupert Roeder <r.roeder@uib.de>)

## [4.9.1.0] - 2009-09-17

### Changed

- trying to reconnect when a connection error 10054 occured (Rupert Roeder <r.roeder@uib.de>)

## [4.9.0.0] - 2009-09-16

### Added

- new functions GetProductProperty, GetSectionLines (Rupert Roeder <r.roeder@uib.de>)

## [4.8.8.1-2] - 2009-07-02

### Changed

- bg.png template handling in preinst/postinst (Detlef Oertel <d.oertel@uib.de>)

## [4.8.8.1] - 2009-07-02

### Fixed

- bugfix in LDAPsearch result (Rupert Roeder <r.roeder@uib.de>)

## [4.8.8.0] - 2009-06-30

### Added

- new command line option /silent (Rupert Roeder <r.roeder@uib.de>)

### Changed

- repaired idapi section (Rupert Roeder <r.roeder@uib.de>)

## [4.8.7.2] - 2009-06-22

### Fixed

- bugfix in PatchTextFile, set_netscape_user_pref (Rupert Roeder <r.roeder@uib.de>)

## [4.8.7.1] - 2009-06-19

### Added

- new section type ldapsearch (Rupert Roeder <r.roeder@uib.de>)

## [4.8.7] - 2009-06-17

### Changed

- adaption of sendlog to python 2.6 (Rupert Roeder <r.roeder@uib.de>)

## [4.8.6-5] - 2009-05-22

### Changed

- adaption to changed service methods, bugfixes (Rupert Roeder <r.roeder@uib.de>)

## [4.8.6-2] - 2009-05-22

### Changed

- adaption to changed service methods (Rupert Roeder <r.roeder@uib.de>)

## [4.8.6-1] - 2009-05-08

### Added

- new functions getLastServiceErrorClass, getLastServiceErrorMessage,
  opsiLicenseManagementEnabled (Rupert Roeder <r.roeder@uib.de>)

## [4.8.5-2] - 2009-05-08

### Fixed

- bugfix (Rupert Roeder <r.roeder@uib.de>)

## [4.8.5-1] - 2009-05-06

### Added

- added support for special version of server based sorting of products (Rupert Roeder <r.roeder@uib.de>)

## [4.8.4-2] - 2009-04-03

### Fixed

- Fixed winstskin copying in postinst (Jan Schneider <j.schneider@uib.de>)

## [4.8.4-1] - 2009-03-19

### Added

- new option -V for winst copy actions: version control only with regard (Rupert Roeder <r.roeder@uib.de>)

### Changed

- to target directory (not for windows system directory) (Rupert Roeder <r.roeder@uib.de>)

## [4.8.3-1] - 2009-03-16

### Fixed

- bugfix: winst crashed when connection to service failed (Rupert Roeder <r.roeder@uib.de>)

## [4.8.2-1] - 2009-03-11

### Changed

- special option for products sorting (Rupert Roeder <r.roeder@uib.de>)

## [4.8.1-1] - 2009-02-20

### Added

- new functions for licence management (Rupert Roeder <r.roeder@uib.de>)

## [4.8.0.1-2] - 2009-01-28

### Changed

- changed postinst chmod o+r  $DST_DIR/winst32.exe ... for domainlogins (Rupert Roeder <r.roeder@uib.de>)

## [4.8.0.1-1] - 2009-01-27

### Changed

- the symbol ^ in the command line parameter service_options is replaced by " (Rupert Roeder <r.roeder@uib.de>)

## [4.8.0-1] - 2009-01-26

### Changed

- supports parameter service_options (Rupert Roeder <r.roeder@uib.de>)

## [4.7.4-6] - 2009-01-20

### Changed

- libeay32.dll version 0.9.8.8 (0.9.8 h) (Detlef Oertel <d.oertel@uib.de>)
- ssleay32.dll version 0.9.8.8 (0.9.8 h) (Detlef Oertel <d.oertel@uib.de>)
