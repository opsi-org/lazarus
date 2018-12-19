echo on
::rem **************************************************
::rem extractMSI.cmd: 
::rem extract MSI file from "InstallShield+MSI.exe"
::rem **************************************************

::rem **************************************************
::rem command line parameter:
::rem %1=setup.exe (including path)
::rem **************************************************

::rem **************************************************
::rem LanguageCode de-de=1031
::rem set LC=1031 
::rem LanguageCode en-us=1033
::rem SET LC=1033
::rem **************************************************

::rem **************************************************
set ORGTMP=%TMP%
set OPSITMP=%TMP%\opsitmp
set OPSITMPTMP=%TMP%\opsitmp\tmp
::rem **************************************************
echo extractMSI.cmd setup.exe 
echo example:
echo extractMSI.cmd "C:\setup.exe"
echo extracts MSI and MST files from setup.exe and
echo copies them to %OPSITMP%

::rem **************************************************
set SETUP=%~1
if "%SETUP%"=="" goto opsiend
::rem **************************************************

::rem **************************************************
::rem delete old OPSITMP with all files and subdirs
del /S /Q "%OPSITMP%\*"
for /d %%i in ("%OPSITMP%\*.*") do rd /s /q "%%i"
rd /s /q "%OPSITMP%"
::rem **************************************************

::rem **************************************************
::rem create new OPSITMP
md "%OPSITMP%"
md "%OPSITMPTMP%"
set TEMP=%OPSITMPTMP%
set TMP=%TEMP%
::rem **************************************************

::rem **************************************************
::rem copy setup.exe (because name might have blanks)
::rem **************************************************
copy "%SETUP%" "%OPSITMP%\testsetup.exe"
::rem **************************************************

::rem **************************************************
::rem start setup to unpack MSI
start /d "%OPSITMP%" testsetup.exe
::rem **************************************************

if "%ERRORLEVEL%"=="0" goto waitforstart
goto opsiend


:waitforstart
::rem wait 3 seconds for MSI to start...
echo "wait 3 seconds for setup to start ..."
ping 1.1.1.1 -n 1 -w 3000 >NUL
tasklist /FI "IMAGENAME eq msiexec.exe" 2>NUL | find /I /N "msiexec.exe">NUL
if "%ERRORLEVEL%"=="0" goto msiexecrunning

::rem wait another 3 seconds for MSI to start...
echo "wait another 3 seconds for setup to start ..."
ping 1.1.1.1 -n 1 -w 3000 >NUL
tasklist /FI "IMAGENAME eq msiexec.exe" 2>NUL | find /I /N "msiexec.exe">NUL
if "%ERRORLEVEL%"=="0" goto msiexecrunning

::rem wait another 3 seconds for MSI to start...
echo "wait another 3 seconds for setup to start ..."
ping 1.1.1.1 -n 1 -w 3000 >NUL
tasklist /FI "IMAGENAME eq msiexec.exe" 2>NUL | find /I /N "msiexec.exe">NUL
if "%ERRORLEVEL%"=="0" goto msiexecrunning

::rem wait another 3 seconds for MSI to start...
echo "wait another 3 seconds for setup to start ..."
ping 1.1.1.1 -n 1 -w 3000 >NUL
tasklist /FI "IMAGENAME eq msiexec.exe" 2>NUL | find /I /N "msiexec.exe">NUL
if "%ERRORLEVEL%"=="0" goto msiexecrunning

::rem timeout!!!
echo "Timeout starting setup"
goto opsiend

:msiexecrunning

echo "wait another second to get MSI ..."
ping 1.1.1.1 -n 1 -w 1000 >NUL

::rem **************************************************
::rem copy MSI and MST files
for /R "%OPSITMPTMP%" %%f in (*.msi) DO COPY "%%f" "%OPSITMP%"
for /R "%OPSITMPTMP%" %%f in (*.mst) DO COPY "%%f" "%OPSITMP%"
ping 1.1.1.1 -n 1 -w 500 >NUL
taskkill /F /IM msiexec.exe /T 
ping 1.1.1.1 -n 1 -w 500 >NUL
del "%OPSITMP%\testsetup.exe"
::rem **************************************************

::rem **************************************************
::rem reset TEMP and TMP
set TEMP=%ORGTMP%
set TMP=%ORGTMP%
::rem **************************************************

echo "files have been copied to %OPSITMP%"
dir %OPSITMP%

@echo off
:opsiend
echo "expected setup.exe as parameter"
rem **************************************************
rem **************************************************
