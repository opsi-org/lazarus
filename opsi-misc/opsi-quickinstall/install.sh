#!/bin/bash 

#set -x

echo .
echo   Aktueller PC: $HOSTNAME
echo   You are at PC: $HOSTNAME
echo .
echo   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
echo   !!                                                                    !!
echo   !!           Zum Starten der opsi-server Installation           !!
echo   !!             druecken Sie bitte eine beliebige Taste                !!
echo   !!        Zum Abbrechen schliessen Sie einfach dieses Fenster         !!
echo   !!                                                                    !!
echo   !!          To start the installation of the opsi-server        !!
echo   !!                        just press any key                          !!
echo   !!                 To cancel just close this window                   !!
echo   !!                                                                    !!
echo   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
echo .
read

# check if we running as root
MYSUDO=''
if (( $EUID != 0 )); then
	MYSUDO='sudo'
	echo not running as root - using sudo
else
	echo running as root -ok
fi
MYDIR=`pwd`
# check X running
if [ "$DISPLAY" ]; then
	echo running on X -ok
	# check architecture
	uname -a | grep x86_64 > /dev/null
	if [ $? -eq 0 ] ; then
		echo "64"
		chmod u+x "${MYDIR}/opsi-quickinstall"
		export DISPLAY=:0
		$MYSUDO "${MYDIR}/opsi-quickinstall""${MYDIR}/opsi-quickinstall"
	else
		echo "32 Bit not supported any more"
	fi
else
	echo running without X -ok
	# check architecture
	uname -a | grep x86_64 > /dev/null
	if [ $? -eq 0 ] ; then
		echo "64 Bit nogui"
		chmod u+x "${MYDIR}/opsi-quickinstall-nogui"
		$MYSUDO "${MYDIR}/opsi-quickinstall-nogui" 
	else
		echo "32 Bit not supported any more"
	fi
fi


echo .
echo   Installation abgeschlossen
echo   Installation completed
echo .
read



sudo opsi-script-nogui l-opsi-server.opsiscript -batch -logfile l-opsi-server.log
