# Opsi-QuickInstall

This is a description of the functionality of Opsi-QuickInstall for any developer who wants to or must work with the Opsi-QuickInstall source code.

## Forms of GUI-Version

First I want to list all forms belonging to Opsi-QuickInstall (and their units in brackets) in their order of appearance:

+ TQuickInstall (opsi_quick_install_unit_language)
+ TDistribution (opsi_quick_install_unit_distr)
+ TQuery (opsi_quick_install_unit_query): Only shown in custom setup
+ TQuery2 (opsi_quick_install_unit_query2): Only shown in custom setup
+ TQuery4 (opsi_quick_install_unit_query4)
+ TQuery5_dhcp (opsi_quick_install_unit_query5_dhcp): Only shown if the user wants to install a dhcp-server on the opsi-server (asked in TQuery4).
+ TQuery6 (opsi_quick_install_unit_query6)
+ TOverview (opsi_quick_install_unit_overview)
+ TPassword (opsi_quick_install_unit_password)
+ TWait (opsi_quick_install_unit_wait)

TQuery3 had to disappear in the process of development.

Some notes on TQuickInstall:

+ TQuickInstall reads in the distribution of the system it's executed on automatically. That's the reason why the GUI-Version takes about 2 seconds from execution to showing the first form TQuickInstall.

+ On TQuickInstall, there is the possibility for the user to select the language. If you want to add a new language, be sure to determine the width of BtnNext manually once for this language and hardcode the result in the procedure SetBtnWidth. This ensures that the button width changes correctly on language change because that unfortunately doesn't work automatically while the form is active. This is also the reason for the two invisible buttons BtnOverview and BtnFinish on TQuickInstall. We determine their widths here on TQuickInstall to use them later on TQuery6, TOverview and TPassword.

+ The invisible BtnBack defines with it's properties 'Left' and 'Top' the position of all BtnBacks and BtnNexts on all forms except TDistribution, TPassword and of course TWait.


## After the queries

What happens after the data for the l-opsi-server installation is collected from the user in the queries of Opsi-QuickInstall (GUI same as No-GUI-Version)?

1. Opsi-QuickInstall saves the data in /opsi-quickinstall/l-opsi-server/CLIENT_DATA/properties.conf from where the l-opsi-server script reads it.

2. Opsi-QuickInstall adds a repository to /etc/apt/sources.list.d/opsi.list and installs opsi-script from there (opsi-script is required for executing the l-opsi-server script in /opsi-quickinstall/l-opsi-server/CLIENT_DATA/).

3. Opsi-QuickInstall starts the l-opsi-server installation. When the script of the installation is finished, it writes its result ('success' or 'failed') in the file /opsi-quickinstall/l-opsi-server/CLIENT_DATA/result.conf . From there, Opsi-QuickInstall reads the result and displays it to the user.

4. The log file of Opsi-QuickInstall can usually be found in /tmp/opsi_quickinstall.log (GUI-Version) or /tmp/opsi_quickinstall_nogui.log (No-GUI-Version). The log file of the l-opsi-server installation is located in /var/log/opsi-quick-install-l-opsi-server.log .


## How to add a new distribution

If opsi supports a new linux distribution and you want to add it to QuickIntsall, follow these steps:

1. Check that the distribution is available on http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/stable/ .

2. In osLinuxRepository:
	+ Add the distriution to TDistribution with exactly the same name that the folder on download.opensuse.org has.
	+ Add the distriution in the function TLinuxRepository.GetDefaultURL .
	+ Add the distriution in the procedure TLinuxRepository.Add .

3. In osDistributionInfo:
	+ Add the distriution to the const string Distribs of TDistributionInfo.
	+ Add the distriution to the procedure TDistributionInfo.SetInfo .


