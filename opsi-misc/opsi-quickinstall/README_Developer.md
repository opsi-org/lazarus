# Opsi-QuickInstall Gui

This is a description of the functionality of Opsi-QuickInstall for any developer who wants to or must work with the Opsi-QuickInstall source code.

## Forms

First I want to list all forms belonging to Opsi-QuickInstall (and their units in brackets) in their order of appearance:

+ TQuickInstall (opsi_quick_install_unit_language)
+ TDistribution (opsi_quick_install_unit_distr)
+ TQuery (opsi_quick_install_unit_query): Only shown in custom setup
+ TQuery2 (opsi_quick_install_unit_query2): Only shown in custom setup
+ TQuery4 (opsi_quick_install_unit_query4)
+ TQuery5_dhcp (opsi_quick_install_unit_query5_dhcp): Only shown if the user wants to install a dhcp-server on the opsi-server (asked in Query4).
+ TQuery6 (opsi_quick_install_unit_query6)
+ TOverview (opsi_quick_install_unit_overview)
+ TPassword (opsi_quick_install_unit_password)
+ TWait (opsi_quick_install_unit_wait)

Query3 had to disappear in the process of development.

Some notes on TQuickInstall:

1. TQuickInstall reads in the distribution of the system it's executed on automatically. That's the reason why the GUI-Version takes about 2 seconds from execution to showing the first form TQuickInstall.

2. On TQuickInstall, there is the possibility for the user to select the language. If you want to add a new language, be sure to determine the width of BtnNext manually once for this language and hardcode the result in the procedure SetBtnWidth. This ensures that the button width changes correctly on language change because that unfortunately doesn't work automatically while the form is active. This is also the reason for the two invisible buttons BtnOverview and BtnFinish on TQuickInstall. We determine their widths here on TQuickInstall to use them later on TQuery6, TOverview and TPassword.

3. The invisible BtnBack defines with it's properties 'Left' and 'Top' the position of all BtnBacks and BtnNexts on all forms except TDistribution, TPassword and of course TWait.

...
