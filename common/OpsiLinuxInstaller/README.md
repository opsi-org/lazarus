# OpsiLinuxInstaller

This folder contains parent forms and units and a few helper units for installer programms that install an opsi package 'manually' (i.e. without an opsi server).

The core concept for the installer programms is:
1. Ask for properties of an opsi package,
2. Download opsi-script,
3. Execute the installation script of the opsi package.

The translations for the resourcestrings of the forms and units are in the `locale` folder. The translations are performed in the code of the respective unit by calling `Language.TranslateResourceStrings(...)` using the `OpsiLinuxInstaller_LanguageObject`.

The OpsiLinuxInstaller is currently only used by opsi-QuickInstall. (There was a time when we thought about an opsi-configed installer which used the OpsiLinuxInstaller but this project is redundant and removed since there are opsi-configed portables now.)