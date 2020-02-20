# Notarizing Command Line Tools 

MacOS 10.15 expects command line tools should be [notarized](https://mjtsai.com/blog/2019/06/17/notarizing-command-line-tools-for-macos-10-15/). Apple provides some [documentation](https://developer.apple.com/documentation/security/notarizing_your_app_before_distribution), but most of this assumes projects managed using Xcode. In contrast, many command line tools are built directly from the command line. 

Assuming you have a working Terminal application you will need to complete the following steps:
 1. You will need an Apple developer account.
 2. Generate an app specific password. **You will only do this once.** 
 3. Run the notarize.bash script. **The first time you do this want to edit the Info.plist with your personal CFBundleName, CFBundleExecutable and CFBundleIdentifier values. You will also need to personalize the bash script with your user name and app specific password.** The script will execute the following steps:
  - Compile your executable and optionally append a Info.plist section to your executable. 
  - Generate a disk image.
  - [Signing disk image](https://developer.apple.com/library/archive/technotes/tn2206/_index.html#//apple_ref/doc/uid/DTS40007919-CH1-TNTAG18).
  - Upload your disk image to Apple. Wait for a response (which can take a few minutes).
  - Assuming success, staple your ticket to your disk image.
 
 The includes project demonstrates this process, with the notarize.bash script automating this.

## Generate App Specific Password 

Login to https://appleid.apple.com and find the `Security > App-Specific Password > Generate Password` section. Copy this password, which will be in the form `xxxx-xxxx-xxxx-xxxx`.

## Appending a Info.plist

An Info.plist is [not required](https://eclecticlight.co/2019/06/21/notarization-made-a-bit-simpler/), but it is probably not a bad idea.

Here we consider a very simple command line tool that we would traditionally compile with the command

```
fpc hello.pas
```

Apple notarization requires an XML file named 'Info.plist' For graphical applications distributed as a .app bundle, this file is distributed as a stand-alone file. However, command line tools do not have app bundles, so we need to insert this XML file directly into our executable. Consider a minimal Info.plist (individual projects will need to modify the CFBundleName, CFBundleExecutable and CFBundleIdentifier):

```
 <?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleExecutable</key>
	<string>hello</string>
	<key>CFBundleIdentifier</key>
	<string>com.mycompany.hello</string>
	<key>CFBundleInfoDictionaryVersion</key>
	<string>6.0</string>
	<key>CFBundleName</key>
	<string>hello</string>
	<key>CFBundleShortVersionString</key>
	<string>1.0</string>
	<key>CFBundleVersion</key>
	<string>1</string>
	<key>CFBundleSupportedPlatforms</key>
	<array>
		<string>MacOSX</string>
	</array>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
</dict>
</plist>
```

We can inject this into our application by telling the linker to add this as a section:

```
fpc -k"-sectcreate __TEXT __info_plist Info.plist" hello.pas
```  

## Edit notarize.bash

You will need to insert your personal details in this file.
``` 
    CODE_SIGN_SIGNATURE="Developer ID Application"
    APPLE_ID_USER=email@domain.com
    APP_SPECIFIC_PASSWORD=xxxx-xxxx-xxxx-xxxx
    EXECUTABLE_NAME=hello
    BUNDLE_ID=com.mycompany.hello
``` 

## Run

`cd` to the directory containing notarize.bash and run using `sh notarize.bash`.  The output will be placed in `hello/build/bin`. If the process was successful you will have the following files:

- **hello** The executable built with cmake.
- **hello_macOS.dmg** The final disk image to distribute which is notarized and stapled.
- **upload_log_file.txt** The XML property list log from `xcrun altool --notarize-app`. If the upload was successful you can use the `notarization-upload > RequestUUID` key to query Apple's server to check on the status of the upload.
- **response_log_file.txt** The XML property list log from `xcrun altool --notarization-info `. After the upload is complete the script will query the server and dump the results into the text file.
- **log_file.txt** After the final response is given from the server a log file URL will be set in `notarization-info > LogFileURL` of response_log_file.txt and downloaded. If there are any errors, warnings or other issues you can view in the log file.

## References

- [Notarizing Your App Before Distribution](https://developer.apple.com/documentation/security/notarizing_your_app_before_distribution?language=objc)
 - [Common Notarization Problems](https://developer.apple.com/documentation/security/notarizing_your_app_before_distribution/resolving_common_notarization_issues?language=objc)
 - [Oakley's guide](https://eclecticlight.co/2019/06/13/building-and-delivering-command-tools-for-catalina/) and [update](https://eclecticlight.co/2019/06/21/notarization-made-a-bit-simpler/). 


