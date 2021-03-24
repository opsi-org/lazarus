#!/bin/bash
set -e -x

# Set these values:
CODE_SIGN_SIGNATURE="Developer ID Application: uib gmbh (5H88T32F7P)"
APPLE_ID_USER=macos@uib.de

#APP_SPECIFIC_PASSWORD=fqwo-ztjg-ljte-xkpl
#BUNDLE_ID=org.opsi.opsi-script-nogui
#EXECUTABLE_NAME=opsi-script-nogui

APP_SPECIFIC_PASSWORD=bues-hpoq-pors-iymh
BUNDLE_ID=org.opsi.opsi-script-gui
EXECUTABLE_NAME=opsi-script-gui
EXECUTABLE_DIR=`pwd`/${EXECUTABLE_NAME}.app
FULLPATHTOEXE=${EXECUTABLE_DIR}/Contents/MacOS/${EXECUTABLE_NAME}
ENTITLEMENTS="--entitlements opsi-script.entitlements"




echo signature "$CODE_SIGN_SIGNATURE" 
echo passwd $APP_SPECIFIC_PASSWORD
echo id $APPLE_ID_USER
echo dir $EXECUTABLE_DIR

echo "Clean up temporary files ..."
rm -f ${EXECUTABLE_NAME}_macOS.dmg
rm -f upload_log_file.txt
rm -f request_log_file.txt
rm -f log_file.txt
mkdir -p ${EXECUTABLE_DIR}/Contents/MacOS
rm -f ${EXECUTABLE_DIR}/Contents/MacOS/*
#cp $EXECUTABLE_NAME ${EXECUTABLE_DIR}
cp $EXECUTABLE_NAME ${EXECUTABLE_DIR}/Contents/MacOS/
cp info-osgui.plist ${EXECUTABLE_DIR}/Contents/Info.plist

# Verify the Info.plist was embedded in the executable during linking
echo "Verifying Info.plist"
launchctl plist $FULLPATHTOEXE
launchctl plist $EXECUTABLE_DIR

# Codesign the executable by enabling the hardened runtime (--options=runtime) and include a timestamp (--timestamp)
echo "Code signing..."
#codesign -vvv --force --strict --options=runtime $ENTITLEMENTS --timestamp -s "$CODE_SIGN_SIGNATURE" $FULLPATHTOEXE
codesign -vvv --force --strict  --timestamp -s "$CODE_SIGN_SIGNATURE" $FULLPATHTOEXE
codesign --verify --verbose --strict $FULLPATHTOEXE
codesign -dv -r- $FULLPATHTOEXE
codesign -vvv --deep --strict $FULLPATHTOEXE

echo "Code signing .app..."
#codesign -vvv --force --strict --options=runtime $ENTITLEMENTS --timestamp -s "$CODE_SIGN_SIGNATURE" $EXECUTABLE_DIR
codesign -vvv --force --strict --timestamp -s "$CODE_SIGN_SIGNATURE" $EXECUTABLE_DIR

codesign --verify --verbose --strict $EXECUTABLE_DIR
codesign -dv -r- $EXECUTABLE_DIR
codesign -vvv --deep --strict $EXECUTABLE_DIR
codesign -d --entitlements :- $EXECUTABLE_DIR

### no notarize ####
exit $?
### no notarize ####

# We need to distrubute the executable in a disk image because the stapler only works with directories
echo "Creating disk image..."
hdiutil create -volname $EXECUTABLE_NAME -srcfolder $EXECUTABLE_DIR -ov -format UDZO -layout SPUD -fs HFS+J  ${EXECUTABLE_NAME}_macOS.dmg

codesign -s "$CODE_SIGN_SIGNATURE" ${EXECUTABLE_NAME}_macOS.dmg
codesign -vvv --deep --strict ${EXECUTABLE_NAME}_macOS.dmg

# Notarizing with Apple...
echo "Uploading..."
xcrun altool --notarize-app -t osx --file ${EXECUTABLE_NAME}_macOS.dmg --primary-bundle-id $BUNDLE_ID -u $APPLE_ID_USER -p $APP_SPECIFIC_PASSWORD --output-format xml > upload_log_file.txt

# WARNING: if there is a 'product-errors' key in upload_log_file.txt something went wrong
# TODO: parse out the error instead of exiting the script (remember set -e is enabled)
# /usr/libexec/PlistBuddy -c "Print :product-errors:0:message" upload_log_file.txt

# now we need to query apple's server to the status of notarization
# when the "xcrun altool --notarize-app" command is finished the output plist
# will contain a notarization-upload->RequestUUID key which we can use to check status
echo "Checking status..."
sleep 20
REQUEST_UUID=`/usr/libexec/PlistBuddy -c "Print :notarization-upload:RequestUUID" upload_log_file.txt`
while true; do
  xcrun altool --notarization-info $REQUEST_UUID -u $APPLE_ID_USER -p $APP_SPECIFIC_PASSWORD --output-format xml > request_log_file.txt
  # parse the request plist for the notarization-info->Status Code key which will
  # be set to "success" if the package was notarized
  STATUS=`/usr/libexec/PlistBuddy -c "Print :notarization-info:Status" request_log_file.txt`
  if [ "$STATUS" != "in progress" ]; then
    break
  fi
  # echo $STATUS
  echo "$STATUS"
  sleep 10
done

# download the log file to view any issues
/usr/bin/curl -o log_file.txt `/usr/libexec/PlistBuddy -c "Print :notarization-info:LogFileURL" request_log_file.txt`

# staple
echo "Stapling..."
xcrun stapler staple ${EXECUTABLE_NAME}_macOS.dmg
xcrun stapler validate ${EXECUTABLE_NAME}_macOS.dmg
# open the log file so we can see if there are any warnings or other issues
open log_file.txt
