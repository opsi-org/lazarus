#!/bin/bash
set -e -x

# Set these values:
CODE_SIGN_SIGNATURE="Developer ID Application: uib gmbh (5H88T32F7P)"
APPLE_ID_USER=macos@uib.de
# Login to https://appleid.apple.com 
# and find the Security > App-Specific Password > Generate Password section. 
# Copy this password, which will be in the form xxxx-xxxx-xxxx-xxxx.
APP_SPECIFIC_PASSWORD=toiv-wkgo-lrwb-zbtq

BUNDLE_ID=org.opsi.opsiclientdeventstarter
EXECUTABLE_NAME=opsiclientd_event_starter
EXECUTABLE_DIR=`pwd`/${EXECUTABLE_NAME}.dir
FULLPATHTOEXE=${EXECUTABLE_DIR}/${EXECUTABLE_NAME}

echo signature "$CODE_SIGN_SIGNATURE" 
echo passwd $APP_SPECIFIC_PASSWORD
echo id $APPLE_ID_USER
echo dir $EXECUTABLE_DIR


#compile
#echo "Building"
#fpc -k"-sectcreate __TEXT __info_plist Info.plist" hello.pas

echo "Clean up temporary files ..."
rm -f ${EXECUTABLE_NAME}_macOS.dmg
# rm -f ${EXECUTABLE_NAME}_macOS.tmp.dmg
rm -f upload_log_file.txt
rm -f request_log_file.txt
rm -f log_file.txt
rm -f ${EXECUTABLE_DIR}/*
cp $EXECUTABLE_NAME ${EXECUTABLE_DIR}


# Verify the Info.plist was embedded in the executable during linking
echo "Verifying Info.plist"
launchctl plist $FULLPATHTOEXE

# Codesign the executable by enabling the hardened runtime (--options=runtime) and include a timestamp (--timestamp)
echo "Code signing..."
codesign -vvv --force --strict --options=runtime --entitlements opsi.entitlements --timestamp -s "$CODE_SIGN_SIGNATURE" $FULLPATHTOEXE
codesign --verify --verbose --strict $FULLPATHTOEXE
codesign -dv -r- $FULLPATHTOEXE
codesign -vvv --deep --strict $FULLPATHTOEXE

# We need to distrubute the executable in a disk image because the stapler only works with directories
echo "Creating disk image..."
#cp $EXECUTABLE_NAME ${EXECUTABLE_DIR}
#codesign -dv -r- ${EXECUTABLE_DIR}/${EXECUTABLE_NAME}
# hdiutil create -volname $EXECUTABLE_NAME -srcfolder `pwd` -ov -format UDZO -layout SPUD -fs HFS+J  ${EXECUTABLE_NAME}_macOS.tmp.dmg
# hdiutil convert ${EXECUTABLE_NAME}_macOS.tmp.dmg -format UDZO -o ${EXECUTABLE_NAME}_macOS.dmg
hdiutil create -volname $EXECUTABLE_NAME -srcfolder $EXECUTABLE_DIR -ov -format UDZO -layout SPUD -fs HFS+J  ${EXECUTABLE_NAME}_macOS.dmg
#/usr/bin/ditto -c -k --keepParent "$EXECUTABLE_NAME" ${EXECUTABLE_NAME}.zip

codesign -s "$CODE_SIGN_SIGNATURE" ${EXECUTABLE_NAME}_macOS.dmg
#codesign -s "$CODE_SIGN_SIGNATURE" ${EXECUTABLE_NAME}.zip
# Notarizing with Apple...
echo "Uploading..."
xcrun altool --notarize-app -t osx --file ${EXECUTABLE_NAME}_macOS.dmg --primary-bundle-id $BUNDLE_ID -u $APPLE_ID_USER -p $APP_SPECIFIC_PASSWORD --output-format xml > upload_log_file.txt
#xcrun altool --notarize-app -t osx --file ${EXECUTABLE_NAME}.zip --primary-bundle-id $BUNDLE_ID -u $APPLE_ID_USER -p $APP_SPECIFIC_PASSWORD --output-format xml > upload_log_file.txt

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
#xcrun stapler staple ${EXECUTABLE_NAME}.zip
#xcrun stapler validate ${EXECUTABLE_NAME}.zip
# open the log file so we can see if there are any warnings or other issues
open log_file.txt
