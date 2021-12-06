#!/bin/sh
set -e

# Set these values:
DEVELOPER_ID="Developer ID Application: uib gmbh (5H88T32F7P)"
APPLE_ID_USER=macos@uib.de
APP_SPECIFIC_PASSWORD=

BUNDLE_ID=org.opsi.opsi-client-kiosk
EXECUTABLE_NAME=OpsiClientKiosk
EXECUTABLE_SOURCE="`pwd`/builds/x86_64-darwin/${EXECUTABLE_NAME}"
APP_SOURCE="`pwd`/builds/x86_64-darwin/${EXECUTABLE_NAME}.app"
#ENTITLEMENTS="--entitlements kiosk.entitlements"

# Establish a work directory, create a disk image root directory within 
# that, and then copy the app there.
#
# Note we use `-R`, not `-r`, to preserve symlinks.
WORKDIR="${EXECUTABLE_NAME}-`date '+%Y-%m-%d_%H.%M.%S'`"
DMGROOT="${WORKDIR}/${EXECUTABLE_NAME}"
APP="${WORKDIR}/${EXECUTABLE_NAME}/${EXECUTABLE_NAME}.app"
DMG="${WORKDIR}/${EXECUTABLE_NAME}.dmg"
mkdir -p "${DMGROOT}"
cp -R "${APP_SOURCE}" "${DMGROOT}/"
rm "${APP}/Contents/MacOS/${EXECUTABLE_NAME}"
cp -R "${EXECUTABLE_SOURCE}" "${APP}/Contents/MacOS/${EXECUTABLE_NAME}"
opsi-dev-tool --binary-pull development macos-ssl-libs darwin x64 latest "${DMGROOT}/"
#mkdir "${APP}/Contents/Frameworks"
mv "${DMGROOT}/macos-ssl-libs" "${APP}/Contents/Frameworks"
cp "`pwd`/opsiclientkiosk.conf" "${APP}/Contents/Resources"

# When you use `-f` to replace a signature, `codesign` prints `replacing 
# existing signature`.  There's no option to suppress that.  The message 
# goes to `stderr` so you don't want to redirect it to `/dev/null` because 
# there might be other interesting stuff logged to `stderr`.  One way to 
# prevent it is to remove the signature beforehand, as shown by the 
# following lines.  It does slow things down a bunch though, so I've made 
# it easy to disable them.

# disabled due to bug in codesigning tool set to true for enabling this
if false
then
    codesign --remove-signature "${APP}"
    codesign --remove-signature "${APP}/Contents/Frameworks/libssl.dylib"
    codesign --remove-signature "${APP}/Contents/Frameworks/libcrypto.dylib"
fi

# Create various entitlement files from 'here' documents.
cat > "${WORKDIR}/kiosk.entitlements" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>com.apple.security.app-sandbox</key>
    <true/>
    <key>com.apple.security.network.client</key>
    <true/>
</dict>
</plist>
EOF

# Sign the app from the inside out.
#
# Notes:
#
# * The tool is within the framework, so we have to sign that before the 
#   framework.
#
# * The app and the appex depend on the framework, so we have to sign the 
#   framework before of those.
# 
# * The appex is within the app, so we have to sign that before the app.
#
# * The tool is not bundled, thus doesn't have an `Info.plist`, and thus 
#   you have to explicitly set a code signing identifier.
# 
# * The tool, appex and app are all executables, and thus need to the 
#   hardened runtime flag.
# 
# * The tool, appex and app all need unique entitlements.
#codesign -s $DEVELOPER_ID -f --timestamp -i com.example.apple-samplecode.QShare.QCoreTool -o runtime --entitlements "${WORKDIR}/tool.entitlements"  "${APP}/Contents/Frameworks/QCore.framework/Versions/A/Helpers/QCoreTool"
#codesign -s $DEVELOPER_ID -f --timestamp -o runtime --entitlements "${WORKDIR}/appex.entitlements" "${APP}/Contents/PlugIns/QShareExtension.appex"

#codesign -s $DEVELOPER_ID -f --timestamp "${APP}/Contents/Frameworks/libssl.dylib"
#codesign -s $DEVELOPER_ID -f --timestamp "${APP}/Contents/Frameworks/libcrypto.dylib"
#codesign -s $DEVELOPER_ID -f --timestamp -o runtime --entitlements "${WORKDIR}/kiosk.entitlements" "${APP}"

## Create a disk image from our disk image root directory.
#hdiutil create -srcFolder "${DMGROOT}" -quiet -o "${DMG}"
## Sign that.
#codesign -s $DEVELOPER_ID --timestamp -i "${BUNDLE_ID}.DiskImage "${DMG}"
#echo "${DMG}"
