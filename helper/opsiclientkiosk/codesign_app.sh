#!/bin/sh
set -e

# Set these values:
CODE_SIGN_SIGNATURE="Developer ID Application: uib gmbh (5H88T32F7P)"
APPLE_ID_USER=macos@uib.de
APP_SPECIFIC_PASSWORD=

BUNDLE_ID=org.opsi.opsi-client-kiosk
EXECUTABLE_NAME=opsiclientkiosk
EXECUTABLE_SOURCE=`pwd`builds/x86_64-darwin/${EXECUTABLE_NAME}
EXECUTABLE_DIR=`pwd`/${EXECUTABLE_NAME}.dir
FULLPATHTOEXE=${EXECUTABLE_DIR}/${EXECUTABLE_NAME}
ENTITLEMENTS="--entitlements kiosk.entitlements"

# Establish a work directory, create a disk image root directory within 
# that, and then copy the app there.
#
# Note we use `-R`, not `-r`, to preserve symlinks.
WORKDIR="QShare-`date '+%Y-%m-%d_%H.%M.%S'`"
DMGROOT="${WORKDIR}/QShare"
APP="${WORKDIR}/QShare/QShare.app"
DMG="${WORKDIR}/QShare.dmg"
mkdir -p "${DMGROOT}"
cp -R "${ARCHIVE}/Products/Applications/QShare.app" "${DMGROOT}/"
# When you use `-f` to replace a signature, `codesign` prints `replacing 
# existing signature`.  There's no option to suppress that.  The message 
# goes to `stderr` so you don't want to redirect it to `/dev/null` because 
# there might be other interesting stuff logged to `stderr`.  One way to 
# prevent it is to remove the signature beforehand, as shown by the 
# following lines.  It does slow things down a bunch though, so I've made 
# it easy to disable them.
if true
then
    codesign --remove-signature "${APP}"
    codesign --remove-signature "${APP}/Contents/PlugIns/QShareExtension.appex"
    codesign --remove-signature "${APP}/Contents/Frameworks/QCore.framework"
    codesign --remove-signature "${APP}/Contents/Frameworks/QCore.framework/Versions/A/Helpers/QCoreTool"
fi
# Create various entitlement files from 'here' documents.
cat > "${WORKDIR}/app.entitlements" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>com.apple.security.app-sandbox</key>
    <true/>
    <key>com.apple.security.network.client</key>
    <true/>
    <key>com.apple.security.network.server</key>
    <true/>
</dict>
</plist>
EOF
cat > "${WORKDIR}/appex.entitlements" <<EOF
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
cat > "${WORKDIR}/tool.entitlements" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>com.apple.security.app-sandbox</key>
    <true/>
    <key>com.apple.security.inherit</key>
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
codesign -s "Developer ID Application" -f --timestamp -i com.example.apple-samplecode.QShare.QCoreTool -o runtime --entitlements "${WORKDIR}/tool.entitlements"  "${APP}/Contents/Frameworks/QCore.framework/Versions/A/Helpers/QCoreTool"
codesign -s "Developer ID Application" -f --timestamp                                                                                                            "${APP}/Contents/Frameworks/QCore.framework"
codesign -s "Developer ID Application" -f --timestamp                                                  -o runtime --entitlements "${WORKDIR}/appex.entitlements" "${APP}/Contents/PlugIns/QShareExtension.appex"
codesign -s "Developer ID Application" -f --timestamp                                                  -o runtime --entitlements "${WORKDIR}/app.entitlements"   "${APP}"
# Create a disk image from our disk image root directory.
hdiutil create -srcFolder "${DMGROOT}" -quiet -o "${DMG}"
# Sign that.
codesign -s "Developer ID Application" --timestamp -i com.example.apple-samplecode.QShare.DiskImage "${DMG}"
echo "${DMG}"