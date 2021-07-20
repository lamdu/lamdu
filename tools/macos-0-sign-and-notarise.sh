# Intended to be used after make_archive.hs
# Based on http://www.zarkonnen.com/signing_notarizing_catalina
#
# Usage: sign-and-notarize <SIGNING-ID> <APPLE-ID> <SIGNING-APP-PASSWORD>

set -e

echo Signing bundled nodejs
codesign -s "$1" --timestamp --options runtime -f --entitlements tools/data/entitlements.plist --deep Lamdu.app/Contents/Resources/bin/node.exe

echo Signing
codesign -s "$1" --timestamp --options runtime -f --entitlements tools/data/entitlements.plist --deep Lamdu.app

echo Zipping
ditto -c -k --keepParent Lamdu.app lamdu-notarize.zip

echo Notarising
xcrun altool --notarize-app --primary-bundle-id "org.lamdu.www" -u $2 -p $3 --file lamdu-notarize.zip

# After this step one should wait for the notarization success (or failure) email
