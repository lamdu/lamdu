# Intended to be used after macos-0-sign-and-notarise.sh
# Based on http://www.zarkonnen.com/signing_notarizing_catalina
#
# Usage: staple-and-zip <ZIP-FILENAME>

set -e

echo Stapling
xcrun stapler staple Lamdu.app

echo Re-zipping
ditto -c -k --keepParent Lamdu.app $1

echo Cleaning up
rm lamdu-notarize.zip
rm -r Lamdu.app
