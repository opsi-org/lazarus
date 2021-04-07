#!/bin/bash
set -x
# Do not forget to export the passord
#export UIB_BINARYINDEX_PASSWORD="********"
opsi-dev-tool  -l info --binary-push opsi-script
opsi-dev-tool  -l info --binary-push opsi-script-gui
