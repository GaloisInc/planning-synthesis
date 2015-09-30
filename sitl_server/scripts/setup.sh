#!/bin/sh

set -e

# Activate the virtual env
. "$BUILD_DIR/env/bin/activate"

pip install pip==7.1.0
pip install pymavlink==1.1.57 MAVProxy==1.4.24

pip install -e ./sitl-proxy
