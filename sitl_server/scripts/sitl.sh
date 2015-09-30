#!/bin/sh

set -ex

if [ $# -lt 3 ]; then
  echo 'Expected at least 3 arguments:'
  echo '  ./scripts/sitl.sh <build-dir> <ardupilot-path> <sitl-instance> \\'
  echo '      <sitl-name>'
  exit 1
fi

build_path=$(realpath $1)
ardupilot_path=$(realpath $2)
ident=$3
name=$4

if [ "$ident" -eq 0 ]; then
  extra="--map "
else
  extra=""
fi

# setup the python environment
. "$build_path/env/bin/activate"

# add the simulator to the path
export PATH="$ardupilot_path/Tools/autotest:$PATH"

if [ ! -d "$build_path/sitl/$ident" ]; then
  # change to the directory for this instance of the sitl
  mkdir -p "$build_path/sitl/$ident"

  # make sure that parameters get wiped this first time
  wipe="-w"
fi

cd "$build_path/sitl/$ident"
cmd="sitl_connect $ident"

# start the sitl
export SITL_INST_ID=$ident
export SITL_PID=$$
exec sim_vehicle.sh $wipe -vArduCopter -N -f+ "-I$ident" "-L$name" "-t$name" \
	$extra --load-module sitl_proxy.proxy --cmd=sitl_connect
