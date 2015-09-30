#!/bin/bash

set -x -e

tmux -V 2>&1 > /dev/null
if [ $? -ne 0 ]; then
	echo '`tmux` is required to start the SITL server'
	exit 1
fi

server_dir="$(dirname $(realpath $0))/sitl_server"
echo $server_dir

detach=
if [ $1 == 'detach' ]; then
	detach="-d "
	shift 1
fi

if [ -z "$TMUX" ]; then
	cmd="new-session -A -s sitl-server"
else
	cmd="new-window"
fi


SERVER_ARGS=

while [[ $# > 0 ]]; do
	key="$1"

	case $key in
		-d|--drop)
			SERVER_ARGS="$SERVER_ARGS -d $2"
			shift 1
			;;

		*)
			SERVER_ARGS="$SERVER_ARGS $(realpath $1)"
			;;

	esac

	shift 1
done

exec tmux $cmd $detach "cd ${server_dir}; SERVER_ARGS=\"$SERVER_ARGS\" make run"
