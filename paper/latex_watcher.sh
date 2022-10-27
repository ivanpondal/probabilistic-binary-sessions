#!/bin/bash

trap exit_watcher SIGINT

exit_watcher() {
	exit
}

if [[ $OSTYPE == "darwin"* ]]; then
	WATCHER="fswatch -1l 5 ."
else
	WATCHER="inotifywait -e close_write -e delete -r ."
fi

while true; do
	$WATCHER
	make
done
