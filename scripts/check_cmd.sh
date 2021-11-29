#!/usr/bin/env bash

function check_cmd {
    if [ $# -ne 2 ]; then
        echo "Usage: check_cmd cmd_name pkg_name"
        exit
    fi

    cmd_name=$1
    pkg_name=$2

    if ! command -v $cmd_name > /dev/null; then
        info="Need to install $pkg_name"
        notify-send "$info"
        echo "$info"
        exit
    fi
}
