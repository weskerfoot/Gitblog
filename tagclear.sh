#! /usr/bin/bash

name="$(echo "$(realpath --relative-to=/home/wes/myblog ${1})" | sed "s/[\:\.\/\\ \n]/_/g")_tags"
unset "$name"
