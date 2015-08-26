#! /usr/bin/bash

# add a tag to the environment
name="$(echo "$(realpath --relative-to=/home/wes/myblog ${1})" | sed "s/[\:\.\/\\ \n]/_/g")_tags"
export "$name"="${!name}:#${2}"
