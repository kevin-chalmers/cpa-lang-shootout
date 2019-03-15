#!/bin/bash

cd src
for dir in */ ; do
    if [-e "$dir/Dockerfile"] then
        echo "Building ${dir%*/}"
        docker build -t ${dir%*/} $dir
    fi
done
