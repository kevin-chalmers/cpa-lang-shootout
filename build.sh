#!/bin/bash

cd src
for dir in */ ; do
    docker build -t ${dir%*/} $dir
done
