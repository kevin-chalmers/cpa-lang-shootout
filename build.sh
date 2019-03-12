#!/bin/bash

cd src
for dir in */ ; do
    docker build $dir
done
