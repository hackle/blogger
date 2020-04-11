#!/bin/bash

mkdir -p build/src
stack clean --docker
stack build --docker
cp $(stack --docker path --local-install-root)/bin/bootstrap build
cp -rf ./src/raw ./build/src
cd build && zip -r function.zip ./ && rm -rf src && rm bootstrap && cd ..

stage="$1"
if [ -z "$stage" ]
then
    stage="dev"
fi

serverless deploy function -f haskell --stage "$stage"