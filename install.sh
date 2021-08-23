#!/bin/bash

cabal install --overwrite-policy=always
cd frontend
yarn build
mkdir -p $HOME/.ticket-manager
cp -R build/* $HOME/.ticket-manager
