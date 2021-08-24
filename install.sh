#!/bin/bash

cabal install --overwrite-policy=always
ticket-manager typescript > frontend/src/Api.ts
echo "Generated typescript types"
cd frontend
yarn build
mkdir -p $HOME/.ticket-manager
cp -R build/* $HOME/.ticket-manager
echo "Copied artifacts to $HOME/.ticket-manager"
