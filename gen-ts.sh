#!/bin/bash

sh install.sh
ticket-manager typescript > frontend/src/Api.ts
echo "Generated typescript types"
