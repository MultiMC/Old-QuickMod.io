#!/bin/bash

# A small script to package QuickMod and its necessary files into a tarball.

if [ -d "dist/deploy" ]; then
  rm -r dist/deploy;
fi

# Clean up if we exit.
trap "echo \"Cleaning up...\"; rm -r dist/deploy" EXIT

# Create package directory.
mkdir -p dist/deploy &&

# Add necessary files.
cp -r static dist/deploy &&
cp -r config dist/deploy &&
cp dist/build/quickmod-io/quickmod-io dist/deploy &&

# Remove session key.
rm dist/deploy/config/client_session_key.aes &&

# Tarball
pushd dist &&
tar cvzf qmio.tar.gz deploy
popd

