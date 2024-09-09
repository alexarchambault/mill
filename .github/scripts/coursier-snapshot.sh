#!/usr/bin/env bash
set -e

git clone https://github.com/alexarchambault/coursier.git -b debug-mill-ci-windows
cd coursier
git fetch --tags
git submodule update --init --recursive
./mill -i __.publishLocal
cd -
