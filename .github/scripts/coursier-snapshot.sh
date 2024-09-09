#!/usr/bin/env bash
set -e

if ! test -d coursier; then
  git clone https://github.com/alexarchambault/coursier.git -b debug-mill-ci-windows
  cd coursier
  git fetch --tags
  git submodule update --init --recursive
  cd -
fi

cd coursier
./mill -i 'core.jvm[2.13.12].publishLocal' +\
  'cache.jvm[2.13.12].publishLocal' +\
  'proxy-setup.publishLocal' +\
  'util.jvm[2.13.12].publishLocal' +\
  'coursier.jvm[2.13.12].publishLocal'
cd -
