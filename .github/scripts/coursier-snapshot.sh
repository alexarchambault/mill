#!/usr/bin/env bash
set -e

git clone https://github.com/alexarchambault/coursier.git -b debug-mill-ci-windows
cd coursier
git fetch --tags
git submodule update --init --recursive
./mill -i 'core.jvm[2.13.12].publishLocal' +\
  'cache.jvm[2.13.12].publishLocal' +\
  'proxy-setup.publishLocal' +\
  'util.jvm[2.13.12].publishLocal' +\
  'coursier.jvm[2.13.12].publishLocal'
cd -
