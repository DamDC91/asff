#!/bin/bash

set -e

VERSION=$(grep -i "Crate_Version" config/asff_config.ads | grep -o "\".*\"" | sed 's/"//g')
DIR=asff-$VERSION
PROJECT_DIR=$(realpath $(dirname $0))
mkdir -p "$DIR/bin"
mkdir -p "$DIR/plugin"
mkdir -p "$DIR/completion"
cp ./bin/asff "$DIR/bin"
cp ./plugin/asff.py "$DIR/plugin"
cp ./completion/asff_completion "$DIR/completion"
tar cvzf "rpmbuild/SOURCES/$DIR.tar.gz" "$DIR"
rpmbuild --define "_topdir $PROJECT_DIR/rpmbuild" --define "VERSION ${VERSION}" -ba rpmbuild/SPECS/asff.spec
rm -rf "$DIR"
