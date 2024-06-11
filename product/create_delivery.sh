#!/bin/bash

set -e
set -x




create_archive () {
    local project_dir=$1
    local delivery_name=$2
    local archive_path=$3

    if [[ -d "$project_dir/tmp" ]]; then
        rm -rf "$project_dir/tmp"
    fi

    mkdir -p "$project_dir/tmp/$delivery_name/bin"
    mkdir -p "$project_dir/tmp/$delivery_name/plugin"
    mkdir -p "$project_dir/tmp/$delivery_name/completion"
    cp "$project_dir/bin/asff" "$project_dir/tmp/$delivery_name/bin"
    cp "$project_dir/plugin/asff.py" "$project_dir/tmp/$delivery_name/plugin"
    cp "$project_dir/completion/asff_completion" "$project_dir/tmp/$delivery_name/completion"

    cd "$project_dir/tmp/"
    tar cvzf "$archive_path" "$delivery_name"
    cd -
}

if [[ "$(git clean -n | wc -l)" != "0" ]]; then
    read -p "Are you sure you want to continue? [y/N] " prompt
    if [[ "$prompt" != "y" ]]; then
        exit 1
    fi
fi


SCRIPT_DIR=$(realpath $(dirname $0))
PROJECT_DIR=$(realpath "$SCRIPT_DIR/..")

VERSION=$(grep -i "Crate_Version" "$PROJECT_DIR"/config/asff_config.ads | grep -o "\".*\"" | sed 's/"//g')
DELIVERY_NAME="asff-$VERSION"
PROD_DIR="$SCRIPT_DIR/$DELIVERY_NAME"

if [[ -d "$PROD_DIR" ]]; then
    read -p "Are you sure you want to rm $PROD_DIR? [y/N] " prompt
    if [[ "$prompt" != "y" ]]; then
        exit 1
    fi
    rm -rf "$PROD_DIR"
fi
mkdir -p "$PROD_DIR"


ARCH="linux"
ARCHIVE_NAME="$DELIVERY_NAME-bin-$ARCH.tar.gz"
ARCHIVE_PATH="$PROD_DIR/$ARCHIVE_NAME"
create_archive "$PROJECT_DIR" "$DELIVERY_NAME" "$ARCHIVE_PATH"

mkdir -p "$SCRIPT_DIR/rpmbuild/SOURCES/"
cp "$ARCHIVE_PATH" "$SCRIPT_DIR/rpmbuild/SOURCES/$DELIVERY_NAME.tar.gz"

rpmbuild --define "_topdir $SCRIPT_DIR/rpmbuild" --define "VERSION ${VERSION}" -ba "$SCRIPT_DIR"/rpmbuild/SPECS/asff.spec
RPM_NAME="asff-$VERSION-1.x86_64.rpm"
cp "$SCRIPT_DIR/rpmbuild/RPMS/x86_64/$RPM_NAME" "$PROD_DIR"
cd "$PROD_DIR"
alien $RPM_NAME
cd -
