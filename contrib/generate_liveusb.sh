#!/usr/bin/env bash

ISO_SOURCE=$1
ISO_SOURCE_MOUNT_POINT=/tmp/nixos_iso_source
ISO_TARGET_CONTENTS_LOCATION=/tmp/nixos_iso_target
INITRD_WORKING_DIR=/tmp/nixos_initrd_work

CONFIG_TAG=$(git describe --long --dirty --abbrev=10 --tags --always)
CONFIG_FULL_PATH=$(git rev-parse --show-toplevel)
ISO_TARGET=${ISO_SOURCE%%.*}-custom-$CONFIG_TAG.iso

if [ -z "$ISO_SOURCE" ]; then
    echo "No source image provided, exiting"
    exit 1
fi

if [ -z "$ALLOW_DIRTY" ]; then
    echo "########## check git state #####################"
    case "${CONFIG_TAG}" in
        *dirty*)
            echo "You have dirty git state"
            exit 1
            ;;
    esac
fi

echo "########## cleanup target contents dir ##########"
sudo rm -rf "${ISO_TARGET_CONTENTS_LOCATION/*:?}"
echo "########## cleanup initrd working dir ###########"
sudo rm -rf "${INITRD_WORKING_DIR/*:?}"

echo "########## ensure source contents dir ##########"
mkdir -p "$ISO_SOURCE_MOUNT_POINT"
echo "########## ensure target contents dir ##########"
mkdir -p "$ISO_TARGET_CONTENTS_LOCATION"
echo "########## ensure initrd working dir ###########"
mkdir -p "$INITRD_WORKING_DIR"

echo "########## mount source ISO ####################"
sudo mount -o loop "$ISO_SOURCE" "$ISO_SOURCE_MOUNT_POINT"

echo "########## unpack initrd #######################"
cd $INITRD_WORKING_DIR && zcat $ISO_SOURCE_MOUNT_POINT/boot/initrd | cpio -idmv

echo "########## customize initrd ####################"
mkdir -p $INITRD_WORKING_DIR/etc/custom/nixos-config
cd $INITRD_WORKING_DIR/etc/custom && git clone "file://$CONFIG_FULL_PATH/" nixos-config
sudo rm -rf "$INITRD_WORKING_DIR/etc/custom/nixos-config/.git/"

echo "########## copy source to target ##########"
cp -r $ISO_SOURCE_MOUNT_POINT/* $ISO_TARGET_CONTENTS_LOCATION

echo "########## substitute initrd ###################"
sudo rm $ISO_TARGET_CONTENTS_LOCATION/boot/initrd
# fd . "$INITRD_WORKING_DIR" | cpio -o --format='newc' | gzip -9 | sudo tee $ISO_TARGET_CONTENTS_LOCATION/boot/initrd > /dev/null
fd . "$INITRD_WORKING_DIR" | cpio --null --create --verbose --format='newc' | gzip --best | sudo tee $ISO_TARGET_CONTENTS_LOCATION/boot/initrd > /dev/null
# cpio --null --create --verbose --format=newc | gzip --best > /boot/custom-initramfs.cpio.gz

echo "########## build target ISO ####################"
chmod a+w $ISO_TARGET_CONTENTS_LOCATION/isolinux/isolinux.bin
genisoimage -v -r -V NIXOS_ISO -b isolinux/isolinux.bin -no-emul-boot -boot-load-size 4 -boot-info-table \
            -c isolinux/boot.cat -o "$ISO_TARGET" "$ISO_TARGET_CONTENTS_LOCATION"

echo "########## unmount source ISO ####################"
sudo umount "$ISO_SOURCE"

echo "########## postprocess target ISO ####################"
isohybrid "$ISO_TARGET"
