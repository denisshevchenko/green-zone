#!/bin/bash

readonly RASPBIAN=2018-04-18-raspbian-stretch-lite
readonly DISK_IMAGE=$RASPBIAN.img
readonly DISK_IMAGE_ARCHIVE=$RASPBIAN.7z

readonly REPOSITORY=https://github.com/denisshevchenko/green-zone.git
readonly LOCAL_REPOSITORY=$HOME/green-zone

readonly MOUNT_POINT=/mnt/raspbian

cd "$HOME" || exit

echo "Prepare QEMU..."
sudo apt install qemu-user-static binfmt-support
sudo update-binfmts --enable

echo "Check if disk image is here (it is assumed that it can be cached)..."
if [[ ! -f "$DISK_IMAGE" ]]; then
    wget -O "$DISK_IMAGE" "URL"
fi

echo "Mount disk image..."
sudo mount "$DISK_IMAGE" -o loop,offset=$((512 * 98304)) "$MOUNT_POINT"

echo "chroot..."
sudo chroot "$MOUNT_POINT" qemu-arm-static /bin/bash

echo "Take latest version of our code..."
rm -rf "$LOCAL_REPOSITORY"
git clone --branch master --single-branch -- "$REPOSITORY" "$LOCAL_REPOSITORY"
cd "$LOCAL_REPOSITORY" || exit

echo "Building..."
cabal update
cabal install

echo "Exit from chroot..."
exit

echo "Get executables, for CI-artifacts..."
mv /mnt/raspbian/root/../* /tmp

echo "Umount disk image..."
sudo umount "$MOUNT_POINT"

echo "Compress disk image..."
7z a "$DISK_IMAGE_ARCHIVE" "$DISK_IMAGE"

echo "CI-artifacts will be copied later, please see ./config.yml"
