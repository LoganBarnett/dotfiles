#!/usr/bin/env bash
################################################################################
# Create the partitions on the main disk of the machine.
#
# You'll see a lot of superfluous use of export so this can be copied into a
# shell for testing, since this is a rather dangerous script.
################################################################################
set -euo pipefail
# set -x

function slog {
  printf "\e[93m[\e[96m$(basename $0)\e[93m]\e[0m " 1>&2
  echo -e "$*" 1>&2
}

# `grep` doesn't work well with `set -o pipefail` because it returns exit code 1
# if there are no matches.  We don't care about that, so make sure we zero-ize a
# 1.  Other errors (2+) are real errors we should pay attention to though.
function greppipe {
  grep "$@" || test $? = 1;
}

export -f slog

# 0. Unmount anything we might have mounted at /mnt and /mnt/boot.
sudo umount /mnt/boot 2>/dev/null || true
sudo umount /mnt 2>/dev/null || true
# 1. See if we can find the disk.
# We need a far more robust way of detecting drives.
disk=$(\
       find /dev \( -name 'nvme*' -o -name 'sd*' \) \
         | greppipe --invert-match 'loop' \
         | greppipe --invert-match --extended-regexp 'p[0-9]' \
         | greppipe --extended-regexp 'n[0-9]')
if [[ "$disk" == '' ]]; then
  slog "No workable disk found.  Aborting..."
  exit 1
else
  slog "Found disk ${disk}."
fi
# 2. Create the boot partition.
# --no-reread prevents the "This disk is currently in use" error even though
# it's not in use in any meaningful way.
cat <<EOF | sudo sfdisk --no-reread $disk
label: gpt

start=2048, size=512MiB, type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, name="EFI System Partition"
type=0FC63DAF-8483-4772-8E79-3D69D8477DE4
EOF
# Unsure what defaults this - perhaps sfdisk itself?
export boot_partition='EFIBOOT'
# The first partition is always the boot.
# -n is the label to use, obviously.
sudo mkfs.vfat $disk'p1' -n $boot_partition
# If somehow that didn't work, you can use:
# fatlabel /dev/nvme0n1p1 EFIBOOT
# The first partition is always going to be the boot partition.
export partition=$disk'p2'
# 3. Format the disk based on our preferences.
# pvcreate won't work until all device mappings are already gone (assuming
# there's partitions in the first place).
# You might see this error:
# /dev/mapper/control: open failed: Permission denied
# Failure to communicate with kernel device-mapper driver.
# Incompatible libdevmapper 1.02.197 (2023-11-21) and kernel driver (unknown version).
# Command failed.
# I think this is okay and probably the result of an empty line being executed.
# awk NF removes empty lines because (N)umber of (F)ields is > 0.  See
# https://stackoverflow.com/questions/23544804/how-awk-nf-filename-is-working
# for an attempt of a deeper explanation.
sudo dmsetup ls \
  | grep --invert-match 'No devices found' \
  | cut -f1 \
  | awk NF \
  | xargs -I{} sudo dmsetup remove {} \; \
  || true
# -ff is like force but really force.  I couldn't find a long argument for it in
# the help, but the error message says to use it.
# --yes is needed because you can be prompted if a partition already exists.
sudo pvcreate $partition -ff --yes
export volume_group='vg'
sudo vgcreate $volume_group $partition
# Start with 10GB to allow a rather large NixOS installation if needed, but
# should fit pretty much any disk we can imagine.  The
# \`fileSystems.\"/\".autoResize = true\` declaration will expand it to fit the
# disk on first boot.
# --yes is needed because if something was already there we need to wipe it
# non-interactively.
sudo lvcreate --extents '100%FREE' --name root $volume_group --yes
# Now actually create the concrete partition.
# -L is the label.  Long arguments don't exist for it.
sudo mkfs.xfs /dev/$volume_group/root -L nixos
# Use xfs_io to change the label after the fact.
# -c is for command.
# -s is for someone hates long arguments that are self documenting.  See?  It
#    uses the 's'.
# The partition must be mounted first.  It should work (see below for the exact
# invocation).
# xfs_io -c "label -s nixos" /mnt/
# 4. Mount the disk to /mnt.
sudo mkdir -p /mnt
# Just to make sure thing stuck.  This will make /dev/disk/by-label populate
# correctly, which is indicative of total success.
sudo udevadm control --reload-rules && sudo udevadm trigger
# mount /dev/disk/by-label/nixos /mnt
# mount /dev/disk/by-label/$boot_partition /mnt/boot
sudo mount /dev/$volume_group/root /mnt
# I've seen where this isn't needed, and where it is.  Just do it anyways.
sudo mkdir -p /mnt/boot
sudo mount /dev/disk/by-label/$boot_partition /mnt/boot
slog "Disks are partitioned and mounted to /mnt and /mnt/boot.  This is ready \
for nixos-install."
