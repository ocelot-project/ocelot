{ config, pkgs, ... }:

{
  # TODO: write an OS-independent disk mounting interface
  # TODO: on Linux, modify devmon and udevil to not pull in udisks
  # NOTE: udevil doesn't need udisks if it's suid (chmod +s udevil)
  # TODO: analyze suid udevil's security risk profile
  # NOTE: disks UI could be a buffer that calls devmon on opening, lists all
  # the devmon enumerated disks and their status, then kills devmon on buffer
  # kill, unmounting all devmon disks
  # (or, just start devmon on boot and have the UI enumerate the disks and
  # provide an umount bind)
  # NOTE: for a boot-time devmon that automounts all the new drives it finds,
  # a minimal UI is just disk status, and an unmount binding
  # (with the option to unmount the drive itself, which tells devmon to unmount
  # all of its partitions)
  # TODO: use eudev on Linux

  services.devmon.enable = true;

  environment.systemPackages = with pkgs; [
    udevil
  ];
}
