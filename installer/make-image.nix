{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/iso-image.nix>
    <nixpkgs/nixos/modules/profiles/all-hardware.nix>
    <nixpkgs/nixos/modules/profiles/base.nix>
    <nixpkgs/nixos/modules/profiles/installation-device.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
    ./live-system.nix
  ];

  isoImage = {
    isoBaseName = "ocelot-${config.ocelot.versions.system.Ocelot.version}-nixos";
    isoName = "${config.isoImage.isoBaseName}-${config.system.nixosLabel}-${pkgs.stdenv.system}.iso";
    volumeID = substring 0 11 "_OCELOTLIVE";
    makeEfiBootable = true;
    makeUsbBootable = true;
    appendToMenuLabel = " Ocelot Live";
  };
}
