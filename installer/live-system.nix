{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ../default.nix
  ];

  boot.loader.grub.memtest86.enable = true;
  boot.supportedFilesystems = [ "zfs" ];

  fileSystems."/data" = {
    label = "OCELOT_DATA";
    options = [ "nofail" "rw" "uid=1000" "gid=100" ];
  };

  installer.cloneConfigIncludes = [ "/data/ocelot/installer/make-image.nix" ];

  users.extraUsers = {
    root.initialHashedPassword = "";

    ocelot = {
      isNormalUser = true;
      uid = 1000;
      initialHashedPassword = "";
      description = "Ocelot Live User";
      extraGroups = [ "wheel" ];
    };
  };

  users.groups.users.gid = mkForce 100;

  services.xserver.displayManager.slim = {
    defaultUser = "ocelot";
    autoLogin = true;
  };

  services.mingetty.autologinUser = mkForce "ocelot";

  security.sudo = {
    enable = mkForce true;
    wheelNeedsPassword = false;
  };

  environment.systemPackages = with pkgs; [
    wget
    curl
    mkpasswd
    nix-repl
    qutebrowser
    firefox
  ];

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };

  systemd.services.apple-keyboard = {
    description = "Reconfigure an Apple USB keyboard, if attached.";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
      ExecStart = "${pkgs.stdenv.shell} -c \"echo 2 > /sys/module/hid_apple/parameters/fnmode\"";
      ExecStop = "${pkgs.stdenv.shell} -c \"echo 1 > /sys/module/hid_apple/parameters/fnmode\"";
    };
  };
}
