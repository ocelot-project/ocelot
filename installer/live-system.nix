{ config, lib, pkgs, ... }:

# TODO: live system leaves its boot partition dirty when it shuts down

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

  security.pam.services.ocelot.allowNullPassword = true;

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
}
