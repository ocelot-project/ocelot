{ config, lib, pkgs, external, ...}:

with lib;

let
  cfg = config.ocelot.ui;
in
{
  imports = [
    ./distro
  ];

  options.ocelot.ui = {
    earlyBootForegroundColor = mkOption {
      type = types.str;
      default = "#FFFFFF";
      description = ''
        Configures emacs to set a foreground text color early in its startup
        process, when running graphically.
      '';
    };

    earlyBootBackgroundColor = mkOption {
      type = types.str;
      default = "#110024";
      description = ''
        Configures emacs to set a background text color early in its startup
        process, when running graphically.
      '';
    };
  };

  config = {
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/183110577003838110e05b9840e109b2089bb499.tar.gz";
        sha256 = "02fiapwd37ld5brv7isrqs44ssv5xjbap99cmwbk16amzyisi2pb";
      }))
      (import (builtins.fetchTarball {
        url = "https://github.com/ocelot-project/local-elpa/archive/0c7e815d4b59136f54bbb027f435fd57d578e9fa.tar.gz";
        sha256 = "1gv33b3virddizhpqf0ywnar9q33j49323c0xlph4nmljpwqvgy4";
      }))
      (import ./overlay.nix {
        inherit (config.ocelot) versions;
        globalDistribution = config.ocelot.emacs.distribution;
        userDistributions = [];
        inherit (config.ocelot.security) credentialsTimeout;
        inherit (config.ocelot.ui) workspaces;
        lockerMessage = config.ocelot.ui.locker.message;
        earlyBootForegroundColor = cfg.earlyBootForegroundColor;
        earlyBootBackgroundColor = cfg.earlyBootBackgroundColor;
      })
    ];
  };
}
