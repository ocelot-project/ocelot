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
    # nixpkgs.config.packageOverrides = pkgs: rec {
    #   emacs = import ./ocelot-emacs.nix {
    #     inherit lib pkgs external;
    #     inherit (pkgs) emacsPackagesNg;
    #     inherit (pkgs) emacs;
    #     inherit (pkgs) stdenv;
    #     inherit (pkgs) git;
    #     inherit (pkgs) writeText;
    #     inherit (pkgs) writeScript;
    #     inherit (pkgs) fetchFromGitHub;
    #     versioning = config.ocelot.versions;
    #     earlyBootForegroundColor = cfg.earlyBootForegroundColor;
    #     earlyBootBackgroundColor = cfg.earlyBootBackgroundColor;
    #     globalDistribution = config.ocelot.emacs.distribution;
    #     credentialsTimeout = config.ocelot.security.credentialsTimeout;
    #     workspaces = config.ocelot.ui.workspaces;
    #     lockerMessage = config.ocelot.ui.locker.message;
    #   };
    # };

    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/183110577003838110e05b9840e109b2089bb499.tar.gz";
        sha256 = "02fiapwd37ld5brv7isrqs44ssv5xjbap99cmwbk16amzyisi2pb";
      }))
      (import ./ocelot-emacs-layer.nix {
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
