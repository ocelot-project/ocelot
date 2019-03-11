{ config, lib, pkgs, ...}:

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
      type = types.string;
      default = "#FFFFFF";
      description = ''
        Configures emacs to set a foreground text color early in its startup
        process, when running graphically.
      '';
    };

    earlyBootBackgroundColor = mkOption {
      type = types.string;
      default = "#110024";
      description = ''
        Configures emacs to set a background text color early in its startup
        process, when running graphically.
      '';
    };
  };

  config = {
    nixpkgs.config.packageOverrides = pkgs: rec {
      emacs = import ./ocelot-emacs.nix {
        inherit lib;
        inherit (pkgs) emacsPackagesNg;
        inherit (pkgs) emacs;
        inherit (pkgs) stdenv;
        inherit (pkgs) git;
        inherit (pkgs) writeText;
        inherit (pkgs) writeScript;
        inherit (pkgs) fetchFromGitHub;
        versioning = config.ocelot.versions;
        earlyBootForegroundColor = cfg.earlyBootForegroundColor;
        earlyBootBackgroundColor = cfg.earlyBootBackgroundColor;
        globalDistribution = config.ocelot.emacs.distribution;
        credentialsTimeout = config.ocelot.security.credentialsTimeout;
        workspaces = config.ocelot.ui.workspaces;
        lockerMessage = config.ocelot.ui.locker.message;
      };
    };
  };
}
