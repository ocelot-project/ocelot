{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.ocelot.security;

in

{
  options.ocelot.security = {
    credentialsTimeout = mkOption {
      type = types.int;
      default = 5;
      description = ''
        The number of minutes that sudo and Emacs `tramp` should retain
        login credentials for.
      '';
    };
  };

  config = {
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    environment.systemPackages = with pkgs; [
      pass
      gnupg
    ];

    security.sudo.extraConfig = ''

    # Explicitly set a sudo timeout.
    # (from ocelot.security.credentialsTimeout)
    Defaults timestamp_timeout=${toString cfg.credentialsTimeout}
    '';
  };
}
