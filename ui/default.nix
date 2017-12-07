{ config, lib, pkgs, ...}:

with lib;
let
  cfg = config.ocelot.ui;
in
{
  imports = [
    ./gui.nix
  ];

  options.ocelot.ui = {
    graphical = mkOption {
      type = types.bool;
      default = true;
      description = ''
        If enabled, Ocelot is configured for graphical operation. Otherwise,
        Ocelot uses a text-based user interface.
      '';
    };

    # TODO: replace bindCapsToEscape and bindCapsToControl with a more flexible
    # binding mechanism
    bindCapsToEscape = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to rebind the caps lock key to function as an escape key.
      '';
    };

    bindCapsToControl = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to rebind the caps lock key to function as a control key.
      '';
    };
  };

  config = {
    assertions = [
      {
        assertion = !(cfg.bindCapsToEscape && cfg.bindCapsToControl);
        message = "cannot rebind caps lock to more than one key";
      }
    ];
  };
}
