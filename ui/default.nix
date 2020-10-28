{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.ocelot.ui;
in
{
  imports = [
    ./keyboard.nix
    ./locker.nix
    ./login
    ./gui.nix
    ./disk-mounting.nix
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

    workspaces = mkOption {
      type = types.attrsOf (types.listOf types.int);
      default = {};
      description = ''
      An attribute set whose keys are RandR display names, and values are
      lists of EXWM workspace numbers assigned to each display.
      '';
    };
  };

  config = {
    ocelot.ui.login.sessions = mkOrder 500 [
      {
       name = "Textmode Emacs";
       command = "exec ${pkgs.ocelotEmacs}/bin/emacs -nw";
      }

      {
        name = "Textmode Shell";
      }
    ];
  };
}
