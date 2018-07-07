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
  };

  options.services.xserver.xrandrHeads = mkOption {
    options = [{
      workspaces = mkOption {
        type = types.listOf types.int;
        default = [];
        description = ''
          The list of EXWM workspaces assigned to this monitor.
        '';
      };
    }];
  };

  config = {
    ocelot.ui.login.sessions = mkOrder 500 [
      {
       name = "Textmode Emacs";
       command = "exec ${pkgs.emacs}/bin/emacs -nw";
      }

      {
        name = "Textmode Shell";
      }
    ];
  };
}
