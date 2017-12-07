{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.xserver.windowManager.ocelot-exwm;
  emacs = pkgs.emacs;
in
{
  options = {
    services.xserver.windowManager.ocelot-exwm = {
      enable = mkEnableOption "ocelot-exwm";
    };
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager.session = singleton {
      name = "ocelot-exwm";
      start = ''
        export VISUAL=${pkgs.emacs}/bin/emacsclient
        export EDITOR="$VISUAL"
        exec dbus-launch --exit-with-session ${pkgs.emacs}/bin/emacs \
        --fullscreen --ocelot-graphical
      '';
    };
  };
}
