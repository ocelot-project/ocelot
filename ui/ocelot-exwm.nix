{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.xserver.windowManager.ocelot-exwm;
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
        export VISUAL=${pkgs.ocelotEmacs}/bin/emacsclient
        export EDITOR="$VISUAL"
        exec dbus-launch --exit-with-session ${pkgs.ocelotEmacs}/bin/emacs \
        --fullscreen --ocelot-graphical
      '';
    };
  };
}
