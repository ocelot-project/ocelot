{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.ocelot.ui;
in
{
  imports = [
    ./ocelot-exwm.nix
  ];

  config = mkIf cfg.graphical {
    environment.systemPackages = with pkgs; [
      xfontsel
      xdg_utils
      xscreensaver
      xorg.setxkbmap
      xbanish

      ghostscriptX # DocView mode (PDF)
      unoconv # DocView mode (LibreOffice/MS formats)
    ];

    services.xserver = {
      enable = true;

      windowManager = {
        ocelot-exwm.enable = true;
      };

      desktopManager = {
        default = "none";
        xterm.enable = false;
      };

      displayManager = {
        slim = {
          enable = true;
          theme = ./ocelot-slim-theme.tar.gz;

          # Hide the "Session: DM+WM" text
          extraConfig = ''
            session_x -1000
            session_y -1000
          '';
        };

        sessionCommands =
        ''
          xrdb -merge "$HOME/.Xresources"
          ${pkgs.xlibs.xset}/bin/xset r rate 200 30 # Set the keyboard repeat rate
          ${optionalString cfg.bindCapsToEscape
          "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:escape"}
          ${optionalString cfg.bindCapsToControl
          "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps"}

          # TODO: manage mutable state much better than this
          if [ ! -e "$HOME/.xscreensaver" ]; then
          cp ${./configs/dot_xscreensaver} $HOME/.xscreensaver
          chmod 644 $HOME/.xscreensaver
          fi

          xsetroot -solid black &
          xscreensaver -nosplash &
        '';
      };
    };

    # Hide the mouse pointer when typing
    services.xbanish.enable = true;
    # graphical-session.target isn't reliable; thanks systemd
    # prevent xbanish from respawning too quickly when it can't
    # grab a DISPLAY
    # TODO: make xbanish exit before the graphical session does
    # (or ditch systemd)
    systemd.user.services.xbanish.serviceConfig.RestartSec = mkForce 3;
  };
}
