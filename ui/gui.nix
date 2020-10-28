{ config, lib, pkgs, ...}:

with lib;

let
  graphicalEmacsSession = pkgs.writeScript "graphical-emacs.sh" ''
    xrdb -merge "$HOME/.Xresources"
    ${pkgs.xlibs.xset}/bin/xset r rate 200 30 # Set the keyboard repeat rate
    ${optionalString config.ocelot.ui.keyboard.bindCapsToEscape
    "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:escape"}
    ${optionalString config.ocelot.ui.keyboard.bindCapsToControl
    "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps"}
    # Configure scancode 135 (<menu>) to be right Hyper:
    xmodmap -e "clear mod3" -e "keycode 135 = Hyper_R" -e "add mod3 = Hyper_R" &

    # TODO: manage mutable state much better than this
    if [ ! -e "$HOME/.xscreensaver" ]; then
    cp ${./configs/dot_xscreensaver} $HOME/.xscreensaver
    chmod 644 $HOME/.xscreensaver
    fi

    xsetroot -solid black &
    ${(optionalString (config.ocelot.ui.locker.time > 0)) ''
      xset s ${toString (config.ocelot.ui.locker.time * 60 + 10)} ${toString (config.ocelot.ui.locker.time * 60 + 10)}
      xset dpms ${toString (config.ocelot.ui.locker.time * 60)} ${toString (config.ocelot.ui.locker.time * 60 + 10)} ${toString (config.ocelot.ui.locker.time * 60 + 10)}
      xss-lock -- physlock -m -p "${config.ocelot.ui.locker.message}" &
    ''}
    # TODO: write an elisp user service manager with automatic respawning
    xbanish &

    # Have to add the bloat of a polkit agent or else libvirt breaks
    ${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent &

    export VISUAL=${pkgs.ocelotEmacs}/bin/emacsclient
    export EDITOR="$VISUAL"
    exec dbus-launch --exit-with-session ${pkgs.ocelotEmacs}/bin/emacs \
         --fullscreen --ocelot-graphical
  '';

  jitGraphicalEmacsSession = pkgs.writeScript "jit-graphical-emacs.sh" ''
    xrdb -merge "$HOME/.Xresources"
    ${pkgs.xlibs.xset}/bin/xset r rate 200 30 # Set the keyboard repeat rate
    ${optionalString config.ocelot.ui.keyboard.bindCapsToEscape
    "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:escape"}
    ${optionalString config.ocelot.ui.keyboard.bindCapsToControl
    "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps"}
    # Configure scancode 135 (<menu>) to be right Hyper:
    xmodmap -e "clear mod3" -e "keycode 135 = Hyper_R" -e "add mod3 = Hyper_R" &

    # TODO: manage mutable state much better than this
    if [ ! -e "$HOME/.xscreensaver" ]; then
    cp ${./configs/dot_xscreensaver} $HOME/.xscreensaver
    chmod 644 $HOME/.xscreensaver
    fi

    xsetroot -solid black &
    ${(optionalString (config.ocelot.ui.locker.time > 0)) ''
      xset s ${toString (config.ocelot.ui.locker.time * 60 + 10)} ${toString (config.ocelot.ui.locker.time * 60 + 10)}
      xset dpms ${toString (config.ocelot.ui.locker.time * 60)} ${toString (config.ocelot.ui.locker.time * 60 + 10)} ${toString (config.ocelot.ui.locker.time * 60 + 10)}
      xss-lock -- physlock -m -p "${config.ocelot.ui.locker.message}" &
    ''}
    # TODO: write an elisp user service manager with automatic respawning
    xbanish &

    # Have to add the bloat of a polkit agent or else libvirt breaks
    ${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent &

    export VISUAL=${pkgs.ocelotEmacsJit}/bin/emacsclient
    export EDITOR="$VISUAL"
    exec dbus-launch --exit-with-session ${pkgs.ocelotEmacsJit}/bin/emacs \
         --fullscreen --ocelot-graphical
  '';

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
      xss-lock
      xorg.setxkbmap
      xbanish
      alacritty

      ghostscriptX # DocView mode (PDF)
      # unoconv # DocView mode (LibreOffice/MS formats)
    ];

    ocelot.ui.login.sessions = mkOrder 100 [
      {
        name = "Graphical Emacs";
        graphical = true;
        command = "${graphicalEmacsSession}";
      }
       {
         name = "Graphical Emacs (with JIT)";
         graphical = true;
         command = "${jitGraphicalEmacsSession}";
       }
    ];

    services.xserver = {
      enable = true;

      windowManager = {
        ocelot-exwm.enable = true;
      };

      desktopManager = {
        default = "none+ocelot-exwm";
        xterm.enable = false;
      };
    };
  };
}
