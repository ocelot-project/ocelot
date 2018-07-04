{ config, lib, pkgs, ...}:

with lib;

let
  graphicalEmacsSession = pkgs.writeScript "emacs-session.sh" ''
    xrdb -merge "$HOME/.Xresources"
    ${pkgs.xlibs.xset}/bin/xset r rate 200 30 # Set the keyboard repeat rate
    ${optionalString config.ocelot.ui.keyboard.bindCapsToEscape
    "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:escape"}
    ${optionalString config.ocelot.ui.keyboard.bindCapsToControl
    "${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps"}

    # TODO: manage mutable state much better than this
    if [ ! -e "$HOME/.xscreensaver" ]; then
    cp ${../ui/configs/dot_xscreensaver} $HOME/.xscreensaver
    chmod 644 $HOME/.xscreensaver
    fi

    xsetroot -solid black &
    ${(optionalString (config.ocelot.ui.locker.time > 0)) ''
      xset s ${toString (config.ocelot.ui.locker.time * 60 + 10)} ${toString (config.ocelot.ui.locker.time * 60 + 10)}
      xset dpms ${toString (config.ocelot.ui.locker.time * 60)} ${toString (config.ocelot.ui.locker.time * 60 + 10)} ${toString (config.ocelot.ui.locker.time * 60 + 10)}
      xss-lock -- physlock -m -p "${config.ocelot.ui.locker.message}" &
    ''}
    xbanish &

    export VISUAL=${pkgs.emacs}/bin/emacsclient
    export EDITOR="$VISUAL"
    exec dbus-launch --exit-with-session ${pkgs.emacs}/bin/emacs \
         --fullscreen --ocelot-graphical
  '';
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
      olman
    ];

    security.sudo.extraConfig = ''

    # Explicitly set a sudo timeout.
    # (from ocelot.security.credentialsTimeout)
    Defaults timestamp_timeout=${toString cfg.credentialsTimeout}
    '';

    nixpkgs.config.packageOverrides = pkgs: rec {
      olman = pkgs.callPackage ./olman.nix { };
    };

    services.xserver.tty = null;
    services.xserver.display = null;
    services.xserver.autorun = false;
    services.xserver.enableCtrlAltBackspace = true;

    systemd.services."olman@" = {
      conflicts = [ "getty@%i.service" ];
      onFailure = [ "getty@%i.service" ];
      # after = [ "systemd-user-sessions.service" "plymouth-quit-wait.service" "systemd-logind.service" "systemd-vconsole-setup.service" ];
      # requires = [ "systemd-logind.service" ];
      # before = [ "getty.target" ];
      aliases = [ "autovt@tty1.service" "autovt@tty2.service" ];
      restartIfChanged = false;

      unitConfig = {
        IgnoreOnIsolate = true;
        ConditionPathExists = "/dev/tty0";
      };

      serviceConfig = {
        Type = "idle";
        Restart = "always";
        RestartSec = 0;
        UtmpIdentifier = "%I";
        TTYPath = "/dev/%I";
        TTYReset = true;
        TTYVHangup = true;
        TTYVTDisallocate = true;
        KillMode = "process";
        IgnoreSIGPIPE = false;
        SendSIGHUP = true;
        ExecStart= [
          ""
          "@${pkgs.utillinux}/sbin/agetty agetty --skip-login --login-program ${pkgs.olman}/bin/olman -o 'emacs -- -Q -nsl --no-init-file -l ${./login-ocelot-theme.el} -l ${./login.el}' --noclear --keep-baud %I 115200,38400,9600 $TERM"
        ];
      };
    };

    environment.etc."olman.toml".text = ''
    [olman]
    session_default_shell = "${pkgs.runtimeShell}"

    [graphical]
    command = "exec ${pkgs.xorg.xinit}/bin/xinit $OLMAN_SESSION -- ${config.services.xserver.displayManager.xserverBin} $OLMAN_X_DISPLAY ${concatStringsSep " " config.services.xserver.displayManager.xserverArgs} vt$OLMAN_VT"

    [[session]]
    name = "Graphical Emacs"
    graphical = true
    command = "${graphicalEmacsSession}"

    [[session]]
    name = "Textmode Emacs"
    command = "exec ${pkgs.emacs}/bin/emacs -nw"

    [[session]]
    name = "Textmode Shell"
    '';
  };
}
