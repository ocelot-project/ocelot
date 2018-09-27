{ config, lib, pkgs, ...}:

with lib;

let
  tomlValue = x: if builtins.isBool x
            then boolToString x
            else if builtins.isString x
            then ''"${toString x}"''
            else "${toString x}";

  sessionsToToml = sessions: concatMapStrings (s:
    ''[[session]]
${concatStringsSep "\n" (mapAttrsToList (n: v:
  "${n} = ${tomlValue v}") s)}

    ''
  ) sessions;

  cfg = config.ocelot.ui.login;
in

{
  options.ocelot.ui.login = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to enable the Ocelot Login Manager, a login(1) replacement.
      '';
    };

    terminals = mkOption {
      type = types.listOf types.string;
      default = ["tty1" "tty2" "tty3" "tty4"];
      description = ''
        A list of terminal devices for which the Ocelot Login Manager should
        automatically start.
      '';
    };

    sessions = mkOption {
      type = types.listOf (types.attrsOf types.unspecified);
      default = mkOrder 500 [ { name = "Textmode Shell"; } ];
      description = ''
        A list of sessions that will be visible to the user from the Ocelot
        Login Manager frontend. Each session describes a set of programs that
        will run after the user logs in.
      '';
    };

    extraConfig = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Additional lines to add to olman.toml.
      '';
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.config.packageOverrides = pkgs: rec {
      olman = pkgs.callPackage ./olman.nix { };
    };

    environment.systemPackages = with pkgs; [
      olman
    ];

    # For graphical sessions, instruct the X server not to handle TTY or
    # DISPLAY assignment, since olman handles that. Also, disable autorun
    # since olman replaces a traditional X display manager.
    services.xserver.tty = null;
    services.xserver.display = null;
    services.xserver.autorun = false;

    systemd.services."olman@" = {
      conflicts = [ "getty@%i.service" ];
      onFailure = [ "getty@%i.service" ];
      # after = [ "systemd-user-sessions.service" "plymouth-quit-wait.service" "systemd-logind.service" "systemd-vconsole-setup.service" ];
      # requires = [ "systemd-logind.service" ];
      # before = [ "getty.target" ];
      aliases = map (tty: "autovt@${tty}.service") cfg.terminals;
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

    ${cfg.extraConfig}
    ${sessionsToToml cfg.sessions} '';
  };
}
