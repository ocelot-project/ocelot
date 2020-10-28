{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.ocelot.ui;
in
{
  options.ocelot.ui.locker = {
    time = mkOption {
      type = types.int;
      default = 45;
      description = ''
        The number of minutes without keyboard input, before the console
        locker automatically activates. A value of 0 disables automatic
        console locking.
      '';
    };

    message = mkOption {
      type = types.str;
      default = "Terminal locked by $USER from tty$XDG_VTNR.";
      description = ''
        A notification message displayed above the screen locker's
        password prompt. Shell variables contained in this string are
        expanded by the user's shell.
      '';
    };
  };

  config = {
    services.physlock = {
      enable = true;
      allowAnyUser = true;
    };

    # Since physlock uses its own virtual terminal, this option is safe
    # to enable.
    services.xserver.enableCtrlAltBackspace = true;

    boot.kernelParams = [ "consoleblank=300" ];
  };
}
