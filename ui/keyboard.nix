{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.ocelot.ui.keyboard;

in

{
  options.ocelot.ui.keyboard = {
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

    preferFunctionKeys = mkOption {
      type = types.bool;
      default = true;
      description = ''
      TODO: Needs implementation.

      If true, the F-keys on the keyboard will act as function keys when
      pressed without fn held, and as media or special keys when pressed
      with fn held. If false, this is reversed: the F-keys act as media
      or special keys when pressed alone, and as standard function keys
      when pressed with fn held.
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
