{ config, lib, pkgs, ...}:

with lib;

{
  options.ocelot.emacs = {
    distribution = mkOption {
      type = types.str;
      default = "ask";
      description = ''
        Chooses a global default Emacs configuration distribution
        for all users. This only takes effect for users who do not
        have a .emacs.d directory or .emacs file in their home
        directory when an interactive emacs launches under their
        user account.

        Possible values are: "spacemacs" to select
        the Spacemacs distribution, "prelude" for the Prelude
        distribution, "ask" to prompt the user, and "none" for
        no distribution. It is highly recommended to use an Emacs
        distribution under Ocelot, to make configuration management
        much more bearable.

        If you are an Emacs beginner or are more experienced with
        vim, "spacemacs" is a good option here. If you have an
        existing Emacs configuration, consider selecting "prelude"
        and porting your configuration to work with the Prelude
        distribution.
        '';
    };
  };

  options.users.users = mkOption {
    type = with types; loaOf (submodule {
      options.emacs.distribution = mkOption {
        type = types.str;
        default = "global";
        description = ''
        Per-user emacs configuration distribution setting. See
        `options.ocelot.emacs.distribution` for the global
        setting and documentation for the possible values.

        This option can be set to any of the
        `options.ocelot.emacs.distribution` values, or "global"
        (the default) to use the global setting.
        '';
      };
    });
  };
}
