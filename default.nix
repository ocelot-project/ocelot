{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ./emacs
    ./ui
    ./security
  ];

  options.ocelot = {
    versions = mkOption {
      type = types.attrsOf types.attrs;
      default = {
        system.Ocelot.version = "0.1";
        emacs.Emacs.version =
          builtins.head (builtins.match "[^0-9]*([0-9].*)" pkgs.ocelotEmacs.emacs.version) +
          " (JIT " + pkgs.ocelotEmacsJit.emacs.version + ")";
        application = {};
        package-management.Nix.version = pkgs.nix.version;
        base-system.NixOS.version = (if builtins.hasAttr "nixos" config.system
          then config.system.nixos.release # as of 18.03: Feb 23, 2018
          else config.system.nixosRelease);
        kernel.Linux.version = pkgs.linux.version;
        platform.Platform.version = builtins.head
          (builtins.match "(.*)-.*" builtins.currentSystem);
      };
    };

    boot.showBootloaderOnShift = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Only show the bootloader interface when the shift key is held down,
        where available.
      '';
    };
  };

  config = mkMerge [{
    environment.systemPackages = with pkgs; [
      # Base packages:
      ocelotEmacs
      git # Nix-based distros usually lean heavily on git
      man_db # extra documentation
      manpages # extra documentation

      # Emacs dependencies:
      ag # helm-ag and Spacemacs smart search
      poppler # DocView mode (PDF)
      poppler_utils # DocView mode (PDF)
      ctags # Exuberant Ctags, for building tags files
      python3 # treemacs

      # System utilities:
      pv # A modernized `dd`
      direnv
    ];

    services.lorri.enable = true;

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;

      fonts = with pkgs; [
        fira
        fira-mono
        dejavu_fonts
        source-sans-pro
        source-code-pro
        source-serif-pro
        inconsolata
        ubuntu_font_family
        emacs-all-the-icons-fonts
        material-icons
        twemoji-color-font

        # The most popular Google Fonts with Nix derivations:
        roboto
        roboto-mono
        roboto-slab
        opensans-ttf
        lato
        montserrat
        raleway
        noto-fonts
        quattrocento
        quattrocento-sans
        dosis
        oxygenfonts
        cabin
        crimson
        libre-baskerville
        libre-franklin
        eb-garamond
        comfortaa
        shrikhand
        oldstandard
      ];
    };
  } (mkIf config.ocelot.boot.showBootloaderOnShift {
      boot.loader.grub.extraConfig = ''
        if keystatus; then
        if keystatus --shift; then
        set timeout=-1
        else
        set timeout=0
        fi
        fi
      '';
    })];
}
