{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ./emacs
    ./ui
    ./security
  ];

  options.ocelot.boot = {
    showBootloaderOnShift = mkOption {
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
      emacs
      git # Nix-based distros usually lean heavily on git
      man_db # extra documentation
      manpages # extra documentation

      # Emacs dependencies:
      ag # helm-ag and Spacemacs smart search
      poppler # DocView mode (PDF)
      poppler_utils # DocView mode (PDF)
    ];

    fonts = {
      enableFontDir = true;
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
