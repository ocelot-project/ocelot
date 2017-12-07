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
      emacs
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
        ubuntu_font_family
      ];
    };
  } (mkIf pkgs.config.allowUnfree {
      fonts.fonts = with pkgs; [
        corefonts
      ];
    })
      (mkIf config.ocelot.boot.showBootloaderOnShift {
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
