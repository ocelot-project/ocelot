# This package collects the early startup and system-dependent
# elisp used by Ocelot.
# TODO: set `ocelot-pinned-packages` using Nix
{ callPackage,
  writeText,
  exwm,
  earlyBootBackgroundColor,
  earlyBootForegroundColor }:

let
ocelotSystemCfg = writeText "ocelot-system.el" ''
  (setq ocelot-early-boot-background-color "${earlyBootBackgroundColor}")
  (setq ocelot-early-boot-foreground-color "${earlyBootForegroundColor}")

  (setq ocelot-pinned-packages '(cl-generic exwm xelb ocelot-system ocelot))
  (setq ocelot-frozen-packages ocelot-pinned-packages)
  (setq ocelot-spacemacs-layer-path "${../distro/spacemacs-layers}/")

  (require 'ocelot-startup)
  (require 'ocelot-defaults)

  (provide 'ocelot-system)
'';
in

callPackage ({ melpaBuild, lib }: melpaBuild {
  pname = "ocelot-system";
  version = "0.0.1"; # TODO: set the version using Nix
  src = ./ocelot-system;
  packageRequires = [ exwm ]; # TODO: try putting exwm here
  preBuild = ''
    cp ${ocelotSystemCfg} ocelot-system.el
  '';
  meta = {
    license = lib.licenses.free;
  };
}) {}
