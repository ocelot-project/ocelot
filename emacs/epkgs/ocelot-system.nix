# This package collects the early startup and system-dependent
# elisp used by Ocelot.
# TODO: set `ocelot-pinned-packages` using Nix
{ lib, callPackage, writeText, versioning,
  exwm, earlyBootBackgroundColor, earlyBootForegroundColor }:

with lib;

let
  versionsToPairs = attrs: concatStringsSep "\n" (
mapAttrsToList (name: value: "(cons '${name} \"${value.version}\")") attrs);
ocelotSystemCfg = writeText "ocelot-system.el" ''
  (setq ocelot-early-boot-background-color "${earlyBootBackgroundColor}")
  (setq ocelot-early-boot-foreground-color "${earlyBootForegroundColor}")

  (setq ocelot-pinned-packages '(cl-generic exwm xelb ocelot-system ocelot))
  (setq ocelot-frozen-packages ocelot-pinned-packages)
  (setq ocelot-spacemacs-layer-path "${../distro/spacemacs-layers}/")
  (setq ocelot-software-versions (list
  ${versionsToPairs versioning.system}
  ${versionsToPairs versioning.emacs}
  ${versionsToPairs versioning.application}
  ${versionsToPairs versioning.package-management}
  ${versionsToPairs versioning.base-system}
  ${versionsToPairs versioning.kernel}
  ${versionsToPairs versioning.platform}))

  (require 'ocelot-startup)
  (require 'ocelot-defaults)

  (provide 'ocelot-system)
'';
in

callPackage ({ melpaBuild, lib }: melpaBuild {
  pname = "ocelot-system";
  version = versioning.system.Ocelot.version;
  src = ./ocelot-system;
  packageRequires = [ exwm ]; # TODO: try putting exwm here
  preBuild = ''
    cp ${ocelotSystemCfg} ocelot-system.el
  '';
  meta = {
    license = lib.licenses.free;
  };
}) {}
