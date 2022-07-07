# This package collects the early startup and system-dependent
# elisp used by Ocelot.
# TODO: set `ocelot-pinned-packages` using Nix
{ stdenv, lib, callPackage, writeText, writeScript, git, localElpa,
  versions, globalDistribution, userDistributions,
  epkgs, trivialBuild, spacemacs, prelude,
  earlyBootBackgroundColor, earlyBootForegroundColor, credentialsTimeout,
  workspaces, lockerMessage }:

with lib;

let
  versionsToPairs = attrs: concatStringsSep "\n" (
mapAttrsToList (name: value: "(cons '${name} \"${value.version}\")") attrs);

workspacesList = workspaces: concatStringsSep "\n" (
  mapAttrsToList (monitor: wsList: concatMapStringsSep "\n"
  (workspace: "${toString workspace} \"${monitor}\"") wsList) workspaces);

spacemacsRepoScript = writeScript "spacemacs-reset-repo.sh" ''
  #!${stdenv.shell}
  rm -rf "$HOME/.emacs.d/.git"
  cd "$HOME/.emacs.d" &&
  ${git}/bin/git init &&
  ${git}/bin/git checkout -b develop
  ${git}/bin/git remote add origin https://github.com/syl20bnr/spacemacs &&
  ${git}/bin/git fetch &&
  ${git}/bin/git reset --hard origin/develop &&
  ${git}/bin/git branch -u origin/develop
'';

preludeRepoScript = writeScript "prelude-reset-repo.sh" ''
  #!${stdenv.shell}
  rm -rf "$HOME/.emacs.d/.git"
  cd "$HOME/.emacs.d" &&
  ${git}/bin/git init &&
  ${git}/bin/git remote add origin https://github.com/bbatsov/prelude.git &&
  ${git}/bin/git fetch &&
  ${git}/bin/git reset --hard origin/master &&
  ${git}/bin/git branch -u origin/master
'';

ocelotSystemCfg = writeText "ocelot-system.el" ''
  (defvar ocelot-early-boot-background-color "${earlyBootBackgroundColor}")
  (defvar ocelot-early-boot-foreground-color "${earlyBootForegroundColor}")

  (defvar ocelot-locker-message "${toString lockerMessage}"
  "The informative message displayed by the screen locker.")

  (defvar ocelot-credentials-timeout ${toString credentialsTimeout}
  "How long the system says credentials should stay cached (in minutes).")

  (defvar ocelot-pinned-packages '(cl-generic exwm xelb ocelot-system ocelot)
  "A list of system-owned packages which shouldn't be overriden.")
  (defvar ocelot-frozen-packages ocelot-pinned-packages
  "A list of system-owned packages which shouldn't be updated.")
  (defvar ocelot-spacemacs-layer-path "${../distro/spacemacs-layers}/"
  "Where Spacemacs should look for system-owned configuration layers.")
  (defvar ocelot-local-elpa "${localElpa}"
  "A slimmed-down local package archive.")

  (defvar ocelot-software-versions (list
  ${versionsToPairs versions.system}
  ${versionsToPairs versions.emacs}
  ${versionsToPairs versions.application}
  ${versionsToPairs versions.package-management}
  ${versionsToPairs versions.base-system}
  ${versionsToPairs versions.kernel}
  ${versionsToPairs versions.platform})
  "Used by `ocelot-version'.")

  (defvar ocelot-spacemacs-repo-script "${spacemacsRepoScript}")
  (defvar ocelot-prelude-repo-script "${preludeRepoScript}")

  (defvar ocelot-workspace-plist
  '(
  ${workspacesList workspaces})
  "The system-defined plist mapping framebuffers to workspaces.")

  (declare-function ocelot "ocelot-startup.el")
  (defvar ocelot-inhibit-startup nil)

  (when (not noninteractive)
  (require 'ocelot-startup)
  (unless ocelot-inhibit-startup
  (ocelot)))

  (provide 'ocelot-system)
'';

installerCfg = callPackage ./ocelot-installer-config.nix {
  inherit spacemacs prelude globalDistribution epkgs;
};

in

callPackage ({ lib }: trivialBuild {
  pname = "ocelot-system";
  version = versions.system.Ocelot.version;
  src = ./ocelot-system;
  packageRequires = [ epkgs.exwm ];
  preBuild = ''
    cp ${ocelotSystemCfg} ocelot-system.el
    cp ${installerCfg} ocelot-installer-config.el
  '';
  meta = {
    license = lib.licenses.free;
  };
}) {}
