# This package collects the early startup and system-dependent
# elisp used by Ocelot.
# TODO: set `ocelot-pinned-packages` using Nix
{ lib, callPackage, stdenv, git, writeText, writeScript, versioning,
  exwm, help-fns-plus, hide-comnt, info-plus, evil-unimpaired, highlight,
  globalDistribution, userDistributions,
  elpaPinned, orgPinned, melpaPinned, spacemacs, prelude,
  earlyBootBackgroundColor, earlyBootForegroundColor }:

with lib;

let
  versionsToPairs = attrs: concatStringsSep "\n" (
mapAttrsToList (name: value: "(cons '${name} \"${value.version}\")") attrs);

packageToPlist =
  epkg: ("(list 'name '${epkg.pname} 'path \"${epkg}\" 'dependency nil)");

dependencyToPlist =
  epkg: ("(list 'name '${epkg.pname} 'path \"${epkg}\" 'dependency t)");

spacemacsRepoScript = writeScript "spacemacs-reset-repo.sh" ''
  #!${stdenv.shell}
  rm -rf "$HOME/.emacs.d/.git"
  cd "$HOME/.emacs.d" &&
  ${git}/bin/git init &&
  ${git}/bin/git remote add origin https://github.com/syl20bnr/spacemacs &&
  ${git}/bin/git fetch &&
  ${git}/bin/git reset --hard origin/master &&
  ${git}/bin/git branch -u origin/master
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

  (defvar ocelot-pinned-packages '(cl-generic exwm xelb ocelot-system ocelot)
  "A list of system-owned packages which shouldn't be overriden.")
  (defvar ocelot-frozen-packages ocelot-pinned-packages
  "A list of system-owned packages which shouldn't be updated.")
  (defvar ocelot-spacemacs-layer-path "${../distro/spacemacs-layers}/"
  "Where Spacemacs should look for system-owned configuration layers.")

  (defvar ocelot-software-versions (list
  ${versionsToPairs versioning.system}
  ${versionsToPairs versioning.emacs}
  ${versionsToPairs versioning.application}
  ${versionsToPairs versioning.package-management}
  ${versionsToPairs versioning.base-system}
  ${versionsToPairs versioning.kernel}
  ${versionsToPairs versioning.platform})
  "Used by `ocelot-version'.")

  (defvar ocelot-spacemacs-repo-script "${spacemacsRepoScript}")
  (defvar ocelot-prelude-repo-script "${preludeRepoScript}")

  (when (not noninteractive)
  (require 'ocelot-startup))

  (provide 'ocelot-system)
'';

installerCfg = writeText "ocelot-installer-config.el" ''
  (defvar ocelot-installer-spacemacs-path "${spacemacs}")
  (defvar ocelot-installer-prelude-path "${prelude}")

  (defvar ocelot-global-distribution "${globalDistribution}")
  (defvar ocelot-user-distributions '())

  (defvar ocelot-installer-spacemacs-packages (list
  ${packageToPlist elpaPinned.cl-lib}
  ${packageToPlist melpaPinned.avy}
  ${packageToPlist melpaPinned.async}
  ${packageToPlist melpaPinned.helm-core}
  ${packageToPlist melpaPinned.popup}
  ${packageToPlist melpaPinned.helm}
  ${packageToPlist melpaPinned.ace-jump-helm-line}
  ${packageToPlist melpaPinned.ace-window}
  ${packageToPlist melpaPinned.packed}
  ${packageToPlist melpaPinned.auto-compile}
  ${packageToPlist melpaPinned.bind-key}
  ${packageToPlist melpaPinned.bind-map}
  ${packageToPlist melpaPinned.diminish}
  ${packageToPlist melpaPinned.elisp-slime-nav}
  ${packageToPlist melpaPinned.epl}
  ${packageToPlist melpaPinned.goto-chg}
  ${packageToPlist melpaPinned.undo-tree}
  ${packageToPlist melpaPinned.evil}
  ${packageToPlist melpaPinned.evil-escape}
  ${packageToPlist melpaPinned.evil-visualstar}
  ${packageToPlist melpaPinned.exec-path-from-shell}
  ${packageToPlist melpaPinned.flx}
  ${packageToPlist melpaPinned.helm-ag}
  ${packageToPlist melpaPinned.helm-descbinds}
  ${packageToPlist melpaPinned.helm-flx}
  ${packageToPlist melpaPinned.pkg-info}
  ${packageToPlist melpaPinned.projectile}
  ${packageToPlist melpaPinned.helm-make}
  ${packageToPlist melpaPinned.helm-mode-manager}
  ${packageToPlist melpaPinned.helm-projectile}
  ${packageToPlist melpaPinned.helm-swoop}
  ${packageToPlist melpaPinned.helm-themes}
  ${packageToPlist melpaPinned.hydra}
  ${packageToPlist melpaPinned.macrostep}
  ${packageToPlist melpaPinned.pcre2el}
  ${packageToPlist melpaPinned.undo-tree}
  ${packageToPlist melpaPinned.use-package}
  ${packageToPlist melpaPinned.which-key}
  ${packageToPlist help-fns-plus}
  ${packageToPlist melpaPinned.counsel}
  ${packageToPlist melpaPinned.counsel-projectile}
  ${packageToPlist melpaPinned.ivy}
  ${packageToPlist melpaPinned.ivy-hydra}
  ${packageToPlist melpaPinned.smex}
  ${packageToPlist melpaPinned.swiper}
  ${packageToPlist melpaPinned.wgrep}
  ${packageToPlist melpaPinned.ace-link}
  ${packageToPlist elpaPinned.adaptive-wrap}
  ${packageToPlist melpaPinned.aggressive-indent}
  ${packageToPlist melpaPinned.auto-highlight-symbol}
  ${packageToPlist melpaPinned.clean-aindent-mode}
  ${packageToPlist melpaPinned.column-enforce-mode}
  ${packageToPlist melpaPinned.define-word}
  ${packageToPlist melpaPinned.dumb-jump}
  ${packageToPlist highlight}
  ${packageToPlist melpaPinned.eval-sexp-fu}
  ${packageToPlist melpaPinned.evil-anzu}
  ${packageToPlist melpaPinned.evil-args}
  ${packageToPlist melpaPinned.evil-ediff}
  ${packageToPlist melpaPinned.evil-exchange}
  ${packageToPlist melpaPinned.evil-iedit-state}
  ${packageToPlist melpaPinned.evil-indent-plus}
  ${packageToPlist melpaPinned.evil-lisp-state}
  ${packageToPlist melpaPinned.evil-matchit}
  ${packageToPlist melpaPinned.evil-mc}
  ${packageToPlist melpaPinned.evil-nerd-commenter}
  ${packageToPlist melpaPinned.evil-numbers}
  ${packageToPlist melpaPinned.evil-search-highlight-persist}
  ${packageToPlist melpaPinned.evil-surround}
  ${packageToPlist melpaPinned.evil-tutor}
  ${packageToPlist evil-unimpaired}
  ${packageToPlist melpaPinned.evil-visual-mark-mode}
  ${packageToPlist melpaPinned.expand-region}
  ${packageToPlist melpaPinned.eyebrowse}
  ${packageToPlist melpaPinned.fancy-battery}
  ${packageToPlist melpaPinned.fill-column-indicator}
  ${packageToPlist melpaPinned.flx-ido}
  ${packageToPlist melpaPinned.golden-ratio}
  ${packageToPlist melpaPinned.google-translate}
  ${packageToPlist hide-comnt}
  ${packageToPlist melpaPinned.highlight-indentation}
  ${packageToPlist melpaPinned.highlight-numbers}
  ${packageToPlist melpaPinned.highlight-parentheses}
  ${packageToPlist melpaPinned.hl-todo}
  ${packageToPlist melpaPinned.hungry-delete}
  ${packageToPlist melpaPinned.indent-guide}
  ${packageToPlist info-plus}
  ${packageToPlist melpaPinned.link-hint}
  ${packageToPlist melpaPinned.linum-relative}
  ${packageToPlist melpaPinned.lorem-ipsum}
  ${packageToPlist melpaPinned.move-text}
  ${packageToPlist melpaPinned.neotree}
  ${packageToPlist melpaPinned.open-junk-file}
  ${packageToPlist melpaPinned.org-bullets}
  ${packageToPlist orgPinned.org-plus-contrib}
  ${packageToPlist melpaPinned.paradox}
  ${packageToPlist melpaPinned.persp-mode}
  ${packageToPlist melpaPinned.popwin}
  ${packageToPlist melpaPinned.rainbow-delimiters}
  ${packageToPlist melpaPinned.request}
  ${packageToPlist melpaPinned.restart-emacs}
  ${packageToPlist melpaPinned.smartparens}
  ${packageToPlist melpaPinned.spaceline}
  ${packageToPlist melpaPinned.toc-org}
  ${packageToPlist melpaPinned.uuidgen}
  ${packageToPlist melpaPinned.vi-tilde-fringe}
  ${packageToPlist melpaPinned.volatile-highlights}
  ${packageToPlist melpaPinned.winum}
  ${packageToPlist melpaPinned.ws-butler}
  ${packageToPlist melpaPinned.dash}
  ${packageToPlist melpaPinned.powerline}
  ${packageToPlist melpaPinned.s}
  ${packageToPlist elpaPinned.spinner}
  ${packageToPlist melpaPinned.parent-mode}
  ${packageToPlist melpaPinned.iedit}
  ${packageToPlist melpaPinned.anzu}
  ${packageToPlist melpaPinned.f}))

  (defvar ocelot-installer-prelude-packages (list
  ${packageToPlist melpaPinned.ace-window}
  ${packageToPlist melpaPinned.anzu}
  ${packageToPlist melpaPinned.beacon}
  ${packageToPlist melpaPinned.browse-kill-ring}
  ${packageToPlist melpaPinned.crux}
  ${packageToPlist melpaPinned.dash}
  ${packageToPlist melpaPinned.diff-hl}
  ${packageToPlist melpaPinned.diminish}
  ${packageToPlist melpaPinned.discover-my-major}
  ${packageToPlist melpaPinned.easy-kill}
  ${packageToPlist melpaPinned.editorconfig}
  ${packageToPlist melpaPinned.epl}
  ${packageToPlist melpaPinned.expand-region}
  ${packageToPlist melpaPinned.flycheck}
  ${packageToPlist melpaPinned.gist}
  ${packageToPlist melpaPinned.git-timemachine}
  ${packageToPlist melpaPinned.gitconfig-mode}
  ${packageToPlist melpaPinned.gitignore-mode}
  ${packageToPlist melpaPinned.god-mode}
  ${packageToPlist melpaPinned.grizzl}
  ${packageToPlist melpaPinned.guru-mode}
  ${packageToPlist melpaPinned.imenu-anywhere}
  ${packageToPlist melpaPinned.magit}
  ${packageToPlist melpaPinned.move-text}
  ${packageToPlist melpaPinned.operate-on-number}
  ${packageToPlist melpaPinned.ov}
  ${packageToPlist melpaPinned.projectile}
  ${packageToPlist melpaPinned.smart-mode-line}
  ${packageToPlist melpaPinned.smartparens}
  ${packageToPlist melpaPinned.smartrep}
  ${packageToPlist melpaPinned.undo-tree}
  ${packageToPlist melpaPinned.volatile-highlights}
  ${packageToPlist melpaPinned.which-key}
  ${packageToPlist melpaPinned.zenburn-theme}
  ${packageToPlist melpaPinned.zop-to-char}
  ${dependencyToPlist melpaPinned.async}
  ${dependencyToPlist melpaPinned.avy}
  ${dependencyToPlist melpaPinned.gh}
  ${dependencyToPlist melpaPinned.ghub}
  ${dependencyToPlist melpaPinned.git-commit}
  ${dependencyToPlist melpaPinned.ht}
  ${dependencyToPlist elpaPinned.let-alist}
  ${dependencyToPlist melpaPinned.logito}
  ${dependencyToPlist melpaPinned.magit-popup}
  ${dependencyToPlist melpaPinned.makey}
  ${dependencyToPlist melpaPinned.marshal}
  ${dependencyToPlist melpaPinned.pcache}
  ${dependencyToPlist melpaPinned.pkg-info}
  ${dependencyToPlist melpaPinned.rich-minority}
  ${dependencyToPlist melpaPinned.s}
  ${dependencyToPlist elpaPinned.seq}
  ${dependencyToPlist melpaPinned.with-editor}))

  (provide 'ocelot-installer-config)
  '';
in

callPackage ({ melpaBuild, lib }: melpaBuild {
  pname = "ocelot-system";
  version = versioning.system.Ocelot.version;
  src = ./ocelot-system;
  packageRequires = [ exwm ];
  preBuild = ''
    cp ${ocelotSystemCfg} ocelot-system.el
    cp ${installerCfg} ocelot-installer-config.el
  '';
  meta = {
    license = lib.licenses.free;
  };
}) {}
