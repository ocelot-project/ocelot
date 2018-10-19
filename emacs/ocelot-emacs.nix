{ lib, emacsPackagesNg, emacs, stdenv, git,
  writeText, writeScript, fetchFromGitHub,
  versioning ? { system.Ocelot.version = "0.1"; },
  globalDistribution, userDistributions ? {},
  earlyBootBackgroundColor ? "#110024",
  earlyBootForegroundColor ? "#FFFFFF",
  credentialsTimeout ? 5,
  xrandrHeads ? [],
  lockerMessage}:

let
ocelotEmacs = emacsPackagesNg.overrideScope' (self: super: {
  emacs = emacs.overrideAttrs (attrs: {
    postInstall = (attrs.postInstall or "") + ''
      rm -f $out/share/emacs/site-lisp/site-start.elc
      mv $out/share/emacs/site-lisp/site-start.el $out/share/emacs/site-lisp/nix-start.el
      cat >"$out/share/emacs/site-lisp/site-start.el" <<EOF
      (load-file "$out/share/emacs/site-lisp/nix-start.el")
      (require 'ocelot-system 'nil 'noerror)
      EOF
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/nix-start.el
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/site-start.el
    '';
  });
  elpaPinned = import ./epkgs/elpa-pinned.nix {
    inherit (self) callPackage;
  };
  orgPinned = import ./epkgs/org-pinned.nix {
    inherit (self) callPackage;
  };
  melpaPinned = import ./epkgs/melpa-pinned.nix {
    inherit (self) callPackage;
  };
  # highlight = import ./epkgs/highlight.nix {
  #   inherit (self) callPackage;
  # };
  ocelot-system = import ./epkgs/ocelot-system.nix {
    inherit lib;
    inherit (self) callPackage;
    inherit stdenv;
    inherit git;
    inherit writeText;
    inherit writeScript;
    inherit versioning;
    inherit earlyBootBackgroundColor;
    inherit earlyBootForegroundColor;
    inherit credentialsTimeout;
    inherit xrandrHeads;
    inherit lockerMessage;
    inherit globalDistribution;
    inherit userDistributions;
    inherit (self) elpaPinned;
    inherit (self) orgPinned;
    inherit (self) melpaPinned;
    inherit (self) highlight;
    exwm = self.elpaPinned.exwm;
    evil-unimpaired = import ./epkgs/evil-unimpaired.nix {
      inherit (self) callPackage;
      spacemacs = import ./distro/spacemacs.nix {
        inherit fetchFromGitHub;
      };
      dash = self.melpaPinned.dash;
      f = self.melpaPinned.f;
    };
    spacemacs = import ./distro/spacemacs.nix {
      inherit fetchFromGitHub;
    };
    prelude = import ./distro/prelude.nix {
      inherit fetchFromGitHub;
    };
  };
  ocelot = import ./epkgs/ocelot-epkg.nix {
    inherit writeText;
    inherit (self) callPackage;
  };
});
in
  ocelotEmacs.emacsWithPackages (epkgs: [
  epkgs.ocelot-system
  epkgs.ocelot
])
