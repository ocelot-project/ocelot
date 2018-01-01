{ lib, emacsPackagesNg, emacs, writeText,
  versioning ? { system.Ocelot.version = "0.1"; },
  earlyBootBackgroundColor ? "#110024",
  earlyBootForegroundColor ? "#FFFFFF" }:

let
ocelotEmacs = emacsPackagesNg.overrideScope (super: self: {
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
  ocelot-system = import ./epkgs/ocelot-system.nix {
    inherit lib;
    inherit (self) callPackage;
    inherit writeText;
    inherit versioning;
    inherit earlyBootBackgroundColor;
    inherit earlyBootForegroundColor;
    exwm = self.elpaPinned.exwm;
  };
  ocelot = import ./epkgs/ocelot-epkg.nix {
    inherit (self) callPackage;
  };
});
in
  ocelotEmacs.emacsWithPackages (epkgs: [
  epkgs.ocelot-system
  epkgs.ocelot
])
