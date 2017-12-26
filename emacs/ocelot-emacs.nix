{ emacsPackagesNg, emacs,
  earlyBootBackgroundColor ? "#110024",
  earlyBootForegroundColor ? "#FFFFFF" }:

let
ocelotEmacs = emacsPackagesNg.overrideScope (super: self: {
  emacs = emacs.overrideAttrs (attrs: {
    postInstall = (attrs.postInstall or "") + ''
      rm -f $out/share/emacs/site-lisp/site-start.elc
      mv $out/share/emacs/site-lisp/site-start.el $out/share/emacs/site-lisp/nix-start.el
      cp ${./ocelot-defaults.el} $out/share/emacs/site-lisp/ocelot-defaults.el
      cp ${./ocelot-start.el} $out/share/emacs/site-lisp/ocelot-start.el
      cp ${./ocelot-evil-defaults.el} $out/share/emacs/site-lisp/ocelot-evil-defaults.el
      cat >"$out/share/emacs/site-lisp/site-start.el" <<EOF
      (setq ocelot-early-boot-background-color "${earlyBootBackgroundColor}")
      (setq ocelot-early-boot-foreground-color "${earlyBootForegroundColor}")

      (setq ocelot-pinned-packages '(cl-generic exwm xelb ocelot))
      (setq ocelot-frozen-packages ocelot-pinned-packages)
      (setq ocelot-spacemacs-layer-path "${./distro/spacemacs-layers}/")

      (load-file "$out/share/emacs/site-lisp/nix-start.el")
      (load-file "$out/share/emacs/site-lisp/ocelot-start.el")
      EOF
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/nix-start.el
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/ocelot-defaults.el
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/ocelot-start.el
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/site-start.el
    '';
  });
  elpaPinned = import ./epkgs/elpa-pinned.nix {
    inherit (self) callPackage;
  };
  ocelot = import ./epkgs/ocelot-epkg.nix {
    inherit (self) callPackage;
  };
});
in
  ocelotEmacs.emacsWithPackages (epkgs: [
  epkgs.elpaPinned.exwm
  epkgs.ocelot
])
