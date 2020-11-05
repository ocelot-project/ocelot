{ versions, globalDistribution, userDistributions,
  earlyBootBackgroundColor, earlyBootForegroundColor, credentialsTimeout,
  workspaces, lockerMessage }:

self: super:
let
  elispPrelude = ''
      rm -f $out/share/emacs/site-lisp/site-start.elc
      mv $out/share/emacs/site-lisp/site-start.el $out/share/emacs/site-lisp/nix-start.el
      cat >"$out/share/emacs/site-lisp/site-start.el" <<EOF
      (load-file "$out/share/emacs/site-lisp/nix-start.el")
      (require 'ocelot-system 'nil 'noerror)
      EOF
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/nix-start.el
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/site-start.el
    '';
  emacsPkg = super.emacsUnstable.overrideAttrs (attrs: {
    postInstall = (attrs.postInstall or "") + elispPrelude;
  });
  emacsJitPkg = super.emacsGcc.overrideAttrs (attrs: {
    postInstall = (attrs.postInstall or "") + elispPrelude;
  });

in
{
  ocelotEmacs = (
    (
      (super.emacsPackagesGen emacsPkg).emacsWithPackages (epkgs: [
        epkgs.exwm
        (super.callPackage ./epkgs/ocelot-system.nix {
          inherit (self.emacsPackages) melpaBuild;
          inherit epkgs versions globalDistribution userDistributions;
          inherit earlyBootBackgroundColor earlyBootForegroundColor;
          inherit credentialsTimeout workspaces lockerMessage;
          spacemacs = super.callPackage ./distro/spacemacs.nix {};
          prelude = super.callPackage ./distro/prelude.nix {};
        })
        (super.callPackage ./epkgs/ocelot-epkg.nix {
          inherit (self.emacsPackages) melpaBuild;
        })
      ])
    )
  );
  ocelotEmacsJit = (
    (
      (super.emacsPackagesGen emacsJitPkg).emacsWithPackages (epkgs: [
        epkgs.exwm
        (super.callPackage ./epkgs/ocelot-system.nix {
          inherit (self.emacsPackages) melpaBuild;
          inherit epkgs versions globalDistribution userDistributions;
          inherit earlyBootBackgroundColor earlyBootForegroundColor;
          inherit credentialsTimeout workspaces lockerMessage;
          spacemacs = super.callPackage ./distro/spacemacs.nix {};
          prelude = super.callPackage ./distro/prelude.nix {};
        })
        (super.callPackage ./epkgs/ocelot-epkg.nix {
          inherit (self.emacsPackages) melpaBuild;
        })
      ])
    )
  );
}
