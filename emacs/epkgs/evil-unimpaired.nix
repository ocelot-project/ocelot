{ callPackage, spacemacs, dash, f }:

callPackage ({ elpaBuild, fetchurl, lib }: elpaBuild {
  pname = "evil-unimpaired";
  version = "0.1";
  src = "${spacemacs}/layers/+spacemacs/spacemacs-evil/local/evil-unimpaired/evil-unimpaired.el";
  packageRequires = [ dash f ];
  meta = {
    homepage = "https://github.com/syl20bnr/spacemacs";
    license = lib.licenses.free;
  };
}) {}
